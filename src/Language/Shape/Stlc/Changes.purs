module Language.Changes where

import Prelude
import Prim hiding (Type)

import Control.Monad.State (State, get, put, runState)
import Data.List (List(..), union, singleton)
import Data.Map (Map, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Set (Set(..), difference, empty, filter, member)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd)
import Language.Shape.Stlc.Metadata (defaultArrowTypeMetadata, defaultBlockMetadata, defaultHoleTermMetadata, defaultHoleTypeMetadata, defaultLambdaTermMetadata, defaultTermBindingMetadata, defaultTermDefinitionMetadata)
import Language.Shape.Stlc.Syntax (Block(..), Constructor(..), Definition(..), NeutralTerm, Term(..), TermBinding(..), TermID(..), Type(..), TypeBinding(..), TypeID(..), freshHoleID, freshTermID)
import Language.Shape.Stlc.Typing (Context, inferNeutral, inferTerm)
import Pipes.Prelude (mapM)
import Undefined (undefined)
import Unsafe (error)

data TypeChange
    = ArrowCh TypeChange TypeChange -- only applies to types of form (ArrowType a b _)
    | NoChange
    | InsertArg Type
    | Swap -- only applies to types of form (ArrowType a (ArrowType b c _) _)
    | RemoveArg -- only applies to types of form (ArrowType a b _)
    | Replace Type
-- Note for the future: could e.g. make Swap take a typechange which says what happens to rest of type after swap. Currently, it is implicitly NoChange.

data VarChange = VariableTypeChange TypeChange | VariableDeletion

-- DataChange is used where data type is used in type OR a value of it is matched upon
data ConstructorChange = ChangeC TypeChange | InsertConstructor Constructor

type Changes = {
    termChanges :: Map TermID VarChange,
    matchChanges :: Map TypeID (List ConstructorChange),
    dataTypeDeletions :: Set TypeID
}

deleteVar :: Changes -> TermID -> Changes
deleteVar {termChanges, matchChanges, dataTypeDeletions} i
    = {termChanges : insert i VariableDeletion termChanges, matchChanges, dataTypeDeletions}

type KindChanges = Set TypeID -- set of datatypes which have been deleted

chType :: KindChanges -> TypeChange -> Type -> Type
chType chs (ArrowCh c1 c2) (ArrowType a b md) = ArrowType (chType chs c1 a) (chType chs c2 b) md
chType chs (InsertArg a) t = ArrowType a (chType chs NoChange t) defaultArrowTypeMetadata
chType chs Swap (ArrowType a (ArrowType b c md1) md2)
    = ArrowType (continue a) (ArrowType (continue b) (continue c) md1) md2
      where continue = chType chs NoChange
chType chs RemoveArg (ArrowType a b md) = chType chs NoChange b
chType chs (Replace a) t = a
chType chs NoChange (ArrowType a b md) = ArrowType (chType chs NoChange a) (chType chs NoChange a) md
chType chs NoChange (HoleType i w md) = HoleType i (difference w chs) md -- remove deleted datatypes from weakening -- TODO: is this how difference works?
chType chs NoChange (DataType i md) = if member i chs
    then HoleType (freshHoleID unit) empty defaultHoleTypeMetadata
    else DataType i md
chType chs _ _ = error "shouldn't get here"

-- (Map.insert id beta gamma)
cons :: TermBinding -> Type -> Context -> Context
cons (TermBinding i _) t ctx = insert i t ctx

-- TODO: is there a monad which is like State (List T) but only has put, and is for building a list of elements?
chTerm :: Context -> Changes -> TypeChange -> Term -> State (List Definition) Term
chTerm ctx chs (Replace a) t = pure $ HoleTerm a defaultHoleTermMetadata
chTerm ctx chs (ArrowCh c1 c2) (LambdaTerm i t b md)
    = pure $ LambdaTerm i t' (chBlock (cons i t' ctx) chs c2 b) md
             where t' = (chType chs.dataTypeDeletions c1 t) -- TODO, VERY IMPORTANT DONT FORGET: when I make chType return a TypeChange, then that needs to be added to chs in line above.
chTerm ctx chs NoChange (LambdaTerm i t b md)
    = pure $ LambdaTerm i t' (chBlock (cons i t' ctx) chs NoChange b) md
        where t' = (chType chs.dataTypeDeletions NoChange t)
chTerm ctx chs (InsertArg a) t =
    do t' <- (chTerm (cons newBinding a ctx) chs NoChange t)
       pure $ LambdaTerm newBinding a (Block Nil t' defaultBlockMetadata) defaultLambdaTermMetadata
    where newBinding = (TermBinding (freshTermID unit) defaultTermBindingMetadata)
chTerm ctx chs Swap (LambdaTerm i1 a (Block defs (LambdaTerm i2 b (Block defs2 t md4) md1) md2) md3)
    = pure $ LambdaTerm i2 b'
        (Block Nil (LambdaTerm i1 a'
            (chBlock (cons i2 b' (cons i1 a' ctx)) chs NoChange (Block (defs <> defs2) t md4)) md3) md2) md1
      where b' = (chType chs.dataTypeDeletions NoChange b)
            a' = (chType chs.dataTypeDeletions NoChange a)
chTerm ctx chs RemoveArg (LambdaTerm (TermBinding i _) a (Block defs t md) _)
    = do currStuff <- get
         displacedDefs <- sequence $ map (chDefinition ctx (deleteVar chs i)) defs
         _ <- put (currStuff <> displacedDefs) -- obviously this should be able to be nicer than "newList = oldList + newStuff", but for now...
         chTerm ctx (deleteVar chs i) NoChange t
chTerm ctx chs ch (NeutralTerm t md) = do
    t' <- chNeutral ctx chs ch t
    case t' of
        Just ne -> pure (NeutralTerm ne md)
        Nothing -> pure $ HoleTerm (inferNeutral ctx t) defaultHoleTermMetadata
chTerm ctx chs ch (HoleTerm t md) = pure $ HoleTerm (chType chs.dataTypeDeletions ch t) md -- TODO, DON'T FORGET: when chType returns a TypeChange, use it somehow.
chTerm ctx chs ch (MatchTerm i t cases md) = do
    cases' <- sequence $ (map (chTerm ctx chs ch) cases)
    t' <- (chTerm ctx chs ch t)
    pure $ MatchTerm i t' cases' md
chTerm ctx chs _ t = do
    t' <- chTerm ctx chs NoChange t
    currStuff <- get
    _ <- put $ currStuff <> (singleton (TermDefinition (TermBinding (freshTermID unit) defaultTermBindingMetadata) t' defaultTermDefinitionMetadata))
    pure $ HoleTerm (inferTerm ctx t) defaultHoleTermMetadata -- anything that doesn't fit a pattern just goes into a hole

chNeutral :: Context -> Changes -> TypeChange -> NeutralTerm -> State (List Definition) (Maybe NeutralTerm)
chNeutral = undefined

chBlock :: Context -> Changes -> TypeChange -> Block -> Block
chBlock ctx chs ch (Block defs t md)
    -- = Block (map (chDefinition chs) defs) (chTerm chs ch t) md
    = let (Tuple displaced1 t) = runState (chTerm undefined chs ch t) Nil
      in let (Tuple displaced2 defs) = runState (sequence (map (chDefinition ctx chs) defs)) Nil
      in undefined

chDefinition :: Context -> Changes -> Definition -> State (List Definition) Definition
-- for data definitions, do nothing?
-- for term definitions, change both the type and term.
chDefinition chs = undefined

defFromTerm :: Term -> Definition
defFromTerm t = TermDefinition undefined t defaultTermDefinitionMetadata


-- TODO: for wrap stuff, remember that it needs to be possible to have f : A -> B -> C, and in a buffer,
-- have f a, and change it to f a b. Also, to change f a b to f a. In other words, make sure that
-- when propagating changes upwards in wrap, this stuff all works.


-- TODO: chNeutral and chType should output a TypeChange.
-- chType needs it because if I delete a datatype, then the type of each definition may have some corresponding changes which arise.
-- QUESTION: if TermDefinitions don't literally have a type anymore, how should this work?

-- QUESTION: What happens if I chTerm (ArrowChange (Replace HoleType) NoChange) (lam x . x 5)

-- chTerm should not?, because there are only two places where we have terms: in a definition or on the r.h.s. of an application.
-- In neither case are we willing to change it's type as a result of a different definition changing.
-- except, that the type of a derinition SHOULD change if a datatype is deleted.
-- So actually, chTerm SHOULD return a TypeChange?

-- PROBLEM: consider
-- let f : A -> A
-- in  f = lam x . (neutral x)

-- But, then I change the input type to B.
-- SHould it automatically change output to B as well? No.
-- It should end up with:
-- let f : B -> A
-- in  f = lam x . ?

-- QUESTION: Does just making everything return a TypeChange, and then using the outputs correctly, make everything work as I want?
-- QUESTION: What is the relationship between the TypeChange passed INTO chTerm and the out passed out????
-- INstead of passing one out, should it just compute additional changes (from DataType deletions) going into the input TypeChange?

{-

To summarize the problems that I am finding: it doesn't seem right that you could pass in a TypeChange
to a type or term to be changed, but then discover other things which needed to be changed about it and return
a different TypeChange, which is presumably similar to the one that was passed in but maybe with some extra deletions.

I'm not sure what the resolution is. Maybe Insertions/Deletions are different than deleting a datatype?
But I need to think it through pretty carefully with a lot of cases in mind.

Maybe the TC going in is TC : TypeChange T1 T2, and then the one going out is TypeChange T2 T3.
In other words, it changes it even further.

No, probably it should return TypeChange T1 T3.

-}