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
import Language.Shape.Stlc.Metadata (defaultArrowTypeMetadata, defaultBlockMetadata, defaultDataTypeMetadata, defaultHoleTermMetadata, defaultHoleTypeMetadata, defaultLambdaTermMetadata, defaultTermBindingMetadata, defaultTermDefinitionMetadata)
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
    dataTypeDeletions :: KindChanges
}

deleteVar :: Changes -> TermID -> Changes
deleteVar {termChanges, matchChanges, dataTypeDeletions} i
    = {termChanges : insert i VariableDeletion termChanges, matchChanges, dataTypeDeletions}

varChange :: Changes -> TermID -> TypeChange -> Changes
varChange {termChanges, matchChanges, dataTypeDeletions} i ch
    = {termChanges : insert i (VariableTypeChange ch) termChanges, matchChanges, dataTypeDeletions}

type KindChanges = Set TypeID -- set of datatypes which have been deleted

chType :: KindChanges -> Type -> Tuple Type TypeChange
chType chs (ArrowType a b md)
    = let (Tuple a' ca) = chType chs a in
      let (Tuple b' cb) = chType chs b
      in Tuple (ArrowType a' b' md) (ArrowCh ca cb)
chType chs (HoleType i w md)
    = Tuple (HoleType i (difference w chs) md) NoChange -- remove deleted datatypes from weakening -- TODO: is this how difference works?
chType chs (DataType i md) = if member i chs
    then let h = HoleType (freshHoleID unit) empty defaultHoleTypeMetadata
        in Tuple h (Replace h)
    else Tuple (DataType i md) NoChange

indexOf :: TermBinding -> TermID
indexOf (TermBinding i _) = i

-- (Map.insert id beta gamma)
cons :: TermBinding -> Type -> Context -> Context
cons (TermBinding i _) t ctx = insert i t ctx

-- morally, the type input here should not have metadata. But we can just go with it anyway.
chTerm :: Context -> Type -> Changes -> TypeChange -> Term -> State (List Definition) Term
chTerm ctx ty chs (Replace a) t = pure $ HoleTerm defaultHoleTermMetadata
chTerm ctx (ArrowType a b _) chs (ArrowCh c1 c2) (LambdaTerm bind@(TermBinding index _) block md)
    = pure $ LambdaTerm bind (chBlock (insert index a ctx) b (varChange chs index change) c2 block) md
             where (Tuple _ change) = (chType chs.dataTypeDeletions a) -- TODO, VERY IMPORTANT DONT FORGET: when I make chType return a TypeChange, then that needs to be added to chs in line above.
chTerm ctx (ArrowType a b _) chs NoChange (LambdaTerm i@(TermBinding index _) block md)
    = pure $ LambdaTerm i (chBlock (cons i a' ctx) b (varChange chs index change) NoChange block) md
        where (Tuple a' change) = (chType chs.dataTypeDeletions a)
chTerm ctx ty chs (InsertArg a) t =
    do t' <- (chTerm (cons newBinding a ctx) (ArrowType a ty defaultArrowTypeMetadata) chs NoChange t)
       pure $ LambdaTerm newBinding (Block Nil t' defaultBlockMetadata) defaultLambdaTermMetadata
    where newBinding = (TermBinding (freshTermID unit) defaultTermBindingMetadata)
chTerm ctx (ArrowType a (ArrowType b c _) _) chs Swap (LambdaTerm i1 (Block defs (LambdaTerm i2 (Block defs2 t md4) md1) md2) md3)
    = pure $ LambdaTerm i2
        (Block Nil (LambdaTerm i1
            (chBlock ctx' c chs' NoChange (Block (defs <> defs2) t md4)) md3) md2) md1
      where (Tuple a' change1) = (chType chs.dataTypeDeletions a)
            (Tuple b' change2) = (chType chs.dataTypeDeletions b)
            ctx' = (cons i2 b' (cons i1 a' ctx))
            chs' = varChange (varChange chs (indexOf i1) change1) (indexOf i2) change2
chTerm ctx (ArrowType a b _ ) chs RemoveArg (LambdaTerm (TermBinding i _) (Block defs t md) _)
    = do currStuff <- get
         displacedDefs <- sequence $ map (chDefinition ctx (deleteVar chs i)) defs
         _ <- put (currStuff <> displacedDefs) -- obviously this should be able to be nicer than "newList = oldList + newStuff", but for now...
         chTerm ctx b (deleteVar chs i) NoChange t
chTerm ctx ty chs ch (NeutralTerm t md) = do
    t' <- chNeutral ctx chs ch t
    case t' of
        Just ne -> pure (NeutralTerm ne md)
        Nothing -> pure $ HoleTerm defaultHoleTermMetadata
chTerm ctx ty chs ch (HoleTerm md) = pure $ HoleTerm md -- TODO, DON'T FORGET: when chType returns a TypeChange, use it somehow.
chTerm ctx ty chs ch (MatchTerm i t cases md) = do
    cases' <- sequence $ (map (chTerm ctx ty chs ch) cases)
    t' <- (chTerm ctx (DataType i defaultDataTypeMetadata) chs ch t)
    pure $ MatchTerm i t' cases' md
chTerm ctx ty chs _ t = do -- anything that doesn't fit a pattern just goes into a hole
    t' <- chTerm ctx ty chs NoChange t
    currStuff <- get
    _ <- put $ currStuff <> (singleton (TermDefinition (TermBinding (freshTermID unit) defaultTermBindingMetadata) ty t' defaultTermDefinitionMetadata))
    pure $ HoleTerm defaultHoleTermMetadata

chNeutral :: Context -> Changes -> TypeChange -> NeutralTerm -> State (List Definition) (Maybe NeutralTerm)
chNeutral = undefined

chBlock :: Context -> Type -> Changes -> TypeChange -> Block -> Block
chBlock ctx ty chs ch (Block defs t md)
    -- = Block (map (chDefinition chs) defs) (chTerm chs ch t) md
    = let (Tuple displaced1 t) = runState (chTerm undefined ty chs ch t) Nil
      in let (Tuple displaced2 defs) = runState (sequence (map (chDefinition ctx chs) defs)) Nil
      in undefined

chDefinition :: Context -> Changes -> Definition -> State (List Definition) Definition
-- for data definitions, do nothing?
-- for term definitions, change both the type and term.
chDefinition chs = undefined

defFromTerm :: Term -> Definition
defFromTerm t = TermDefinition undefined undefined t defaultTermDefinitionMetadata


-- TODO: for wrap stuff, remember that it needs to be possible to have f : A -> B -> C, and in a buffer,
-- have f a, and change it to f a b. Also, to change f a b to f a. In other words, make sure that
-- when propagating changes upwards in wrap, this stuff all works.

-- QUESTION: Does just making everything return a TypeChange, and then using the outputs correctly, make everything work as I want?
-- QUESTION: What is the relationship between the TypeChange passed INTO chTerm and the out passed out????
-- INstead of passing one out, should it just compute additional changes (from DataType deletions) going into the input TypeChange?

-- QUESTION: in version with annotations on lets, does chType ever INPUT a TypeChange?
    -- I thought it would be used to compute a new type if I e.g. add an argument to a declaration. But,
    -- now we are handling that with wrap.
-- Also, is there any reason for chNeutral to input a TypeChange?

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

{-

Here is the solution to my problems:
1) Put annotations back on let and NOT in lambda and hole.
2) Thread ctx and type theory ch_ function (annoying, but have to)
3) chTerm inputs a TypeChange but doesn't output one.
4) chType and chNeutral DONT input a TypeChange, but DO output one.
5) chBlock and chDefinition, I'll have to think about it.

-}