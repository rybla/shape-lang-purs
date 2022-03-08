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
import Prim.Boolean (True)
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

-- TODO: consider just outputting TypeChange, and then use chType : Type -> TypeChange -> Type to actually get the Type.
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
chTerm ctx (ArrowType a b _) chs (ArrowCh c1 c2) (LambdaTerm binding block md)
    = pure $ LambdaTerm binding
        (chBlock (insert (indexOf binding) a ctx) b (varChange chs (indexOf binding) change) c2 block) md
             where (Tuple _ change) = (chType chs.dataTypeDeletions a)
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
chTerm ctx ty chs ch (HoleTerm md) = pure $ HoleTerm md
chTerm ctx ty chs ch (MatchTerm i t cases md) = do -- TODO: This is wrong. In order to deal with this correctly, I need to either make Case (not just use Term), or need to know types of constructors.
    cases' <- sequence $ (map (chTerm ctx ty chs ch) cases)
    t' <- (chTerm ctx (DataType i defaultDataTypeMetadata) chs ch t)
    pure $ MatchTerm i t' cases' md
chTerm ctx ty chs _ t -- anything that doesn't fit a pattern just goes into a hole
    = let (Tuple ty' change) = chType chs.dataTypeDeletions ty in
    do
    t' <- chTerm ctx ty chs change t -- is passing in ty correct? the type input to chTerm is the type of the term that is inputted?
    currStuff <- get
    _ <- put $ currStuff <> (singleton (TermDefinition (TermBinding (freshTermID unit) defaultTermBindingMetadata) ty' t' defaultTermDefinitionMetadata))
    pure $ HoleTerm defaultHoleTermMetadata

isNoChange :: TypeChange -> Boolean
isNoChange (ArrowCh c1 c2) = isNoChange c1 && isNoChange c2
isNoChange NoChange = true
isNoChange _ = false

-- TODO: make this not input a TypeChange, and instead output one. Don't output Maybe, instead chTerm determines what to
-- do with the resulting neutral form from the TypeChange using isNoChange.
chNeutral :: Context -> Changes -> TypeChange -> NeutralTerm -> State (List Definition) (Maybe NeutralTerm)
chNeutral = undefined

chBlock :: Context -> Type -> Changes -> TypeChange -> Block -> Block
chBlock ctx ty chs ch (Block defs t md)
    = let (Tuple t' displaced1) = runState (chTerm undefined ty chs ch t) Nil
      in let (Tuple defs' displaced2) = runState (sequence (map (chDefinition ctx chs) defs)) Nil
      in Block (defs' <> displaced1 <> displaced2) t' md -- TODO: maybe consider positioning the displaced terms better rather than all at the end. This is fine for now though.

chDefinition :: Context -> Changes -> Definition -> State (List Definition) Definition
-- for data definitions, do nothing?
-- for term definitions, change both the type and term.
chDefinition ctx chs (TermDefinition binding ty t md)
    = let (Tuple ty' change) = chType chs.dataTypeDeletions ty -- TODO IMPORTANT DON'T FORGET: the changes to each definition type need to be found in block, and then applied to all definitions! This is because all definitions in a block can refer to each other.
      in let chs' = varChange chs (indexOf binding) change
      in do
        t' <- chTerm ctx ty' chs' change t
        pure $ TermDefinition binding ty' t' md
chDefinition ctx chs (DataDefintion binding constrs md) -- TODO: make sure that types of constructors end up in the context of the rest of the block.
    = undefined

defFromTerm :: Term -> Definition
defFromTerm t = TermDefinition undefined undefined t defaultTermDefinitionMetadata


-- TODO: for wrap stuff, remember that it needs to be possible to have f : A -> B -> C, and in a buffer,
-- have f a, and change it to f a b. Also, to change f a b to f a. In other words, make sure that
-- when propagating changes upwards in wrap, this stuff all works.