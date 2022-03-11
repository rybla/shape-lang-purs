module Language.Shape.Stlc.Changes where

import Prelude
import Prim hiding (Type)

import Control.Monad.State (State, get, put, runState)
import Data.Either (Either)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.List (List(..), fold, foldr, singleton, zip, zipWith, (:))
import Data.Map (Map, insert, lookup, mapMaybeWithKey, toUnfoldable, union)
import Data.Map as Map
import Data.Map.Unsafe (lookup')
import Data.Maybe (Maybe(..))
import Data.Set (Set(..), difference, empty, filter, member)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd)
import Language.Shape.Stlc.Holes (HoleSub, subTerm, subType, unifyType)
import Language.Shape.Stlc.Metadata (defaultArgConsMetaData, defaultArrowTypeMetadata, defaultBlockMetadata, defaultDataTypeMetadata, defaultHoleTermMetadata, defaultHoleTypeMetadata, defaultLambdaTermMetadata, defaultParameterMetadata, defaultTermBindingMetadata, defaultTermDefinitionMetadata)
import Language.Shape.Stlc.Syntax (Args(..), Block(..), Case(..), Constructor(..), Definition(..), HoleId(..), Parameter(..), Term(..), TermBinding(..), TermId(..), Type(..), TypeBinding(..), TypeId(..), freshHoleId, freshTermId)
import Language.Shape.Stlc.Typing (Context)
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
    -- | Replace Type
    | Dig HoleId
-- Note for the future: could e.g. make Swap take a typechange which says what happens to rest of type after swap. Currently, it is implicitly NoChange.

data VarChange = VariableTypeChange TypeChange | VariableDeletion

data ParamsChange = DeleteParam Int | InsertParam Int Parameter | ChangeParam Int TypeChange | ParamsNoChange
data ConstructorChange = ConstructorNoChange | InsertConstructor Int Constructor | DeleteConstructor Int | ChangeConstructor ParamsChange

type Changes = {
    termChanges :: Map TermId VarChange,
    matchChanges :: Map TypeId (List ConstructorChange),
    dataTypeDeletions :: KindChanges
}

emptyChanges :: Changes
emptyChanges = {
    termChanges : Map.empty,
    matchChanges : Map.empty,
    dataTypeDeletions : empty
}

deleteVar :: Changes -> TermId -> Changes
deleteVar {termChanges, matchChanges, dataTypeDeletions} i
    = {termChanges : insert i VariableDeletion termChanges, matchChanges, dataTypeDeletions}

varChange :: Changes -> TermId -> TypeChange -> Changes
varChange {termChanges, matchChanges, dataTypeDeletions} i ch
    = {termChanges : insert i (VariableTypeChange ch) termChanges, matchChanges, dataTypeDeletions}

type KindChanges = Set TypeId -- set of datatypes which have been deleted

applyTC :: TypeChange -> Type -> Type
applyTC (ArrowCh c1 c2) (ArrowType (Parameter a md1) b md2)
    = ArrowType (Parameter (applyTC c1 a) md1) (applyTC c2 b) md2
applyTC NoChange t = t
applyTC (InsertArg a) t = ArrowType (Parameter a defaultParameterMetadata) t defaultArrowTypeMetadata
applyTC Swap (ArrowType a (ArrowType b c md1) md2) = ArrowType b (ArrowType a c md1) md2
applyTC RemoveArg (ArrowType a b _) = b
applyTC (Dig id) t = HoleType (freshHoleId unit) empty defaultHoleTypeMetadata
applyTC _ _ = error "Shouldn't get ehre"

-- TODO: consider just outputting TypeChange, and then use chType : Type -> TypeChange -> Type to actually get the Type.
chType :: KindChanges -> Type -> Tuple Type TypeChange
chType chs (ArrowType (Parameter a pmd) b md)
    = let (Tuple a' ca) = chType chs a in
      let (Tuple b' cb) = chType chs b
      in Tuple (ArrowType (Parameter a' pmd) b' md) (ArrowCh ca cb)
chType chs (HoleType i w md)
    = Tuple (HoleType i (difference w chs) md) NoChange -- remove deleted datatypes from weakening -- TODO: is this how difference works?
chType chs (DataType i md) = if member i chs
    then let id = (freshHoleId unit)
        in Tuple (HoleType id empty defaultHoleTypeMetadata) (Dig id)
    else Tuple (DataType i md) NoChange
chType chs (ProxyHoleType i) = Tuple (ProxyHoleType i) NoChange

indexOf :: TermBinding -> TermId
indexOf (TermBinding i _) = i

-- (Map.insert id beta gamma)
cons :: TermBinding -> Type -> Context -> Context
cons (TermBinding i _) t ctx = insert i t ctx

-- subContradict :: HoleSub -> HoleSub -> Boolean
-- subContradict sub1 sub2 = mapMaybeWithKey

-- If subs conflict, give error. Else, combine them.
-- This function is terrible
combineSubs :: HoleSub -> HoleSub -> HoleSub
combineSubs original new =
    foldrWithIndex combineSub original new
    where combineSub :: HoleId -> Type -> HoleSub -> HoleSub
          combineSub id ty acc
            = let ty' = subType acc ty in
              case lookup id acc of
                Nothing -> insert id ty' acc
                Just tyAcc -> case unifyType ty' tyAcc of
                    Nothing -> error "this breaks my assumption that unifications in chTerm should never result in ambiguous situations"
                    Just secondarySubs -> combineSubs (insert id (subType secondarySubs ty') acc) secondarySubs

chDefinition :: Context -> Changes -> Definition -> State (Tuple (List Definition) HoleSub) Definition
-- for data definitions, do nothing?
-- for term definitions, change both the type and term.
chDefinition ctx chs (TermDefinition binding ty t md)
    = let (Tuple ty' change) = chType chs.dataTypeDeletions ty -- TODO IMPORTANT DON'T FORGET: the changes to each definition type need to be found in block, and then applied to all definitions! This is because all definitions in a block can refer to each other.
      in let chs' = varChange chs (indexOf binding) change
      in do
        t' <- chTerm ctx ty' chs' change t
        pure $ TermDefinition binding ty' t' md
chDefinition ctx chs (DataDefinition binding constrs md) -- TODO: make sure that types of constructors end up in the context of the rest of the block.
    = undefined

liiift :: forall a b c. State a c -> State (Tuple b a) c
liiift s = do (Tuple b a) <- get
              let (Tuple c a') = runState s a
              put (Tuple b a')
              pure c

-- morally, the type input here should not have metadata. But we can just go with it anyway.
chTerm :: Context -> Type -> Changes -> TypeChange -> Term -> State (Tuple (List Definition) HoleSub) Term
chTerm ctx ty chs (Dig _) t = pure $ HoleTerm defaultHoleTermMetadata
chTerm ctx (ArrowType (Parameter a _) b _) chs (ArrowCh c1 c2) (LambdaTerm binding block md)
    = do let (Tuple _ change) = chType chs.dataTypeDeletions a
         block' <- liiift $ chBlock (insert binding a ctx) b (varChange chs binding change) c2 block
         pure $ LambdaTerm binding block' md
chTerm ctx (ArrowType (Parameter a _) b _) chs NoChange (LambdaTerm index block md)
    = do let (Tuple a' change) = chType chs.dataTypeDeletions a
         block' <- liiift $ chBlock (insert index a' ctx) b (varChange chs index change) NoChange block
         pure $ LambdaTerm index block' md
chTerm ctx ty chs (InsertArg a) t =
    do t' <- (chTerm (insert newBinding a ctx) (ArrowType (Parameter a defaultParameterMetadata) ty defaultArrowTypeMetadata) chs NoChange t)
       pure $ LambdaTerm newBinding (Block Nil t' defaultBlockMetadata) defaultLambdaTermMetadata
    where newBinding = (freshTermId unit)
chTerm ctx (ArrowType (Parameter a _) (ArrowType (Parameter b _) c _) _) chs Swap (LambdaTerm i1 (Block defs (LambdaTerm i2 (Block defs2 t md4) md1) md2) md3) =
    do let (Tuple a' change1) = (chType chs.dataTypeDeletions a)
       let (Tuple b' change2) = (chType chs.dataTypeDeletions b)
       let ctx' = (insert i2 b' (insert i1 a' ctx))
       let chs' = varChange (varChange chs i1 change1) i2 change2
       block <- liiift $ chBlock ctx' c chs' NoChange (Block (defs <> defs2) t md4)
       pure $ LambdaTerm i2 (Block Nil (LambdaTerm i1 block md3) md2) md1
chTerm ctx (ArrowType a b _ ) chs RemoveArg (LambdaTerm i (Block defs t md) _) =
      do displacedDefs <- sequence $ map (chDefinition ctx (deleteVar chs i)) defs
         displaceDefs displacedDefs
         chTerm ctx b (deleteVar chs i) NoChange t
chTerm ctx ty chs ch (NeutralTerm id args md) =
    case lookup id chs.termChanges of
        Just VariableDeletion -> ifChanged NoChange
        Just (VariableTypeChange varTC) -> ifChanged varTC
        Nothing -> ifChanged NoChange
    where
    ifChanged varTC = do
        (Tuple args' ch') <- chArgs ctx ty chs varTC args
        let maybeSub = unifyType (applyTC ch ty) (applyTC ch' ty)
        case maybeSub of
            Just holeSub -> do subHoles holeSub
                               pure $ NeutralTerm id args' md
            Nothing -> do displaceDefs (singleton (TermDefinition (TermBinding (freshTermId unit) defaultTermBindingMetadata) (applyTC ch' ty)
                                                        (NeutralTerm id args' md) defaultTermDefinitionMetadata))
                          pure $ HoleTerm defaultHoleTermMetadata
chTerm ctx ty chs ch (HoleTerm md) = pure $ HoleTerm md
chTerm ctx ty chs ch (MatchTerm i t cases md) = do -- TODO, IMPORTANT: Needs to deal with constructors being changed/added/removed and datatypes being deleted.
    cases' <- sequence $ (map (chCase ctx ty chs ParamsNoChange ch) cases)
    t' <- (chTerm ctx (DataType i defaultDataTypeMetadata) chs ch t)
    pure $ MatchTerm i t' cases' md
-- TODO: does this last case ever actually happen?
chTerm ctx ty chs _ t -- anything that doesn't fit a pattern just goes into a hole
    = let (Tuple ty' change) = chType chs.dataTypeDeletions ty in
    do
    t' <- chTerm ctx ty chs change t -- is passing in ty correct? the type input to chTerm is the type of the term that is inputted?
    _ <- displaceDefs $ singleton (TermDefinition (TermBinding (freshTermId unit) defaultTermBindingMetadata) ty' t' defaultTermDefinitionMetadata)
    pure $ HoleTerm defaultHoleTermMetadata

displaceDefs :: (List Definition) -> State (Tuple (List Definition) HoleSub) Unit
displaceDefs defs = do
    Tuple currDisplaced holeSub <- get
    put $ Tuple (currDisplaced <> defs) holeSub

subHoles :: HoleSub -> State (Tuple (List Definition) HoleSub) Unit
subHoles sub = do
    Tuple currDisplaced holeSub <- get
    put $ Tuple currDisplaced (combineSubs holeSub sub)


-- data ParamsChange = DeleteParam Int | InsertParam Int Parameter | ChangeParam Int TypeChange | ParamsNoChange
-- The Type is type of term in the case excluding bindings
chCase :: Context -> Type -> Changes -> ParamsChange -> TypeChange -> Case -> State (Tuple (List Definition) HoleSub) Case
chCase ctx ty chs paramsChange innerTC (Case bindings t md) = case paramsChange of
    ParamsNoChange -> do
        t' <- (chTerm ctx ty chs innerTC t)
        pure $ Case bindings t' md
    DeleteParam i -> undefined
    InsertParam i p -> undefined
    ChangeParam i tc -> undefined


isNoChange :: TypeChange -> Boolean
isNoChange (ArrowCh c1 c2) = isNoChange c1 && isNoChange c2
isNoChange NoChange = true
isNoChange _ = false

-- State (Tuple (List Definition) HoleSub) Term
-- morally, shouldn't have the (List Definition) in the state of chBlock, as it always outputs the empty list.
chBlock :: Context -> Type -> Changes -> TypeChange -> Block -> State HoleSub Block
chBlock ctx ty chs ch (Block defs t md)
    = undefined
    -- = let (Tuple t' displaced1) = runState (chTerm undefined ty chs ch t) undefined -- was Nil
    --   in let (Tuple defs' displaced2) = runState (sequence (map (chDefinition ctx chs) defs)) undefined--waas Nil
    --   in Block (defs' <> displaced1 <> displaced2) t' md -- TODO: maybe consider positioning the displaced terms better rather than all at the end. This is fine for now though.

-- the Type is type of function which would input these args.
-- the TypeChange is the change of the function which inputs these arguments
chArgs :: Context -> Type -> Changes -> TypeChange -> Args -> State (Tuple (List Definition) HoleSub) (Tuple Args TypeChange)
chArgs ctx (ArrowType (Parameter a _) b _) chs RemoveArg (ConsArgs arg args md) = do
    (Tuple args' chOut) <- chArgs ctx b chs NoChange args
    pure $ Tuple args' chOut -- note chOut should always be NoChange
chArgs ctx a chs (InsertArg t) args = do
    (Tuple rest chOut) <- chArgs ctx a chs NoChange args
    pure $ Tuple (ConsArgs (HoleTerm defaultHoleTermMetadata) rest defaultArgConsMetaData) chOut -- always chOut = noChange
chArgs ctx (ArrowType (Parameter a _) b _) chs (ArrowCh c1 c2) (ConsArgs arg args md) = do
    arg' <- chTerm ctx a chs c1 arg
    (Tuple args' tc) <- chArgs ctx b chs c2 args
    pure $ Tuple (ConsArgs arg' args' md) tc
chArgs ctx (ArrowType (Parameter a _) (ArrowType (Parameter b _) c _) _) chs Swap (ConsArgs arg1 (ConsArgs arg2 args md1) md2) = do
    arg1' <- chTerm ctx a chs NoChange arg1
    arg2' <- chTerm ctx b chs NoChange arg2
    (Tuple rest chOut) <- chArgs ctx c chs NoChange args
    pure $ Tuple (ConsArgs arg2' (ConsArgs arg1' rest md1) md2) chOut -- chOut = NoChange alwyas
chArgs ctx a chs NoChange NoneArgs = pure $ Tuple NoneArgs NoChange
chArgs ctx (ArrowType (Parameter a _) b _) chs NoChange (ConsArgs arg args md) = do
    arg' <- chTerm ctx a chs NoChange arg
    (Tuple args' outCh) <- chArgs ctx b chs NoChange args
    pure $ Tuple (ConsArgs arg' args' md) outCh -- outCh = nochange
chArgs ctx ty chs (Dig hId) args = do
    displaceArgs ty args
    pure $ Tuple NoneArgs (Dig hId)
chArgs _ _ _ _ _ = error "shouldn't get here"

displaceArgs :: Type -> Args -> State (Tuple (List Definition) HoleSub) Unit
displaceArgs _ NoneArgs = pure unit
displaceArgs (ArrowType (Parameter a _) b _) (ConsArgs arg args _) = do
    displaceDefs $ singleton $ TermDefinition (TermBinding (freshTermId unit)  defaultTermBindingMetadata)
        a arg defaultTermDefinitionMetadata
    displaceArgs b args
displaceArgs _ _ = error "no"

argsToTermList :: Args -> List Term
argsToTermList NoneArgs = Nil
argsToTermList (ConsArgs arg args _) = arg : (argsToTermList args)


-- TODO: for wrap stuff, remember that it needs to be possible to have f : A -> B -> C, and in a buffer,
-- have f a, and change it to f a b. Also, to change f a b to f a. In other words, make sure that
-- when propagating changes upwards in wrap, this stuff all works.

-- here