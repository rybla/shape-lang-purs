module Language.Shape.Stlc.Changes where

import Data.Tuple.Nested
import Language.Shape.Stlc.Metadata
import Prelude
import Prim hiding (Type)

import Control.Monad.State (State, get, put)
import Data.Default (default)
import Data.Generic.Rep (class Generic)
import Data.Int.Bits ((.&.))
import Data.List (List(..), (:), concat)
import Data.Map (Map, insert, lookup)
import Data.Map as Map
import Data.Map.Unsafe (lookup')
import Data.Maybe (Maybe(..))
import Data.Set (Set, difference, member)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Language.Shape.Stlc.Context (Context(..), insertVarType, lookupVarType)
import Language.Shape.Stlc.Hole (HoleEq, unifyTypeRestricted)
import Language.Shape.Stlc.Index (IxDown(..), IxUp(..))
import Language.Shape.Stlc.Recursor.Context as Rec
import Language.Shape.Stlc.Syntax (ArgItem, ArrowType, HoleId(..), Term(..), TermId(..), Type(..), TypeId(..), CaseItem, freshHoleId, freshTermId)
import Undefined (undefined)
import Unsafe (error)

data TypeChange
    = ArrowCh TypeChange TypeChange -- only applies to types of form (ArrowType a b _)
    | NoChange
    | InsertArg Type
    | Swap -- only applies to types of form (ArrowType a (ArrowType b c _) _)
    | RemoveArg -- only applies to types of form (ArrowType a b _)
    -- | Replace Type -- can't allow Replace, because it would break the invariant that holesubs collected from chTerm can be applied at the end and never conflict with each other.
    | Dig HoleId
-- Note for the future: could e.g. make Swap take a typechange which says what happens to rest of type after swap. Currently, it is implicitly NoChange.

derive instance Generic TypeChange _
instance Show TypeChange where show x = genericShow x

data VarChange = VariableTypeChange TypeChange | VariableDeletion
data ConstructorChange = ChangeConstructor -- TODO: write this!

type KindChanges = Set TypeId -- set of datatypes which have been deleted

type Changes = {
    termChanges :: Map TermId VarChange,
    matchChanges :: Map TypeId (List ConstructorChange),
    dataTypeDeletions :: KindChanges
}

emptyChanges :: Changes
emptyChanges = {
    termChanges : Map.empty,
    matchChanges : Map.empty,
    dataTypeDeletions : Set.empty
}

deleteVar :: Changes -> TermId -> Changes
deleteVar {termChanges, matchChanges, dataTypeDeletions} i
    = {termChanges : insert i VariableDeletion termChanges, matchChanges, dataTypeDeletions}

varChange :: Changes -> TermId -> TypeChange -> Changes
varChange {termChanges, matchChanges, dataTypeDeletions} i ch
    = {termChanges : insert i (VariableTypeChange ch) termChanges, matchChanges, dataTypeDeletions}

applyTC :: TypeChange -> Type -> Type
applyTC (ArrowCh c1 c2) (ArrowType {dom, cod, meta})
    = ArrowType {dom: (applyTC c1 dom), cod: (applyTC c2 cod), meta}
applyTC NoChange t = t
applyTC (InsertArg a) t = ArrowType {dom: a, cod: t, meta:default}
applyTC Swap (ArrowType {dom: a, cod: (ArrowType {dom: b, cod:c, meta: md1}), meta: md2})
    = ArrowType {dom: b, cod: (ArrowType {dom: a, cod: c, meta: md1}), meta: md2}
applyTC RemoveArg (ArrowType {cod: b}) = b
applyTC (Dig id) t = HoleType {holeId: (freshHoleId unit), weakening: Set.empty, meta: default}
applyTC tc ty = error $ "Shouldn't get ehre. tc is: " <> show tc <> " ty is: " <> show ty

-- this is like infer
chType :: KindChanges -> Type -> Type /\ TypeChange
chType chs (ArrowType {dom, cod, meta})
    = let (dom' /\ ca) = chType chs dom in
      let (cod' /\ cb) = chType chs cod
      in (ArrowType {dom:dom', cod:cod', meta}) /\ (ArrowCh ca cb)
chType chs (HoleType {holeId, weakening, meta})
    = (HoleType {holeId, weakening: (difference weakening chs), meta}) /\ NoChange -- remove deleted datatypes from weakening -- TODO: is this how difference works?
chType chs (DataType {typeId, meta}) = if member typeId chs
    then let holeId = (freshHoleId unit)
        in (HoleType {holeId, weakening: Set.empty, meta: default}) /\ (Dig holeId)
    else (DataType {typeId, meta}) /\ NoChange

combineSubs :: HoleEq -> HoleEq -> HoleEq
combineSubs _ _ = Map.empty -- TODO: fix this later!!!

{-Ideally, chTerm would be written with the Context recursor. However, some of the cases are easier to
write with advanced pattern matching rather than merely simple recursion. Therefore, I write those cases
manually, and then use the Context recursor for the remaining cases in chTermAux.-}
chTerm :: Context -> Type -> Changes -> TypeChange -> Term -> State HoleEq Term
chTerm gamma (ArrowType {dom, cod, meta:m1}) chs (ArrowCh c1 c2) (Lam {termBind, body, meta:m2})
    = do let (_ /\ change) = chType chs.dataTypeDeletions dom
        -- TODO TODO TODO TODO TODO TODO: this is the point where I need to figure out when to use
        -- change vs when to use c1!!!!! I think this shouldn't come up in practice though.
        -- Also, this applies to all the other cases with ArrowType.
         body' <- chTerm (insertVarType termBind.termId dom gamma) cod (varChange chs termBind.termId c1) c2 body
         pure $ Lam {termBind, body: body', meta: m2}
chTerm gamma (ArrowType {dom, cod, meta:m1}) chs NoChange (Lam {termBind, body, meta:m2})
    = do let (_ /\ change) = chType chs.dataTypeDeletions dom
         body' <- chTerm (insertVarType termBind.termId dom gamma) cod (varChange chs termBind.termId change) NoChange body
         pure $ Lam {termBind, body: body', meta:m2}
chTerm gamma ty chs (InsertArg a) t
    = do t' <- chTerm gamma ty chs NoChange t
         pure $ Lam {termBind:{termId: freshTermId unit, meta: default}, body: t, meta: default}
chTerm gamma (ArrowType {dom: a, cod: ArrowType {dom: b, cod: c}}) chs Swap
    (Lam {termBind:i1, body: Lam {termBind:i2, body: t, meta: m2}, meta:m1})
    = do let (a' /\ change1) = chType chs.dataTypeDeletions a
             (b' /\ change2) = chType chs.dataTypeDeletions b
             gamma' = insertVarType i2.termId b' (insertVarType i1.termId a' gamma)
             chs' = varChange (varChange chs i1.termId change1) i2.termId change2
         body' <- chTerm gamma' c chs' NoChange t
         pure $ Lam {termBind:i2, body: Lam {termBind:i1, body: body', meta: m2}, meta: m1}
chTerm gamma (ArrowType {dom, cod, meta:m1}) chs RemoveArg (Lam {termBind, body, meta:m2})
    = chTerm (insertVarType termBind.termId dom gamma) cod (deleteVar chs termBind.termId) NoChange body
chTerm gamma ty chs tc t 
    = chTermAux {alpha: ty, gamma: gamma, term: t} chs tc

chTerm' :: Record (Rec.ArgsTerm ()) -> Changes -> TypeChange -> State HoleEq Term 
chTerm' args chs tc = chTerm args.gamma args.alpha chs tc args.term

chTermAux :: Record (Rec.ArgsTerm ()) -> (Changes -> TypeChange -> State HoleEq Term)
chTermAux args chs sbjto = Rec.recTerm {
    lam : error "shouldn't get here"
    , neu : \args chs sbjto ->
        let varType = (lookupVarType args.neu.termId args.gamma) in
        let ifChanged varTC = do
                (argItems' /\ tc /\ displaced1) <- chArgs args.gamma varType chs varTC args.neu.argItems
                let maybeSub = unifyTypeRestricted (applyTC sbjto args.alpha) (applyTC tc args.alpha)
                case maybeSub of
                    Just holeSub -> do subHoles holeSub
                                       pure $ wrapInDisplaced displaced1 (Neu $ args.neu{argItems = argItems'})
                    Nothing -> do displaced2 <- displaceArgs args.gamma varType chs args.neu.argItems
                                  pure $ wrapInDisplaced (displaced1 <> displaced2) (Hole {meta: default})
        in
        case lookup args.neu.termId chs.termChanges of
            Just VariableDeletion -> do displaced <- displaceArgs args.gamma varType chs args.neu.argItems
                                        pure $ wrapInDisplaced displaced (Hole {meta: default})
            Just (VariableTypeChange varTC) -> ifChanged varTC
            Nothing -> ifChanged NoChange
    , let_ : \args chs sbjto -> do
        let (ty' /\ tc) = chType chs.dataTypeDeletions args.let_.sign
        impl' <- chTerm' args.impl chs tc 
        body' <- chTerm' args.body (varChange chs args.let_.termBind.termId tc) sbjto
        pure $ Let $ args.let_ {impl = impl', body = body'}
    , buf : \args chs sbjto -> do
        (impl' /\ changedBy) <- inferChTerm args.impl chs
        -- TODO: are datatypedeletions and inference of buffer in correct order? Does this always work?
        let type' = applyTC changedBy args.buf.sign
        let (type'' /\ tc) = chType chs.dataTypeDeletions type'
        body' <- chTerm' args.body chs sbjto
        pure $ Buf $ args.buf {impl = impl', body = body', sign = type''}
    , data_ : \args chs sbjto -> do
        let sumItems' = chSum args chs
        -- TODO: TODO: TODO::: chSum needs to return potentially changes which get added to chs.
        body' <- chTerm' args.body chs sbjto
        pure $ Data $ args.data_ {sumItems= sumItems', body = body'}
    , match : \args chs sbjto -> do
        -- TODO: this doesn't do any reordering of cases in the match.
        caseItems' <- sequence (map (\cas -> chCaseItem cas chs sbjto) args.caseItems)
        term' <- chTerm' args.term chs NoChange
        pure $ Match $ args.match {term = term', caseItems = caseItems'}
    , hole : \args chs sbjto -> pure $ Hole args.hole
} args chs sbjto

wrapInDisplaced :: Displaced -> Term -> Term
wrapInDisplaced Nil term = term
wrapInDisplaced ((t /\ ty) : displaced) term
    = Buf {
            body: (wrapInDisplaced displaced term)
            , meta: default
            , sign : ty
            , impl : t
          }

subHoles :: HoleEq -> State HoleEq Unit
subHoles sub = do
    holeEq <- get
    put $ combineSubs holeEq sub

-- chCaseItems :: Record (Rec.ArgsCaseItems ()) -> Changes -> TypeChange -> State HoleEq (List CaseItem)
-- chCaseItems args chs sbjto = map concat <<< sequence <<< Rec.recArgsCaseItems {
--     caseItem : ?h
-- } args

chCaseItem :: Record (Rec.ArgsCaseItem ()) -> Changes -> TypeChange -> State HoleEq CaseItem
chCaseItem = Rec.recCaseItem {
    caseItem : \args chs sbjto -> do
        -- TODO: completely missing changes to datatypes
        body' <- chTerm' args.body chs sbjto
        pure $ args.caseItem{body = body'}
}

type Displaced = List (Term /\ Type)
chArgs :: Context -> Type -> Changes -> TypeChange -> List ArgItem -> State HoleEq (List ArgItem /\ TypeChange /\ Displaced)
chArgs ctx (ArrowType {dom, cod}) chs RemoveArg (Cons {term} args) = do
    (args' /\ chOut /\ displacements) <- chArgs ctx cod chs NoChange args
    term' <- chTerm ctx dom chs NoChange term  -- TODO: this assumes that no dataTypeDeltions exist in the Changes.
    pure $ args' /\ chOut /\ ((term' /\ dom) : displacements)
chArgs ctx a chs (InsertArg t) args = do
    (rest /\ chOut /\ displacements) <- chArgs ctx a chs NoChange args
    pure $ (({meta: default, term: Hole {meta: default}} : rest) /\ chOut /\ displacements)
chArgs ctx (ArrowType {dom, cod}) chs (ArrowCh c1 c2) ({term, meta} : args) = do
    arg' <- chTerm ctx dom chs c1 term
    (args' /\ tc /\ displacements) <- chArgs ctx dom chs c2 args
    pure $ ({term: arg', meta} : args') /\ tc /\ displacements
chArgs ctx (ArrowType {dom: a, cod: ArrowType {dom: b, cod: c}}) chs Swap
    ({term: arg1, meta: md1} : {term: arg2, meta: md2} : args) = do
    arg1' <- chTerm ctx a chs NoChange arg1
    arg2' <- chTerm ctx b chs NoChange arg2
    (rest /\ chOut /\ displacements) <- chArgs ctx c chs NoChange args
    pure $ ({term: arg2', meta: md1} : {term: arg1', meta: md2} : rest) /\ chOut /\ displacements
chArgs ctx a chs ch Nil = pure $ Nil /\ ch /\ Nil
chArgs ctx (ArrowType {dom, cod}) chs NoChange ({term, meta} : args) = do
    arg' <- chTerm ctx dom chs NoChange term
    args' /\ outCh /\ displacements <- chArgs ctx dom chs NoChange args
    pure $ ({term: arg', meta} : args') /\ outCh /\ displacements -- of course, always outCh = NoChange and displacements = Nil
chArgs ctx ty chs (Dig hId) args = do
    displaced <- displaceArgs ctx ty chs args
    pure $ Nil /\ (Dig hId) /\ displaced
chArgs _ _ _ _ _ = error $ "shouldn't get here"

displaceArgs :: Context -> Type -> Changes -> List ArgItem -> State HoleEq Displaced
displaceArgs ctx _ chs Nil = pure Nil
displaceArgs ctx (ArrowType {dom, cod}) chs ({term} : args) = do
    term' <- chTerm ctx dom chs NoChange term
    rest <- displaceArgs ctx cod chs args
    pure $ (term' /\ dom) : rest
displaceArgs _ _ _ _ = error "shouldn't get here"

-- chArgsAux ::  Record (Rec.ArgsArgItems ()) -> (Changes -> TypeChange -> State HoleEq (List ArgItem /\ Displaced))
-- chArgsAux = Rec.recArgsArgItems {
--     argItem : \args chs sbjto -> ?h
-- }
-- Need to wait for henry to make args recursor


chSum = undefined

inferChTerm :: Record (Rec.ArgsTerm ()) -> (Changes -> State HoleEq (Term /\ TypeChange))
inferChTerm = Rec.recTerm {
    lam : \args chs -> do
        let (alpha' /\ tcIn) = chType chs.dataTypeDeletions args.alpha
        (body' /\ tcOut) <- inferChTerm args.body (varChange chs args.termBind.termBind.termId tcIn)
        pure $ (Lam args.lam{body=body'} /\ ArrowCh tcIn tcOut)
    , neu : \args chs ->
        let varType = (lookupVarType args.neu.termId args.gamma) in
        let ifChanged varTC = do
                (argItems' /\ tc /\ displaced1) <- chArgs args.gamma varType chs varTC args.neu.argItems
                pure $ wrapInDisplaced displaced1 (Neu $ args.neu{argItems = argItems'}) /\ tc
        in
        case lookup args.neu.termId chs.termChanges of
            Just VariableDeletion -> do displaced <- displaceArgs args.gamma varType chs args.neu.argItems
                                        pure $ wrapInDisplaced displaced (Hole {meta: default}) /\ NoChange -- TODO: should this be NoChange?
            Just (VariableTypeChange varTC) -> ifChanged varTC
            Nothing -> ifChanged NoChange
    , let_ : \args chs -> do
        let (ty' /\ tc) = chType chs.dataTypeDeletions args.let_.sign
        impl' <- chTerm' args.impl chs tc 
        body' /\ bodyTc <- inferChTerm args.body (varChange chs args.let_.termBind.termId tc)
        pure $ (Let (args.let_ {impl = impl', body = body'}) /\ bodyTc)
    , buf : \args chs -> do
        (impl' /\ changedBy) <- inferChTerm args.impl chs
        -- TODO: are datatypedeletions and inference of buffer in correct order? Does this always work?
        let type' = applyTC changedBy args.buf.sign
        let (type'' /\ tc) = chType chs.dataTypeDeletions type'
        -- body' <- chTerm' args.body chs sbjto
        body' /\ bodyTc <- inferChTerm args.body chs
        pure $ Buf (args.buf {impl = impl', body = body', sign = type''}) /\ bodyTc
    , data_ : \args chs -> do
        let sumItems' = chSum args chs
        -- TODO: TODO: TODO::: chSum needs to return potentially changes which get added to chs.
        -- body' <- chTerm' args.body chs sbjto
        body' /\ bodyTc <- inferChTerm args.body chs
        pure $ Data (args.data_ {sumItems= sumItems', body = body'}) /\ bodyTc
    , match : \args chs -> do
        -- TODO: this doesn't do any reordering of cases in the match.
        caseItems' <- sequence (map (\cas -> chCaseItem cas chs NoChange) args.caseItems) -- TODO: what do we really want here?
        term' <- chTerm' args.term chs NoChange
        pure $ Match (args.match {term = term', caseItems = caseItems'}) /\ NoChange
    , hole : \args chs -> pure $ Hole args.hole /\ NoChange -- TODO: is NoChange correct here?
}

{-

The problem, so I don't forget again:
In the program:

let f : A -> A -> A
    f = ...

buf f 
...

Changing the type of f shouldn't displace f from in it's buffer below.
The type of the buffer should merely update to reflect the change in it's value.




Proposed plan to fix the problem:
Have a function called inferChTerm, which doesn't input a TypeChange but instead outputs one
(in addition to the term that it outputs).

Questions:
- does regular chTerm still need to input Changes? Maybe only inferChTerm inputs Changes.
- can chAtIndex be implemented using inferChTerm?
- can/should we replace NeutralTerm with App and Var? In any case, it is probably easy to
    do so now.

Plan:
- Implement inferChTerm with index recursor. Make case for when index is here, and have that return the Syntax input.
- chType is like inferChType, and also needs the thing with the index?
- Implement chTerm with gamma recursor
- If inferChTerm calls chTerm and then index isn't Nothing, then that's an error?
-}