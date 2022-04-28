module Language.Shape.Stlc.Changes where

import Data.Tuple.Nested
import Language.Shape.Stlc.Metadata
import Prelude
import Prim hiding (Type)

import Control.Monad.State (State)
import Data.Default (default)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Map (Map, insert)
import Data.Map as Map
import Data.Set (Set, difference, member)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Language.Shape.Stlc.Context (Context(..), insertVarType)
import Language.Shape.Stlc.Hole (HoleEq)
import Language.Shape.Stlc.Recursor.Context (ProtoArgsTerm, ArgsTerm)
import Language.Shape.Stlc.Recursor.Context as Rec
import Language.Shape.Stlc.Syntax (HoleId(..), Term(..), TermId(..), Type(..), TypeId(..), freshHoleId, freshTermId)
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
chTerm ctx (ArrowType {dom, cod, meta:m1}) chs (ArrowCh c1 c2) (Lam {termBind, body, meta:m2})
    = do let (_ /\ change) = chType chs.dataTypeDeletions dom
        -- TODO TODO TODO TODO TODO TODO: this is the point where I need to figure out when to use
        -- change vs when to use c1!!!!! I think this shouldn't come up in practice though.
        -- Also, this applies to all the other cases with ArrowType.
         body' <- chTerm (insertVarType termBind.termId dom ctx) cod (varChange chs termBind.termId c1) c2 body
         pure $ Lam {termBind, body: body', meta: m2}
chTerm ctx (ArrowType {dom, cod, meta:m1}) chs NoChange (Lam {termBind, body, meta:m2})
    = do let (_ /\ change) = chType chs.dataTypeDeletions dom
         body' <- chTerm (insertVarType termBind.termId dom ctx) cod (varChange chs termBind.termId change) NoChange body
         pure $ Lam {termBind, body: body', meta:m2}
chTerm ctx ty chs (InsertArg a) t
    = do t' <- chTerm ctx ty chs NoChange t
         pure $ Lam {termBind: {termId: freshTermId unit, meta: default}, body: t, meta: default}
chTerm ctx (ArrowType {dom: a, cod: ArrowType {dom: b, cod: c}}) chs Swap
    (Lam {termBind: i1, body: Lam {termBind: i2, body: t, meta: m2}, meta:m1})
    = do let (a' /\ change1) = chType chs.dataTypeDeletions a
             (b' /\ change2) = chType chs.dataTypeDeletions b
             ctx' = insertVarType i2.termId b' (insertVarType i1.termId a' ctx)
             chs' = varChange (varChange chs i1.termId change1) i2.termId change2
         body' <- chTerm ctx' c chs' NoChange t
         pure $ Lam {termBind: i2, body: Lam {termBind: i1, body: body', meta: m2}, meta: m1}
chTerm ctx (ArrowType {dom, cod, meta:m1}) chs RemoveArg (Lam {termBind, body, meta:m2})
    = chTerm (insertVarType termBind.termId dom ctx) cod (deleteVar chs termBind.termId) NoChange body
chTerm ctx ty chs tc t
    = chTermAux { argsCtx :{ ctx , type_ : ty} , argsSyn :{term: t}} chs tc

chTermAux :: Rec.ProtoRec Rec.ArgsTerm () (Changes -> TypeChange -> State HoleEq Term)
chTermAux args chs sbjto
    = let ctx = args.argsCtx.ctx in
      let ty = args.argsCtx.type_ in
     Rec.recTerm {
    lam : error "shouldn't get here"
    , neu : undefined -- depends on chargs
    , let_ : \args chs sbjto -> do
        let (ty' /\ tc) = chType chs.dataTypeDeletions args.argsSyn.let_.type_
        term' <- chTerm ctx ty chs tc args.argsSyn.let_.term
        body' <- chTerm ctx ty (varChange chs args.argsSyn.let_.termBind.termId tc) sbjto args.argsSyn.let_.body
        pure $ Let $ args.argsSyn.let_ {term = term', body = body'}
    , buf : \args chs sbjto -> do
        let (ty' /\ tc) = chType chs.dataTypeDeletions args.argsSyn.buf.type_
        term' <- chTerm ctx ty chs sbjto args.argsSyn.buf.term
        body' <- chTerm ctx ty chs tc args.argsSyn.buf.body
        pure $ Buf $ args.argsSyn.buf {term = term', body = body'}
    , data_ : \args chs sbjto -> do
        let sumItems' = chSum args chs
        -- TODO: TODO: TODO::: chSum needs to return potentially changes which get added to chs.
        body' <- chTerm ctx ty chs sbjto args.argsSyn.data_.body
        pure $ Data $ args.argsSyn.data_ {sumItems= sumItems', body = body'}
    , match : \args chs sbjto -> do
        -- TODO: TODO: apply data type changes to the match cases
        term' <- chTerm ctx ty chs sbjto args.argsSyn.match.term
        pure $ Match $ args.argsSyn.match {term = term'}
        
    , hole : \args chs sbjto -> pure $ Hole args.argsSyn.hole
} args chs sbjto

-- chArgs :: 
-- Need to wait for henry to make args recursor

chSum = undefined