module Language.Shape.Stlc.Changes where

import Data.Tuple.Nested
import Language.Shape.Stlc.Metadata
import Prelude
import Prim hiding (Type)

import Control.Monad.State (State)
import Data.Default (default)
import Data.Generic.Rep (class Generic)
import Data.Int.Bits ((.&.))
import Data.List (List(..))
import Data.Map (Map, insert)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set, difference, member)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Language.Shape.Stlc.Context (Context(..), insertVarType)
import Language.Shape.Stlc.Hole (HoleEq)
import Language.Shape.Stlc.Index (IxDown(..), IxUp(..))
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
    -- { gamma :{ gamma , type_ : ty} , syn :{term: t}} chs tc
    = chTermAux undefined chs tc

chTerm' :: Record (Rec.ArgsTerm ()) -> Changes -> TypeChange -> State HoleEq Term 
chTerm' args chs tc = chTerm args.gamma args.alpha chs tc args.term

chTermAux :: Record (Rec.ArgsTerm ()) -> (Changes -> TypeChange -> State HoleEq Term)
chTermAux args chs sbjto = Rec.recTerm {
    lam : error "shouldn't get here"
    , neu : undefined -- depends on chargs
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
        -- TODO: TODO: apply data type changes to the match cases
        term' <- chTerm' args.term chs sbjto
        pure $ Match $ args.match {term = term'}
    , hole : \args chs sbjto -> pure $ Hole args.hole
} args chs sbjto

-- chArgs :: 
-- Need to wait for henry to make args recursor

-- data ToReplace = ReplaceTerm Term TypeChange | ReplaceType Type TypeChange

chSum = undefined

inferChTerm :: Record (Rec.ArgsTerm ()) -> (Changes -> State HoleEq (Term /\ TypeChange))
inferChTerm = undefined
-- inferChTerm = Rec.recTerm {
--     lam : \args chs -> do
--         -- TODO: is it really right that chs isn't updated from the input to the lambda?
--         -- body <- inferChTerm {gamma: ?h, argsIx: ?h, syn: ?h} chs toReplace
--         -- (body' /\ tc) <- inferChTerm args.body.gamma args.gamma.body.type_ args.lam.body chs
--         (body' /\ tc) <- inferChTerm ?a chs -- {gamma: args.body.gamma, syn: {term : args.lam.body}} chs
--         pure $ Lam (args.lam {body=body'}) /\ ArrowCh NoChange tc
--     , neu : undefined
--     , let_ : \args chs -> do
--         let (ty' /\ tc) = chType chs.dataTypeDeletions args.let_.type_
--         term' <- chTerm args.gamma args.let_.type_ chs tc args.let_.term
--         (body' /\ tc) <- inferChTerm ?a -- {gamma: args.gamma.body, syn: {term: args.let_.body}}
--             (varChange chs args.let_.termBind.termId tc)
--         pure $ Let (args.let_ {term = term', body = body'}) /\ tc
--         -- TODO: big problem: what if the index to be changed is in the definition of the let!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--         -- one solution: make Changes the sum of what it is now and a Term to replace at index.
--     , buf : undefined
--     , data_ : undefined
--     , match : undefined
--     , hole : undefined
-- }

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