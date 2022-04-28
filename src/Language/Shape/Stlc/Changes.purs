module Language.Shape.Stlc.Changes where

import Data.Tuple.Nested
import Language.Shape.Stlc.Metadata
import Prelude
import Prim hiding (Type)

import Data.Default (default)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Map (Map, insert)
import Data.Map as Map
import Data.Set (Set, difference, member)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Language.Shape.Stlc.Syntax (HoleId(..), TermId(..), Type(..), TypeId(..), freshHoleId)
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