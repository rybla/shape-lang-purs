module Language.Shape.Stlc.Example.Datatypes where

import Data.Default
import Data.Maybe
import Data.Newtype
import Data.Tuple.Nested
import Language.Shape.Stlc.Example.Base
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Data.Array as Array
import Data.List (List)
import Data.List as List
import Data.Set as Set

term :: Term
term =
  let
    -- type Unit
    unitId /\ unitName = mkTypeVar "Unit"

    ttId /\ ttName = mkTermVar "()"

    -- type Bool
    boolId /\ boolName = mkTypeVar "Bool"

    trueId /\ trueName = mkTermVar "true"

    falseId /\ falseName = mkTermVar "false"

    -- type Nat 
    natId /\ natName = mkTypeVar "Nat"

    zeroId /\ zeroName = mkTermVar "zero"

    sucId /\ sucName = mkTermVar "suc"

    -- type NatList
    natlistId /\ natlistName = mkTypeVar "NatList"

    nilId /\ nilName = mkTermVar "nil"

    consId /\ consName = mkTermVar "cons"

    -- type NatTree
    nattreeId /\ nattreeName = mkTypeVar "NatTree"

    leafId /\ leafName = mkTermVar "leaf"

    branchId /\ branchName = mkTermVar "branch"

    makeDatatypes :: Array (TypeBind /\ List SumItem) -> Term -> Term
    makeDatatypes xs a = case Array.uncons xs of
      Just { head: typeBind /\ sumItems, tail: xs' } -> Data { typeBind, sumItems, body: makeDatatypes xs' a, meta: default }
      Nothing -> a
  in
    makeDatatypes
      [ { typeId: unitId, meta: default # over TypeBindMetadata (_ { name = unitName }) }
          /\ List.fromFoldable
              [ { termBind: { termId: ttId, meta: default # over TermBindMetadata (_ { name = ttName }) }
                , paramItems: List.fromFoldable []
                , meta: default
                }
              ]
      , { typeId: boolId, meta: default # over TypeBindMetadata (_ { name = boolName }) }
          /\ List.fromFoldable
              [ { termBind: { termId: trueId, meta: default # over TermBindMetadata (_ { name = trueName }) }
                , paramItems: List.fromFoldable []
                , meta: default
                }
              , { termBind: { termId: falseId, meta: default # over TermBindMetadata (_ { name = falseName }) }
                , paramItems: List.fromFoldable []
                , meta: default
                }
              ]
      , { typeId: natId, meta: default # over TypeBindMetadata (_ { name = natName }) }
          /\ List.fromFoldable
              [ { termBind: { termId: zeroId, meta: default # over TermBindMetadata (_ { name = zeroName }) }
                , paramItems: List.fromFoldable []
                , meta: default
                }
              , { termBind: { termId: sucId, meta: default # over TermBindMetadata (_ { name = sucName }) }
                , paramItems: List.fromFoldable [ { type_: DataType { typeId: natId, meta: default }, meta: default } ]
                , meta: default
                }
              ]
      , { typeId: natlistId, meta: default # over TypeBindMetadata (_ { name = natlistName }) }
          /\ List.fromFoldable
              [ { termBind: { termId: nilId, meta: default # over TermBindMetadata (_ { name = nilName }) }
                , paramItems: List.fromFoldable []
                , meta: default
                }
              , { termBind: { termId: consId, meta: default # over TermBindMetadata (_ { name = consName }) }
                , paramItems:
                    List.fromFoldable
                      [ { type_: DataType { typeId: natId, meta: default }, meta: default }
                      , { type_: DataType { typeId: natlistId, meta: default }, meta: default }
                      ]
                , meta: default
                }
              ]
      , { typeId: nattreeId, meta: default # over TypeBindMetadata (_ { name = nattreeName }) }
          /\ List.fromFoldable
              [ { termBind: { termId: leafId, meta: default # over TermBindMetadata (_ { name = leafName }) }
                , paramItems: List.fromFoldable []
                , meta: default
                }
              , { termBind: { termId: branchId, meta: default # over TermBindMetadata (_ { name = branchName }) }
                , paramItems:
                    List.fromFoldable
                      [ { type_: DataType { typeId: natId, meta: default }, meta: default }
                      , { type_: DataType { typeId: nattreeId, meta: default }, meta: default }
                      , { type_: DataType { typeId: nattreeId, meta: default }, meta: default }
                      ]
                , meta: default
                }
              ]
      ]
      (freshHole unit)

type_ ∷ Type
type_ = HoleType { holeId: freshHoleId unit, weakening: Set.empty, meta: default }
