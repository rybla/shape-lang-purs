module Language.Shape.Stlc.Example.Basic where

import Data.Default
import Data.Maybe
import Data.Newtype
import Data.Tuple.Nested
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Data.List as List
import Data.Set as Set
import Language.Shape.Stlc.Example.Base

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
  in
    Data
      { typeBind: { typeId: unitId, meta: default # over TypeBindMetadata (_ { name = unitName }) }
      , sumItems:
          List.fromFoldable
            [ { termBind: { termId: ttId, meta: default # over TermBindMetadata (_ { name = ttName }) }
              , paramItems: List.fromFoldable []
              , meta: default
              }
            ]
      , body:
          Data
            { typeBind: { typeId: boolId, meta: default # over TypeBindMetadata (_ { name = boolName }) }
            , sumItems:
                List.fromFoldable
                  [ { termBind: { termId: trueId, meta: default # over TermBindMetadata (_ { name = trueName }) }
                    , paramItems: List.fromFoldable []
                    , meta: default
                    }
                  , { termBind: { termId: falseId, meta: default # over TermBindMetadata (_ { name = falseName }) }
                    , paramItems: List.fromFoldable []
                    , meta: default
                    }
                  ]
            , body:
                Data
                  { typeBind: { typeId: natId, meta: default # over TypeBindMetadata (_ { name = natName }) }
                  , sumItems:
                      List.fromFoldable
                        [ { termBind: { termId: zeroId, meta: default # over TermBindMetadata (_ { name = zeroName }) }
                          , paramItems: List.fromFoldable []
                          , meta: default
                          }
                        , { termBind: { termId: sucId, meta: default # over TermBindMetadata (_ { name = sucName }) }
                          , paramItems: List.fromFoldable [ { type_: DataType { typeId: natId, meta: default }, meta: default } ]
                          , meta: default
                          }
                        ]
                  , body: Hole { meta: default }
                  , meta: default
                  }
            , meta: default
            }
      , meta: default
      }

type_ ∷ Type
type_ = HoleType { holeId: freshHoleId unit, weakening: Set.empty, meta: default }
