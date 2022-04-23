module Language.Shape.Stlc.Recursion.Basic where

import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Record
import Undefined (undefined)

type RecArrow r a
  = { arrow :: Arrow | r } -> a

type RecBase r a
  = { base :: Base | r } -> a

type RecHoleType r a
  = { holeType :: HoleType | r } -> a

recType :: forall a r. { arrow :: RecArrow r a, base :: RecBase r a, holeType :: RecHoleType r a } -> a
recType rec = undefined
