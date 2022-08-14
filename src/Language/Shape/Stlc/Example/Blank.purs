module Language.Shape.Stlc.Example.Blank where

import Prelude
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
term = freshHole unit

type_ :: Type
type_ = freshHoleType unit
