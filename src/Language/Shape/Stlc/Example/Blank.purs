module Language.Shape.Stlc.Example.Blank where

import Data.Default
import Data.Maybe
import Data.Newtype
import Data.Tuple.Nested
import Language.Shape.Stlc.Example.Base
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Prelude
import Prelude
import Prim hiding (Type)
import Data.List as List
import Data.Set as Set
import Language.Shape.Stlc.Types (Program)

program :: Program
program = { term, type_ }

term :: Term
term = freshHole unit

type_ :: Type
type_ = freshHoleType unit
