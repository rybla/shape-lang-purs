module Language.Shape.Stlc.Hole where

import Control.Monad.State
import Data.Map
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Control.Monad.Except
import Control.Monad.Except as Except
import Undefined (undefined)
import Data.Tuple
import Data.Tuple.Nested

type HoleSub
  = Map HoleId Type
