module Language.Shape.Stlc.Example.Base where

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

mkTermVar :: String -> TermId /\ Name
mkTermVar str = freshTermId unit /\ Name (Just str)

mkTypeVar :: String -> TypeId /\ Name
mkTypeVar str = freshTypeId unit /\ Name (Just str)
