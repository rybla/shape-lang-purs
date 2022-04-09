module Language.Shape.Stlc.Testing.ComparePrograms where

import Data.Tuple.Nested
import Prelude

import Data.List (List(..), (:), fromFoldable)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Language.Shape.Stlc.Holes (HoleSub)
import Language.Shape.Stlc.Metadata (TypeName(..), defaultDataDefinitionMetadata, defaultDefinitionItemMetadata, defaultModuleMetadata, defaultTypeBindingMetadata)
import Language.Shape.Stlc.Syntax (Constructor(..), ConstructorItem, Definition(..), DefinitionItem, Module(..), TypeBinding(..), TypeId(..), freshTypeId)
import Undefined (undefined)

unifyModule :: Module -> Module -> Maybe HoleSub
unifyModule = undefined