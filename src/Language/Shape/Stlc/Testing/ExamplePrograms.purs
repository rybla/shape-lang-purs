
module Language.Shape.Stlc.Testing.ExamplePrograms where

import Data.Tuple.Nested
import Prelude

import Data.List (List(..), (:), fromFoldable)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Language.Shape.Stlc.Metadata (TypeName(..), defaultDataDefinitionMetadata, defaultDefinitionItemMetadata, defaultModuleMetadata, defaultTypeBindingMetadata)
import Language.Shape.Stlc.Syntax (Constructor(..), Definition(..), DefinitionItem, Module(..), TypeBinding(..), TypeId(..), ConstructorItem, freshTypeId)
import Undefined (undefined)

-- a shallow embedding DSL of the language to make it easier to write terms for testing
qMod :: List DefinitionItem -> Module
qMod defs = Module defs defaultModuleMetadata

-- qDataDef :: List ConstructorItem -> DefinitionItem
-- qDataDef ctrs = DataDefinition (TypeBinding typeId defaultTypeBindingMetadata) ctrs defaultDataDefinitionMetadata /\ defaultDefinitionItemMetadata

-- qDataDef : DefinitionItem

-- letDataType :: String -> (TypeId -> (List ConstructorItem) /\ List DefinitionItem) -> List DefinitionItem
-- letDataType name rest =
--     let tyId = freshTypeId unit
--     in let (ctrs /\ defs) = rest tyId
--     in (DataDefinition (TypeBinding tyId defaultTypeBindingMetadata {name = TypeName (Just name)}) ctrs defaultDataDefinitionMetadata
--         /\ defaultDefinitionItemMetadata) : defs

-- qItems :: forall a. Array a -> List a
-- qItems = fromFoldable

-- qCtr :: Constructor

-- letDataType "Nat" (\NatId Nat -> [
--     "zero" /\ [],
--     "suc" /\ [Nat]
-- ] /\ ....

-- prog1 :: Module
-- prog1 = qMod (
--     letDataType "Nat" (\nat -> qItems [
--          -- no way to lambda bind the constructors for later!
--     ] /\ undefined
--     )
-- )

{-
IDEA: if every function is uncurried, then could make a single function
a -> (a -> b) -> b
-}

-- bind