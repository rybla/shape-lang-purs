module Language.Shape.Stlc.Initial where

import Data.List
import Data.Tuple.Nested
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Prelude
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect (Effect)

makeTermVar :: String -> TermId /\ TermName
makeTermVar label = freshTermId unit /\ TermName (Just label)

module_ :: Module
module_ =
  let
    h1_id = freshHoleId unit

    identity_id /\ identity_name = makeTermVar "identity"
  in
    Module
      ( fromFoldable
          [ TermDefinition
              (TermBinding identity_id defaultTermBindingMetadata { name = TermName (Just "identity") })
              (HoleType h1_id Set.empty defaultHoleTypeMetadata)
              (HoleTerm defaultHoleTermMetadata)
              defaultTermDefinitionMetadata
          ]
      )
      defaultModuleMetadata
