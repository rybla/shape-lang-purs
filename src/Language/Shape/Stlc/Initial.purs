module Language.Shape.Stlc.Initial where

import Data.List
import Data.Tuple.Nested
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Prelude
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect (Effect)

makeTypeVar :: String -> TypeId /\ TypeName
makeTypeVar label = freshTypeId unit /\ TypeName (Just label)

makeTermVar :: String -> TermId /\ TermName
makeTermVar label = freshTermId unit /\ TermName (Just label)

module_ :: Module
module_ =
  let
    h1_id = freshHoleId unit

    nat_id /\ nat_name = makeTypeVar "Nat"

    zero_id /\ zero_name = makeTermVar "zero"

    suc_id /\ suc_name = makeTermVar "suc"

    identity_id /\ identity_name = makeTermVar "identity"

    n_name = TermName (Just "n")

    x_id /\ x_name = makeTermVar "x"
  in
    Module
      ( fromFoldable
          [ DataDefinition (TypeBinding nat_id defaultTypeBindingMetadata { name = nat_name })
              ( fromFoldable
                  [ Constructor
                      (TermBinding zero_id defaultTermBindingMetadata { name = zero_name })
                      Nil
                      defaultConstructorMetadata
                  , Constructor
                      (TermBinding suc_id defaultTermBindingMetadata { name = suc_name })
                      (fromFoldable [ Parameter (DataType nat_id defaultDataTypeMetadata) defaultParameterMetadata { name = n_name } ])
                      defaultConstructorMetadata
                  ]
              )
              defaultDataDefinitionMetadata
          , TermDefinition
              (TermBinding identity_id defaultTermBindingMetadata { name = identity_name })
              ( ArrowType
                  (Parameter (DataType nat_id defaultDataTypeMetadata) defaultParameterMetadata { name = x_name })
                  (DataType nat_id defaultDataTypeMetadata)
                  defaultArrowTypeMetadata
              )
              ( LambdaTerm
                  x_id
                  ( Block Nil
                      (NeutralTerm x_id NoneArgs defaultNeutralTermMetadata)
                      defaultBlockMetadata
                  )
                  defaultLambdaTermMetadata
              )
              defaultTermDefinitionMetadata
          ]
      )
      defaultModuleMetadata
