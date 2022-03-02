module Language.Shape.Stlc.Metadata where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

type ModuleMetadata
  = { hidden :: Boolean }

defaultModuleMetadata :: ModuleMetadata
defaultModuleMetadata = { hidden: false }

type BlockMetadata
  = { hidden :: Boolean }

defaultBlockMetadata :: BlockMetadata
defaultBlockMetadata = { hidden: false }

type TermDefinitionMetadata
  = { hidden :: Boolean }

defaultTermDefinitionMetadata :: TermDefinitionMetadata
defaultTermDefinitionMetadata = { hidden: false }

type DataDefinitionMetadata
  = { hidden :: Boolean }

defaultDataDefinitionMetadata :: DataDefinitionMetadata
defaultDataDefinitionMetadata = { hidden: false }

type ConstructorMetadata
  = {}

defaultConstructorMetadata :: ConstructorMetadata
defaultConstructorMetadata = {}

type BufferMetadata
  = {}

defaultBufferMetadata :: BufferMetadata
defaultBufferMetadata = {}

type ArrowTypeMetadata
  = { indentedParameters :: Boolean }

defaultArrowTypeMetadata :: ArrowTypeMetadata
defaultArrowTypeMetadata = { indentedParameters: false }

type DataTypeMetadata
  = {}

defaultDataTypeMetadata :: DataTypeMetadata
defaultDataTypeMetadata = {}

type HoleTypeMetadata
  = {}

defaultHoleTypeMetadata :: HoleTypeMetadata
defaultHoleTypeMetadata = {}

type LambdaTermMetadata
  = { annotated :: Boolean, indentedParameters :: Boolean }

defaultLambdaTermMetadata :: LambdaTermMetadata
defaultLambdaTermMetadata = { annotated: true, indentedParameters: false }

type ApplicationTermMetadata
  = { indentedArgument :: Boolean }

defaultApplicationTermMetadata :: ApplicationTermMetadata
defaultApplicationTermMetadata = { indentedArgument: false }

type MatchTermMetadata
  = { indentedCases :: Boolean }

defaultMatchTermMetadata :: MatchTermMetadata
defaultMatchTermMetadata = { indentedCases: false }

type HoleTermMetadata
  = {}

defaultHoleTermMetadata :: HoleTermMetadata
defaultHoleTermMetadata = {}

type CaseMetadata
  = { annotated :: Boolean, indentedBlock :: Boolean }

defaultCaseMetadata :: CaseMetadata
defaultCaseMetadata = { annotated: true, indentedBlock: false }

type ParameterMetadata
  = {}

defaultParameterMetadata :: ParameterMetadata
defaultParameterMetadata = {}

type TypeUniqueBindingMetadata
  = { name :: TypeName }

defaultTypeUniqueBindingMetadata :: TypeUniqueBindingMetadata
defaultTypeUniqueBindingMetadata = { name: IgnoreTypeName }

type TermUniqueBindingMetadata
  = { name :: TermName }

defaultTermUniqueBindingMetadata :: TermUniqueBindingMetadata
defaultTermUniqueBindingMetadata = { name: IgnoreTermName }

type TermBindingMetadata
  = {}

defaultTermBindingMetadata :: TermBindingMetadata
defaultTermBindingMetadata = {}

type TermLabelMetadata
  = {}

defaultTermLabelMetadata :: TermLabelMetadata
defaultTermLabelMetadata = {}

type TermReferenceMetadata
  = {}

defaultTermReferenceMetadata :: TermReferenceMetadata
defaultTermReferenceMetadata = {}

data TypeName
  = TypeName String
  | IgnoreTypeName

data TermName
  = TermName String
  | IgnoreTermName

derive instance Generic TypeName _
instance Show TypeName where show x = genericShow x
derive instance Eq TypeName
derive instance Ord TypeName

derive instance Generic TermName _
instance Show TermName where show x = genericShow x
derive instance Eq TermName
derive instance Ord TermName

