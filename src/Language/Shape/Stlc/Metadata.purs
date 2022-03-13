module Language.Shape.Stlc.Metadata where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

type ModuleMetadata
  = { hidden :: Boolean }

defaultModuleMetadata :: ModuleMetadata
defaultModuleMetadata = { hidden: false }

type BlockMetadata
  = { indented::Boolean , hidden :: Boolean }

defaultBlockMetadata :: BlockMetadata
defaultBlockMetadata = { indented:false, hidden: false }

type TermDefinitionMetadata
  = { indented::Boolean, hidden :: Boolean }

defaultTermDefinitionMetadata :: TermDefinitionMetadata
defaultTermDefinitionMetadata = { indented:false, hidden: false }

type DataDefinitionMetadata
  = { hidden :: Boolean }

defaultDataDefinitionMetadata :: DataDefinitionMetadata
defaultDataDefinitionMetadata = { hidden: false }

type ConstructorMetadata = {}

defaultConstructorMetadata :: ConstructorMetadata
defaultConstructorMetadata = {}

type ArrowTypeMetadata = {}

defaultArrowTypeMetadata :: ArrowTypeMetadata
defaultArrowTypeMetadata = {}

type DataTypeMetadata = {}

defaultDataTypeMetadata :: DataTypeMetadata
defaultDataTypeMetadata = {}

type HoleTypeMetadata
  = {}

defaultHoleTypeMetadata :: HoleTypeMetadata
defaultHoleTypeMetadata = {}

type LambdaTermMetadata
  = { annotated :: Boolean, indented :: Boolean }

defaultLambdaTermMetadata :: LambdaTermMetadata
defaultLambdaTermMetadata = { annotated: true, indented: false }

type ArgConsMetaData = { indented :: Boolean }

defaultArgConsMetaData :: ArgConsMetaData
defaultArgConsMetaData = { indented : false }

type NeutralTermMetadata = {}

defaultNeutralTermMetadata :: NeutralTermMetadata
defaultNeutralTermMetadata = {}

type VariableTermMetadata =  {}
defaultVariableTermMetadata :: VariableTermMetadata
defaultVariableTermMetadata = {} 

type MatchTermMetadata
  = { indented :: Boolean }

defaultMatchTermMetadata :: MatchTermMetadata
defaultMatchTermMetadata = { indented: false }

type HoleTermMetadata
  = {}

defaultHoleTermMetadata :: HoleTermMetadata
defaultHoleTermMetadata = {}

type CaseMetadata = {}

defaultCaseMetadata :: CaseMetadata
defaultCaseMetadata = {}

type ParameterMetadata = { name :: TermName }

defaultParameterMetadata = {name: TermName Nothing}

type TypeBindingMetadata = {name::TypeName}

defaultTypeBindingMetadata :: TypeBindingMetadata
defaultTypeBindingMetadata = {name:TypeName Nothing}

type TermBindingMetadata = {name::TermName}

defaultTermBindingMetadata :: TermBindingMetadata
defaultTermBindingMetadata = {name: TermName Nothing}

data TypeName = TypeName (Maybe String)
data TermName = TermName (Maybe String)

-- instances for TypeName
derive instance Generic TypeName _
derive instance Eq TypeName
derive instance Ord TypeName

instance Show TypeName where
  show (TypeName (Just label)) = label
  show (TypeName Nothing) = "_"

-- instances for TermName
derive instance Generic TermName _
derive instance Eq TermName
derive instance Ord TermName

instance Show TermName where
  show (TermName (Just label)) = label
  show (TermName Nothing) = "_"


