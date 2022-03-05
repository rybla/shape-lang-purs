module Language.Shape.Stlc.Metadata where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Undefined (undefined)

type ModuleMetadata
  = { hidden :: Boolean }

defaultModuleMetadata :: ModuleMetadata
defaultModuleMetadata = { hidden: false }

type BlockMetadata
  = { indented::Boolean , hidden :: Boolean }

defaultBlockMetadata :: BlockMetadata
defaultBlockMetadata = { indented:false, hidden: false }

type TermDefinitionMetadata
  = { name::TermName, hidden :: Boolean }

defaultTermDefinitionMetadata :: TermDefinitionMetadata
defaultTermDefinitionMetadata = { name: TermName Nothing,  hidden: false }

type DataDefinitionMetadata
  = { name::TypeName, hidden :: Boolean }

defaultDataDefinitionMetadata :: DataDefinitionMetadata
defaultDataDefinitionMetadata = { name: TypeName Nothing, hidden: false }

type ConstructorMetadata
  = {name::TermName}

defaultConstructorMetadata :: ConstructorMetadata
defaultConstructorMetadata = {name: TermName Nothing}

type ArrowTypeMetadata
  = {name::TermName}

defaultArrowTypeMetadata :: ArrowTypeMetadata
defaultArrowTypeMetadata = {name: TermName Nothing}

type DataTypeMetadata
  = {}

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

type ApplicationTermMetadata
  = { indented :: List Boolean } -- tells if each term is indented

defaultApplicationTermMetadata :: ApplicationTermMetadata
defaultApplicationTermMetadata = { indented: undefined }

type MatchTermMetadata
  = { indented :: Boolean }

defaultMatchTermMetadata :: MatchTermMetadata
defaultMatchTermMetadata = { indented: false }

type HoleTermMetadata
  = {}

defaultHoleTermMetadata :: HoleTermMetadata
defaultHoleTermMetadata = {}

data TypeName = TypeName (Maybe String)
data TermName = TermName (Maybe String)

-- instances for TypeName
derive instance Generic TypeName _
instance Show TypeName where show x = genericShow x
derive instance Eq TypeName
derive instance Ord TypeName

-- instances for TermName
derive instance Generic TermName _
instance Show TermName where show x = genericShow x
derive instance Eq TermName
derive instance Ord TermName

