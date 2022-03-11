module Language.Shape.Stlc.Syntax where

import Prelude
import Prim hiding (Type)

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.UUID (UUID, genUUID)
import Effect.Unsafe (unsafePerformEffect)
import Language.Shape.Stlc.Metadata

data Module
  = Module (List Definition) ModuleMetadata

data Block
  = Block (List Definition) Term BlockMetadata

data Definition
  = TermDefinition TermBinding Type Term TermDefinitionMetadata
  | DataDefinition TypeBinding (List Constructor) DataDefinitionMetadata

data Constructor
  = Constructor TermBinding (List Parameter) ConstructorMetadata

data Term
  = LambdaTerm TermId Block LambdaTermMetadata
  | HoleTerm HoleTermMetadata
  | MatchTerm TypeId Term (List Case) MatchTermMetadata
  | NeutralTerm TermId Args NeutralTermMetadata

data Args
  = NoneArgs
  | ConsArgs Term Args ArgConsMetaData

data Case = Case (List TermId) Term CaseMetadata

data Type
  = ArrowType Parameter Type ArrowTypeMetadata
  | DataType TypeId DataTypeMetadata
  | HoleType HoleId TypeWeakening HoleTypeMetadata
  | ProxyHoleType HoleId

data Parameter = Parameter Type ParameterMetadata

type TypeWeakening
  = Set TypeId

data TermBinding = TermBinding TermId TermBindingMetadata

data TypeBinding = TypeBinding TypeId TypeBindingMetadata

data TermId
  = TermId UUID

data TypeId
  = TypeId UUID

data HoleId
  = HoleId UUID

-- Fresh
freshTermId :: Unit -> TermId
freshTermId _ = unsafePerformEffect $ TermId <$> genUUID

freshTypeId :: Unit -> TypeId
freshTypeId _ = unsafePerformEffect $ TypeId <$> genUUID

freshHoleId :: Unit -> HoleId
freshHoleId _ = unsafePerformEffect $ HoleId <$> genUUID

-- Generic instances
derive instance Generic Module _
derive instance Generic Block _
derive instance Generic Definition _
derive instance Generic Constructor _
derive instance Generic Type _
derive instance Generic Term _
derive instance Generic Args _
derive instance Generic Case _
derive instance Generic Parameter _
derive instance Generic TypeBinding _
derive instance Generic TermBinding _
derive instance Generic TermId _
derive instance Generic TypeId  _
derive instance Generic HoleId _

-- Show instances
instance Show Module where show x = genericShow x 
instance Show Block where show x = genericShow x 
instance Show Definition where show x = genericShow x 
instance Show Constructor where show x = genericShow x 
instance Show Type where show x = genericShow x 
instance Show Term where show x = genericShow x 
instance Show Args where show x = genericShow x 
instance Show Case where show x = genericShow x 
instance Show Parameter where show x = genericShow x 
instance Show TypeBinding where show x = genericShow x 
instance Show TermBinding where show x = genericShow x 
instance Show TermId where show x = genericShow x 
instance Show TypeId  where show x = genericShow x 
instance Show HoleId where show x = genericShow x 

-- Eq instances
derive instance Eq TermId
derive instance Eq HoleId
derive instance Eq TypeId

-- Ord instances
derive instance Ord TermId
derive instance Ord TypeId
derive instance Ord HoleId