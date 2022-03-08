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
  | DataDefintion TypeBinding (List Constructor) DataDefinitionMetadata

data Constructor
  = Constructor TermBinding (List Parameter) ConstructorMetadata

data Term
  = LambdaTerm TermBinding Block LambdaTermMetadata
  | HoleTerm HoleTermMetadata
  | MatchTerm TypeID Term (List Case) MatchTermMetadata
  | NeutralTerm NeutralTerm NeutralTermMetadata

data Case = Case (List TermBinding) Term CaseMetadata

data NeutralTerm
  = VariableTerm TermID VariableTermMetadata
  | ApplicationTerm NeutralTerm Term ApplicationTermMetadata

data Type
  = ArrowType Parameter Type ArrowTypeMetadata
  | DataType TypeID DataTypeMetadata
  | HoleType HoleID TypeWeakening HoleTypeMetadata

data Parameter = Parameter Type ParameterMetadata

type TypeWeakening
  = Set TypeID

data TermBinding = TermBinding TermID TermBindingMetadata

data TypeBinding = TypeBinding TypeID TypeBindingMetadata

data TermID
  = TermID UUID

data TypeID
  = TypeID UUID

data HoleID
  = HoleID UUID

-- Fresh
freshTermID :: Unit -> TermID
freshTermID _ = unsafePerformEffect $ TermID <$> genUUID

freshTypeID :: Unit -> TypeID
freshTypeID _ = unsafePerformEffect $ TypeID <$> genUUID

freshHoleID :: Unit -> HoleID
freshHoleID _ = unsafePerformEffect $ HoleID <$> genUUID

-- Generic instances
derive instance Generic Module _
derive instance Generic Block _
derive instance Generic Definition _
derive instance Generic Constructor _
derive instance Generic Type _
derive instance Generic Term _
derive instance Generic NeutralTerm _
derive instance Generic Case _
derive instance Generic Parameter _
derive instance Generic TypeBinding _
derive instance Generic TermBinding _
derive instance Generic TermID _
derive instance Generic TypeID  _
derive instance Generic HoleID _

-- Show instances
instance Show Module where show x = genericShow x 
instance Show Block where show x = genericShow x 
instance Show Definition where show x = genericShow x 
instance Show Constructor where show x = genericShow x 
instance Show Type where show x = genericShow x 
instance Show Term where show x = genericShow x 
instance Show NeutralTerm where show x = genericShow x 
instance Show Case where show x = genericShow x 
instance Show Parameter where show x = genericShow x 
instance Show TypeBinding where show x = genericShow x 
instance Show TermBinding where show x = genericShow x 
instance Show TermID where show x = genericShow x 
instance Show TypeID  where show x = genericShow x 
instance Show HoleID where show x = genericShow x 

-- Eq instances
derive instance Eq TermID
derive instance Eq HoleID
derive instance Eq TypeID

-- Ord instances
derive instance Ord TermID
derive instance Ord TypeID
derive instance Ord HoleID