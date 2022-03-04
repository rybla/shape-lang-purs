module Language.Shape.Stlc.Syntax where

import Prelude
import Prim hiding (Type)

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Show.Generic (genericShow)
import Data.UUID (UUID, genUUID)
import Effect.Unsafe (unsafePerformEffect)
import Language.Shape.Stlc.Metadata (ApplicationTermMetadata, BlockMetadata, ConstructorMetadata, DataDefinitionMetadata, HoleTermMetadata, LambdaTermMetadata, ModuleMetadata, TermDefinitionMetadata)

data Module
  = Module Definitions ModuleMetadata

data Block
  = Block Definitions Term BlockMetadata

type Definitions
  = List Definition

data Definition
  = TermDefinition TermID Type Term TermDefinitionMetadata
  | DataDefintion TermID (List Constructor) DataDefinitionMetadata

data Constructor
  = Constructor TermID Type ConstructorMetadata

data Term
  = LambdaTerm TermID Block LambdaTermMetadata
  | ApplicationTerm TermID (List Term) ApplicationTermMetadata
  | HoleTerm HoleTermMetadata
  | MatchTerm TypeID Term (List Term)

data Type
  = ArrowType Type Type
  | DataType TermID
  | HoleType HoleID TypeWeakening

type TypeWeakening
  = List Type

data TermID
  = TermID UUID

data TypeID
  = TypeID UUID

data HoleID
  = HoleID UUID

-- Fresh
freshTermID :: Unit -> TermID
freshTermID _ = unsafePerformEffect $ map TermID genUUID

freshTypeID :: Unit -> TypeID
freshTypeID _ = unsafePerformEffect $ map TypeID genUUID

freshHoleID :: Unit -> HoleID
freshHoleID _ = unsafePerformEffect $ map HoleID genUUID

-- Generic instances
derive instance Generic Module _
derive instance Generic Block _
derive instance Generic Definition _
derive instance Generic Constructor _
derive instance Generic Term _
derive instance Generic Type _
derive instance Generic TermID _
derive instance Generic TypeID  _
derive instance Generic HoleID _

-- Show instances
instance Show Module where show x = genericShow x 
instance Show Block where show x = genericShow x 
instance Show Definition where show x = genericShow x 
instance Show Constructor where show x = genericShow x 
instance Show Term where show x = genericShow x 
instance Show Type where show x = genericShow x 
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