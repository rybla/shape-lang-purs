module Language.Shape.Stlc.Syntax where

import Data.Tuple
import Prelude
import Prim hiding (Type)
import Undefined

import Data.Boolean as Boolean
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.Show.Generic (genericShow)

data Module = Module (List.List Definition)

data Definition
  = TermDefinition TermName TermId Type Term
  | DataDefinition TypeName TypeId (List.List Constructor)

data Constructor
  = Constructor TermName TermId (Tuple TermName TermId)

data Type
  = ArrowType (List.List (Tuple TermName Type)) BaseType
  | BaseType BaseType

data Block
  = Block (List.List Definition) (List.List NeutralTerm) NeutralTerm

data BaseType
  = DataType TypeId
  | HoleType HoleId TypeWeakening

data Term
  = LambdaTerm (List.List TermId) Block -- the TermIds are specified in its `ArrowType`
  | NeutralTerm NeutralTerm

data Case = Case (List Term) Block

data NeutralTerm
  = ApplicationTerm TermId (List.List Term)
  | MatchTerm TypeId BaseType (List Case)
  | HoleTerm

-- unique
newtype TermId = TermId Int

-- unique
newtype TypeId = TypeId Int

-- Hole
newtype HoleId = HoleId Int

freshHoleTerm :: Unit -> Term
freshHoleTerm = undefined

freshTermId :: Unit -> TermId
freshTermId = undefined

freshTypeId :: Unit -> TypeId
freshTypeId = undefined

freshHoleId :: Unit -> HoleId
freshHoleId = undefined

-- Weakening
type TypeWeakening = List.List TypeId

-- Metadata

data TermName = TermName String
data TypeName = TypeName String

-- Instances

-- Generic
derive instance Generic Module _ 
derive instance Generic Block _
derive instance Generic Definition _
derive instance Generic Constructor _
derive instance Generic Type _
derive instance Generic BaseType _
derive instance Generic Term _
derive instance Generic Case _
derive instance Generic NeutralTerm _
derive instance Generic TermName _
derive instance Generic TermId _
derive instance Generic TypeName _
derive instance Generic TypeId _
derive instance Generic HoleId _

-- Show
instance Show Module where show x = genericShow x
instance Show Definition where show x = genericShow x
instance Show Constructor where show x = genericShow x
instance Show Type where show x = genericShow x
instance Show Block where show x = genericShow x
instance Show BaseType where show x = genericShow x
instance Show Term where show x = genericShow x
instance Show Case where show x = genericShow x
instance Show NeutralTerm where show x = genericShow x
instance Show TermName where show x = genericShow x
instance Show TermId where show x = genericShow x
instance Show TypeName where show x = genericShow x
instance Show TypeId where show x = genericShow x
instance Show HoleId where show x = genericShow x

derive instance Eq TermName
derive instance Ord TermName

derive instance Eq TypeName
derive instance Ord TypeName

derive instance Eq TermId
derive instance Ord TermId

derive instance Eq TypeId
derive instance Ord TypeId
