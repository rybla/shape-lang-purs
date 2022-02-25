module Language.Shape.Stlc.Syntax where

import Data.Tuple
import Prelude
import Prim hiding (Type)
import Undefined

import Data.Generic.Rep (class Generic)
import Data.List as List
import Data.Show.Generic (genericShow)
import Data.Boolean as Boolean

data Module = Module (List.List Definition)

data Definition
  = TermDefinition TermName TermId Type Term
  | DataDefinition TypeName TypeId TermId (List.List Constructor)

data Constructor
  = Constructor TermName TermId (Tuple TermName TermId)

data Type
  = ArrowType (List.List (Tuple TermName Type)) BaseType
  | BaseType BaseType

data Block
  = Block (List.List Definition) (List.List Buffer) NeutralTerm

data Buffer = Buffer NeutralTerm

data BaseType
  = DataType TypeReference
  | HoleType HoleId TypeWeakening

data Term
  = LambdaTerm (List.List TermId) Block -- the TermIds are specified in its `ArrowType`
  | NeutralTerm NeutralTerm

data NeutralTerm
  = ApplicationTerm TermId (List.List Term)
  | HoleTerm HoleId

-- unique
newtype TermId = TermId Int

--  UniqueTypeBinding, TypeReference, TypeName, TypeId
data UniqueTypeBinding
  = UniqueTypeBinding TypeName TypeId

data TypeReference = TypeReference TypeId

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

-- Weakening
type TypeWeakening = List.List TypeName

-- Metadata

data TermName = VariableName String | PrincipleName TypeName (List.List Constructor)
data TypeName = TypeName String

-- Instances

-- Generic
derive instance Generic Module _ 
derive instance Generic Block _
derive instance Generic Buffer _
derive instance Generic Definition _
derive instance Generic Constructor _
derive instance Generic Type _
derive instance Generic BaseType _
derive instance Generic Term _
derive instance Generic NeutralTerm _
derive instance Generic TermName _
derive instance Generic TermId _
derive instance Generic UniqueTypeBinding _
derive instance Generic TypeReference _
derive instance Generic TypeName _
derive instance Generic TypeId _
derive instance Generic HoleId _

-- Show
instance Show Module where show x = genericShow x
instance Show Definition where show x = genericShow x
instance Show Constructor where show x = genericShow x
instance Show Type where show x = genericShow x
instance Show Block where show x = genericShow x
instance Show Buffer where show x = genericShow x
instance Show BaseType where show x = genericShow x
instance Show Term where show x = genericShow x
instance Show NeutralTerm where show x = genericShow x
instance Show TermName where show x = genericShow x
instance Show TermId where show x = genericShow x
instance Show UniqueTypeBinding where show x = genericShow x
instance Show TypeReference where show x = genericShow x
instance Show TypeName where show x = genericShow x
instance Show TypeId where show x = genericShow x
instance Show HoleId where show x = genericShow x

instance Eq TermName where 
  eq (VariableName name1) (VariableName name2) = name1 == name2 
  eq (PrincipleName name1 _) (PrincipleName name2 _) = name1 == name2 
  eq _ _ = false

instance Ord TermName where
  compare (VariableName name1) (VariableName name2) = compare name1 name2
  compare (PrincipleName name1 _) (PrincipleName name2 _) = compare name1 name2
  compare (VariableName _) (PrincipleName _ _) = LT
  compare (PrincipleName _ _) (VariableName _) = GT

derive instance Eq TypeName
derive instance Ord TypeName

derive instance Eq TermId
derive instance Ord TermId

derive instance Eq TypeId
derive instance Ord TypeId
