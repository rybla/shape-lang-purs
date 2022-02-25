module Language.Shape.Stlc.Syntax where

import Prelude
import Prim hiding (Type)
import Data.Tuple
import Data.List as List
import Undefined
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Module = Module (List.List Definition)

data Definition
  = TermDefinition UniqueTermBinding Type Term
  | DataDefinition UniqueTypeBinding (List.List Constructor)

data Constructor
  = Constructor UniqueTermBinding (List.List Parameter)

data Type
  = ArrowType (List.List Parameter) BaseType
  | BaseType BaseType

data Block
  = Block (List.List Definition) (List.List Buffer) NeutralTerm

data Buffer = Buffer NeutralTerm

data BaseType
  = DataType TypeReference
  | HoleType HoleId TypeWeakening

data Term
  = LambdaTerm (List.List TermBinding) Block -- the TermIds are specified in its `ArrowType`
  | NeutralTerm NeutralTerm

data NeutralTerm
  = ApplicationTerm TermReference (List.List Term)
  | HoleTerm HoleId

-- Parameter, TermBinding, UniqueTermBinding
-- A `Parameter` appears where the type of a function specifies the `TermLabel` of a `Parameter` and its `Type`, as in `ArrowType` or `Constructor`. No `TermId` is specified since this is not an instance of the `TermLabel` as a term. The same `Parameter`'s `TermLabel` could be instantiated multiple times, such as in distinct `LambdaTerm`s and `MatchTerm` cases.
data Parameter = Parameter TermLabel Type

data TermLabel = TermLabel TermName

-- TermReference, TermLabel, TermName, TermId
-- A `TermBinding` appears where an instance of a `TermName` is bound, as in `LambdaTerm` and `Case`. The `TermName` that is bound is contextually determined, by a `ArrowType` and `Constructor` respectively.
data TermBinding = TermBinding TermId

-- A `UniqueTermBinding` appears where a `TermName` is introduced and a unique instance of that `TermName` is bound at once.
data UniqueTermBinding = UniqueTermBinding TermName TermId

data TermReference = TermReference TermId

-- not necessarily unique
data TermName = VariableName String | PrincipleName String

-- unique
newtype TermId = TermId Int

--  UniqueTypeBinding, TypeReference, TypeName, TypeId
data UniqueTypeBinding
  = UniqueTypeBinding TypeName TypeId

data TypeReference = TypeReference TypeId

-- not necessarily unique
newtype TypeName = TypeName String

-- unique
newtype TypeId = TypeId Int

-- Hole
newtype HoleId = HoleId Int

freshHoleTerm :: Unit -> Term
freshHoleTerm = undefined

-- Weakening & Substitution
type TypeWeakening = List.List TypeName

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
derive instance Generic Parameter _
derive instance Generic TermLabel _
derive instance Generic UniqueTermBinding _
derive instance Generic TermBinding _
derive instance Generic TermReference _
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
instance Show Parameter where show x = genericShow x
instance Show TermLabel where show x = genericShow x
instance Show UniqueTermBinding where show x = genericShow x
instance Show TermBinding where show x = genericShow x
instance Show TermReference where show x = genericShow x
instance Show TermName where show x = genericShow x
instance Show TermId where show x = genericShow x
instance Show UniqueTypeBinding where show x = genericShow x
instance Show TypeReference where show x = genericShow x
instance Show TypeName where show x = genericShow x
instance Show TypeId where show x = genericShow x
instance Show HoleId where show x = genericShow x

-- Eq/Ord
derive instance Eq TermName
derive instance Ord TermName

derive instance Eq TermId
derive instance Ord TermId

derive instance Eq TypeId
derive instance Ord TypeId
