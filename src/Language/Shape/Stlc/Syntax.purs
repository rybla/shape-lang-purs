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

data Module = Module (List Definition)

data Block = Block (List Definition) (List NeutralTerm) NeutralTerm {indented :: Boolean, hidden :: Boolean}

data Definition
  -- let <id>: <type> := <term>
  = TermDefinition TermId Type Term {name :: TermName, hidden :: Boolean}
  -- data <id> := | <constructor> | ... | <constructor>
  | DataDefinition TypeId (List Constructor) {name :: TypeName, hidden :: Boolean}

data Constructor
  -- <id>(<name>: <type>, ..., <name>: <type>)
  = Constructor TermId (List (Tuple TermName Type)) {name :: TermName}

data Type
  -- (<name>: <type, ..., <name>: <type>)
  = ArrowType (List Type) BaseType {names :: List TermName, indented :: Boolean}
  | BaseType BaseType {indented :: Boolean}

data BaseType
  = DataType TypeId {indented :: Boolean}
  | HoleType HoleId TypeWeakening {indented :: Boolean}

data Term
  -- (<id>, ..., <id>) => <block>
  = LambdaTerm (List TermId) Block {indented :: Boolean}
  | NeutralTerm NeutralTerm {indented :: Boolean}

data NeutralTerm
  -- <id> (<arg>, ..., <arg>)
  = ApplicationTerm TermId (List Term) {indented :: Boolean}
  -- match <term>: <type> with <cases>
  | MatchTerm BaseType NeutralTerm (List Case) {indented :: Boolean}
  | HoleTerm {indented :: Boolean}

data Case =
  -- | (<id>, ..., <id>) => <block>
  Case (List TermId) Block

-- unique
newtype TermId = TermId Int

-- unique
newtype TypeId = TypeId Int

-- Hole
newtype HoleId = HoleId Int

-- Weakening
type TypeWeakening = List TypeId

-- Fresh

freshHoleTerm :: Unit -> Term
freshHoleTerm = undefined

freshTermId :: Unit -> TermId
freshTermId = undefined

freshTypeId :: Unit -> TypeId
freshTypeId = undefined

freshHoleId :: Unit -> HoleId
freshHoleId = undefined

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
