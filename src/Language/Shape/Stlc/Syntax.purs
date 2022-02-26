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

data Block = Block (List Definition) (List NeutralTerm) NeutralTerm

data Definition
  -- let <id>: <type> := <term>
  = TermDefinition TermName TermId Type Term
  -- data <id> := | <constructor> | ... | <constructor>
  | DataDefinition TypeName TypeId (List Constructor)

data Constructor
  -- <id>(<name>: <type>, ..., <name>: <type>)
  = Constructor TermName TermId (List (Tuple TermName Type))

data Type
  -- (<name>: <type, ..., <name>: <type>)
  = ArrowType (List (Tuple TermName Type)) BaseType
  | BaseType BaseType

data BaseType
  = DataType TypeId
  | HoleType HoleId TypeWeakening

data Term
<<<<<<< HEAD
  = LambdaTerm (List TermId) Block -- the names are specified in its `ArrowType`
=======
  -- (<id>, ..., <id>) => <block>
  = LambdaTerm (List TermId) Block
>>>>>>> 6c54532712c6d9eee7467875b32a91129dcf3945
  | NeutralTerm NeutralTerm

data NeutralTerm
  -- <id> (<arg>, ..., <arg>)
  = ApplicationTerm TermId (List Term)
  -- match <term>: <type> with <cases>
  | MatchTerm BaseType NeutralTerm (List Case)
  | HoleTerm

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
