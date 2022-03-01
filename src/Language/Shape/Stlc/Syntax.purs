module Language.Shape.Stlc.Syntax where

import Prelude
import Prim hiding (Type)

import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.Set (Set, empty)
import Data.Show.Generic (genericShow)
import Data.UUID as UUID
import Effect.Unsafe (unsafePerformEffect)

data Module = Module (List Definition) (List Buffer) {}

data Block = Block (List Definition) (List Buffer) Buffer {indented :: Boolean, hidden :: Boolean}

data Definition
  -- let <term-unique-binding>: <type> := <term>
  = TermDefinition TermUniqueBinding Type Term {hidden :: Boolean}
  -- data <type-unique-binding> := | <constructor> | ... | <constructor>
  | DataDefinition TypeUniqueBinding (List Constructor) {hidden :: Boolean}

data Constructor
  -- <id>(<name>: <type>, ..., <name>: <type>)
  = Constructor TermUniqueBinding (List Parameter) {}

type Buffer = NeutralTerm

data Type
  -- (<name>: <type, ..., <name>: <type>)
  = ArrowType (List Parameter) BaseType {indented :: Boolean}
  | BaseType BaseType

type TypeWeakening = Set TypeId

data BaseType
  = DataType TypeId {indented :: Boolean}
  | HoleType HoleId TypeWeakening {indented :: Boolean}

data Term
  -- (<id>, ..., <id>) => <block>
  = LambdaTerm (List TermBinding) Block {annotated :: Boolean, indented :: Boolean}
  | NeutralTerm NeutralTerm

data NeutralTerm
  -- <id> (<arg>, ..., <arg>)
  = ApplicationTerm TermReference (List Term) {indented :: Boolean}
  -- match <term>: <type> with <cases>
  | MatchTerm TypeId NeutralTerm (List Case) {indented :: Boolean}
  | HoleTerm {indented :: Boolean}

data Case =
  -- | (<id>, ..., <id>) => <block>
  Case (List TermBinding) Block {annotated :: Boolean}

data Parameter = Parameter TermLabel Type {}

-- Type stuff

data TypeUniqueBinding = TypeUniqueBinding TypeId {name :: TypeName}
data TypeId = TypeId UUID.UUID
data HoleId = HoleId UUID.UUID
data TypeName = TypeName String | IgnoreTypeName

-- Term stuff

data TermUniqueBinding = TermUniqueBinding TermId {name :: TermName}
data TermLabel = TermLabel TermName {}
data TermBinding = TermBinding TermId {}
data TermReference = TermReference TermId {}

data TermId = TermId UUID.UUID
data TermName = TermName String | IgnoreTermName

-- Make

-- Module

makeModule :: List Definition -> List Buffer -> Module
makeModule defs bufs = Module defs bufs {}

-- Block

makeBlock :: List Definition -> List NeutralTerm -> NeutralTerm -> Block
makeBlock defs bufs a = Block defs bufs a {indented: false, hidden: false}

-- Definition

makeTermDefinition :: TermUniqueBinding -> Type -> Term -> Definition
makeTermDefinition x alpha a = TermDefinition x alpha a {hidden: false}

makeDataDefinition :: TypeUniqueBinding -> List Constructor -> Definition
makeDataDefinition x cnstrs = DataDefinition x cnstrs {hidden: false}

-- Constructor

makeConstructor :: TermId -> List Parameter -> Constructor
makeConstructor id prms = Constructor (makeTermUniqueBinding id) prms {}

-- Type

makeArrowType :: List Parameter -> BaseType -> Type
makeArrowType prms out = ArrowType prms out {indented: false}

makeBaseType :: BaseType -> Type 
makeBaseType alpha = BaseType alpha

makeDataType :: TypeId -> BaseType 
makeDataType id = DataType id {indented: false}

makeHoleType :: Unit -> BaseType 
makeHoleType _ = HoleType (freshHoleId unit) empty {indented: false}

-- Term

makeLambdaTerm :: List TermBinding -> Block -> Term 
makeLambdaTerm xs block = LambdaTerm xs block {annotated: true, indented: false}

makeNeutralTerm :: NeutralTerm -> Term 
makeNeutralTerm neu = NeutralTerm neu 

makeApplicationTerm :: TermReference -> List Term -> NeutralTerm 
makeApplicationTerm x args = ApplicationTerm x args {indented: false}

makeMatchTerm :: TypeId -> NeutralTerm -> List Case -> NeutralTerm
makeMatchTerm alpha neu cases = MatchTerm alpha neu cases {indented: false}

makeHoleTerm :: NeutralTerm
makeHoleTerm = HoleTerm {indented: false}

-- Case

makeCase :: List TermBinding -> Block -> Case 
makeCase xs block = Case xs block {annotated: true}

-- Parameter

makeParameter :: TermLabel -> Type -> Parameter 
makeParameter label alpha = Parameter label alpha {}

-- Type[UniqueBinding|Reference]

makeTypeUniqueBinding :: TypeId -> TypeUniqueBinding
makeTypeUniqueBinding id = TypeUniqueBinding id {name: IgnoreTypeName}

-- Term[UniqueBinding|Binding|Reference]

makeTermUniqueBinding :: TermId -> TermUniqueBinding 
makeTermUniqueBinding id = TermUniqueBinding id {name: IgnoreTermName}

makeTermBinding :: TermId -> TermBinding 
makeTermBinding id = TermBinding id {}

makeTermReference :: TermId -> TermReference
makeTermReference id = TermReference id {}

-- Fresh

freshTermId :: Unit -> TermId
freshTermId _ = unsafePerformEffect $ map TermId UUID.genUUID

freshTypeId :: Unit -> TypeId
freshTypeId _ = unsafePerformEffect $ map TypeId UUID.genUUID

freshHoleId :: Unit -> HoleId
freshHoleId _ = unsafePerformEffect $ map HoleId UUID.genUUID


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
derive instance Generic Parameter _ 
derive instance Generic TypeUniqueBinding _
derive instance Generic TypeName _
derive instance Generic TypeId _
derive instance Generic HoleId _
derive instance Generic TermLabel _ 
derive instance Generic TermUniqueBinding _
derive instance Generic TermBinding _
derive instance Generic TermReference _
derive instance Generic TermName _
derive instance Generic TermId _

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
instance Show Parameter where show x = genericShow x
instance Show TypeUniqueBinding where show x = genericShow x
instance Show TypeName where show x = genericShow x
instance Show TypeId where show x = genericShow x
instance Show HoleId where show x = genericShow x
instance Show TermLabel where show x = genericShow x
instance Show TermUniqueBinding where show x = genericShow x
instance Show TermBinding where show x = genericShow x
instance Show TermReference where show x = genericShow x
instance Show TermName where show x = genericShow x
instance Show TermId where show x = genericShow x

derive instance Eq TermName
derive instance Ord TermName

derive instance Eq TypeName
derive instance Ord TypeName

derive instance Eq TermId
derive instance Ord TermId

derive instance Eq TypeId
derive instance Ord TypeId
