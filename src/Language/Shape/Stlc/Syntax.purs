module Language.Shape.Stlc.Syntax where

import Prelude
import Prim hiding (Type)

import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.Show.Generic (genericShow)
import Data.UUID as UUID
import Effect.Unsafe (unsafePerformEffect)

data Module = Module (List Definition) {cursor :: Boolean}

data Block = Block (List Definition) (List Buffer) Buffer {indented :: Boolean, hidden :: Boolean, cursor :: Boolean}

data Definition
  -- let <term-unique-binding>: <type> := <term>
  = TermDefinition TermUniqueBinding Type Term {hidden :: Boolean, cursor :: Boolean}
  -- data <type-unique-binding> := | <constructor> | ... | <constructor>
  | DataDefinition TypeUniqueBinding (List Constructor) {hidden :: Boolean, cursor :: Boolean}

data Constructor
  -- <id>(<name>: <type>, ..., <name>: <type>)
  = Constructor TermUniqueBinding (List Parameter) {cursor :: Boolean}

type Buffer = NeutralTerm

data Type
  -- (<name>: <type, ..., <name>: <type>)
  = ArrowType (List Parameter) BaseType {indented :: Boolean, cursor :: Boolean}
  | BaseType BaseType

data BaseType
  = DataType TypeId {indented :: Boolean, cursor :: Boolean}
  | HoleType HoleId TypeWeakening {indented :: Boolean, cursor :: Boolean}

data Term
  -- (<id>, ..., <id>) => <block>
  = LambdaTerm (List TermBinding) Block {annotated :: Boolean, indented :: Boolean, cursor :: Boolean}
  | NeutralTerm NeutralTerm

data NeutralTerm
  -- <id> (<arg>, ..., <arg>)
  = ApplicationTerm TermReference (List Term) {indented :: Boolean, cursor :: Boolean}
  -- match <term>: <type> with <cases>
  | MatchTerm BaseType NeutralTerm (List Case) {indented :: Boolean, cursor :: Boolean}
  | HoleTerm {indented :: Boolean, cursor :: Boolean}

data Case =
  -- | (<id>, ..., <id>) => <block>
  Case (List TermBinding) Block {annotated :: Boolean, cursor :: Boolean}

data Parameter = Parameter TermName Type {cursor :: Boolean}

data TypeUniqueBinding = TypeUniqueBinding TypeId {name :: TypeName, cursor :: Boolean}

data TermUniqueBinding = TermUniqueBinding TermId {name :: TermName, cursor :: Boolean}
data TermBinding = TermBinding TermId {cursor :: Boolean}
data TermReference = TermReference TermId {cursor :: Boolean}

data TermId = TermId UUID.UUID

data TypeId = TypeId UUID.UUID

data HoleId = HoleId UUID.UUID

type TypeWeakening = List TypeId

-- Make

-- Module

makeModule :: List Definition -> Module
makeModule defs = Module defs {cursor: false}

-- Block

makeBlock :: List Definition -> List NeutralTerm -> NeutralTerm -> Block
makeBlock defs bufs a = Block defs bufs a {indented: false, hidden: false, cursor: false}

-- Definition

makeTermDefinition :: TermUniqueBinding -> Type -> Term -> Definition
makeTermDefinition x alpha a = TermDefinition x alpha a {hidden: false, cursor: false}

makeDataDefinition :: TypeUniqueBinding -> List Constructor -> Definition
makeDataDefinition x cnstrs = DataDefinition x cnstrs {hidden: false, cursor: false}

-- Constructor

makeConstructor :: TermId -> List Parameter -> Constructor
makeConstructor id prms = Constructor (makeTermUniqueBinding id) prms {cursor: false}

-- Type

makeArrowType :: List Parameter -> BaseType -> Type
makeArrowType prms out = ArrowType prms out {indented: false, cursor: false}

makeBaseType :: BaseType -> Type 
makeBaseType alpha = BaseType alpha

makeDataType :: TypeId -> BaseType 
makeDataType id = DataType id {indented: false, cursor: false}

makeHoleType :: Unit -> BaseType 
makeHoleType _ = HoleType (freshHoleId unit) Nil {indented: false, cursor: false}

-- Term

makeLambdaTerm :: List TermBinding -> Block -> Term 
makeLambdaTerm xs block = LambdaTerm xs block {annotated: true, indented: false, cursor: false}

makeNeutralTerm :: NeutralTerm -> Term 
makeNeutralTerm neu = NeutralTerm neu 

makeApplicationTerm :: TermReference -> List Term -> NeutralTerm 
makeApplicationTerm x args = ApplicationTerm x args {indented: false, cursor: false}

makeMatchTerm :: BaseType -> NeutralTerm -> List Case -> NeutralTerm
makeMatchTerm alpha neu cases = MatchTerm alpha neu cases {indented: false, cursor: false}

makeHoleTerm :: NeutralTerm
makeHoleTerm = HoleTerm {indented: false, cursor: false}

-- Case

makeCase :: List TermBinding -> Block -> Case 
makeCase xs block = Case xs block {annotated: true, cursor: false}

-- Parameter

makeParameter :: TermName -> Type -> Parameter 
makeParameter x alpha = Parameter x alpha {cursor: false}

-- Type[UniqueBinding|Reference]

makeTypeUniqueBinding :: TypeId -> TypeUniqueBinding
makeTypeUniqueBinding id = TypeUniqueBinding id {name: IgnoreTypeName, cursor: false}

-- Term[UniqueBinding|Binding|Reference]

makeTermUniqueBinding :: TermId -> TermUniqueBinding 
makeTermUniqueBinding id = TermUniqueBinding id {name: IgnoreTermName, cursor: false}

makeTermBinding :: TermId -> TermBinding 
makeTermBinding id = TermBinding id {cursor: false}

makeTermReference :: TermId -> TermReference
makeTermReference id = TermReference id {cursor: false}

-- Fresh

freshTermId :: Unit -> TermId
freshTermId _ = unsafePerformEffect $ map TermId UUID.genUUID

freshTypeId :: Unit -> TypeId
freshTypeId _ = unsafePerformEffect $ map TypeId UUID.genUUID

freshHoleId :: Unit -> HoleId
freshHoleId _ = unsafePerformEffect $ map HoleId UUID.genUUID

-- Metadata

data TypeName = TypeName String | IgnoreTypeName

data TermName = TermName String | IgnoreTermName

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

derive instance Generic TermUniqueBinding _
derive instance Generic TermBinding _
derive instance Generic TermReference _
derive instance Generic TermName _
derive instance Generic TermId _

derive instance Generic TypeUniqueBinding _
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

instance Show Parameter where show x = genericShow x

instance Show TermUniqueBinding where show x = genericShow x
instance Show TermBinding where show x = genericShow x
instance Show TermReference where show x = genericShow x
instance Show TermName where show x = genericShow x
instance Show TermId where show x = genericShow x

instance Show TypeUniqueBinding where show x = genericShow x
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
