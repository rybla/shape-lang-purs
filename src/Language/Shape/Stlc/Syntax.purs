module Language.Shape.Stlc.Syntax where

import Prelude
import Prim hiding (Type)

import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.Set (Set, empty)
import Data.Show.Generic (genericShow)
import Data.UUID as UUID
import Effect.Unsafe (unsafePerformEffect)
import Language.Shape.Stlc.Metadata

data Module = Module (List Definition) (List Buffer) ModuleMetadata

data Block = Block (List Definition) (List Buffer) Buffer BlockMetadata

data Definition
  -- let <term-unique-binding>: <type> := <term>
  = TermDefinition TermUniqueBinding Type Term TermDefinitionMetadata
  -- data <type-unique-binding> := | <constructor> | ... | <constructor>
  | DataDefinition TypeUniqueBinding (List Constructor) DataDefinitionMetadata

data Constructor
  -- <id>(<name>: <type>, ..., <name>: <type>)
  = Constructor TermUniqueBinding (List Parameter) ConstructorMetadata

type Buffer = NeutralTerm

data Type = ArrowType (List Parameter) BaseType ArrowTypeMetadata

type TypeWeakening = Set TypeId

data BaseType
  = DataType TypeId DataTypeMetadata
  | HoleType HoleId TypeWeakening HoleTypeMetadata

data Term
  -- (<id>, ..., <id>) => <block>
  = LambdaTerm (List TermBinding) Block LambdaTermMetadata

data NeutralTerm
  -- <id> (<arg>, ..., <arg>)
  = ApplicationTerm TermReference (List Term) ApplicationTermMetadata
  -- match <term>: <type> with <cases>
  | MatchTerm TypeId NeutralTerm (List Case) MatchTermMetadata
  | HoleTerm HoleTermMetadata

data Case =
  -- | (<id>, ..., <id>) => <block>
  Case (List TermBinding) Block CaseMetadata

data Parameter = Parameter TermLabel Type ParameterMetadata

-- Type stuff

data TypeUniqueBinding = TypeUniqueBinding TypeId TypeUniqueBindingMetadata
data TypeId = TypeId UUID.UUID
data HoleId = HoleId UUID.UUID


-- Term stuff

data TermUniqueBinding = TermUniqueBinding TermId TermUniqueBindingMetadata
data TermLabel = TermLabel TermName {}
data TermBinding = TermBinding TermId {}
data TermReference = TermReference TermId {}

data TermId = TermId UUID.UUID


-- Make

-- Module

makeModule :: List Definition -> List Buffer -> Module
makeModule defs bufs = Module defs bufs defaultModuleMetadata

-- Block

makeBlock :: List Definition -> List NeutralTerm -> NeutralTerm -> Block
makeBlock defs bufs a = Block defs bufs a defaultBlockMetadata

-- Definition

makeTermDefinition :: TermUniqueBinding -> Type -> Term -> Definition
makeTermDefinition x alpha a = TermDefinition x alpha a defaultTermDefinitionMetadata

makeDataDefinition :: TypeUniqueBinding -> List Constructor -> Definition
makeDataDefinition x cnstrs = DataDefinition x cnstrs defaultDataDefinitionMetadata

-- Constructor

makeConstructor :: TermId -> List Parameter -> Constructor
makeConstructor id prms = Constructor (makeTermUniqueBinding id) prms defaultConstructorMetadata

-- Type

makeArrowType :: List Parameter -> BaseType -> Type
makeArrowType prms out = ArrowType prms out defaultArrowTypeMetadata

makeDataType :: TypeId -> BaseType 
makeDataType id = DataType id defaultDataTypeMetadata

makeHoleType :: Unit -> BaseType 
makeHoleType _ = HoleType (freshHoleId unit) empty defaultHoleTypeMetadata

-- Term

makeLambdaTerm :: List TermBinding -> Block -> Term 
makeLambdaTerm xs block = LambdaTerm xs block defaultLambdaTermMetadata

makeApplicationTerm :: TermReference -> List Term -> NeutralTerm 
makeApplicationTerm x args = ApplicationTerm x args defaultApplicationTermMetadata

makeMatchTerm :: TypeId -> NeutralTerm -> List Case -> NeutralTerm
makeMatchTerm alpha neu cases = MatchTerm alpha neu cases defaultMatchTermMetadata

makeHoleTerm :: NeutralTerm
makeHoleTerm = HoleTerm defaultHoleTermMetadata

-- Case

makeCase :: List TermBinding -> Block -> Case 
makeCase xs block = Case xs block defaultCaseMetadata

-- Parameter

makeParameter :: TermLabel -> Type -> Parameter 
makeParameter label alpha = Parameter label alpha defaultParameterMetadata

-- Type[UniqueBinding|Reference]

makeTypeUniqueBinding :: TypeId -> TypeUniqueBinding
makeTypeUniqueBinding id = TypeUniqueBinding id defaultTypeUniqueBindingMetadata

-- Term[UniqueBinding|Binding|Reference]

makeTermUniqueBinding :: TermId -> TermUniqueBinding 
makeTermUniqueBinding id = TermUniqueBinding id defaultTermUniqueBindingMetadata

makeTermBinding :: TermId -> TermBinding 
makeTermBinding id = TermBinding id defaultTermBindingMetadata

makeTermReference :: TermId -> TermReference
makeTermReference id = TermReference id defaultTermReferenceMetadata

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
derive instance Generic TypeId _
derive instance Generic HoleId _
derive instance Generic TermLabel _ 
derive instance Generic TermUniqueBinding _
derive instance Generic TermBinding _
derive instance Generic TermReference _
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
instance Show TypeId where show x = genericShow x
instance Show HoleId where show x = genericShow x
instance Show TermLabel where show x = genericShow x
instance Show TermUniqueBinding where show x = genericShow x
instance Show TermBinding where show x = genericShow x
instance Show TermReference where show x = genericShow x
instance Show TermId where show x = genericShow x

derive instance Eq TermId
derive instance Ord TermId

derive instance Eq TypeId
derive instance Ord TypeId
