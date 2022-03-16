module Language.Shape.Stlc.Syntax where

import Language.Shape.Stlc.Metadata
import Prelude
import Data.Tuple.Nested
import Prim hiding (Type)

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.UUID (UUID, genUUID)
import Effect.Unsafe (unsafePerformEffect)
import Unsafe as Unsafe

data Module
  = Module (List (Definition /\ DefinitionItemMetadata)) ModuleMetadata

data Block
  = Block (List Definition) Term BlockMetadata

data Definition
  = TermDefinition TermBinding Type Term TermDefinitionMetadata
  | DataDefinition TypeBinding (List (Constructor /\ ConstructorItemMetadata)) DataDefinitionMetadata

data Constructor
  = Constructor TermBinding (List (Parameter /\ ParameterItemMetadata)) ConstructorMetadata

data Term
  = LambdaTerm TermId Block LambdaTermMetadata
  | HoleTerm HoleTermMetadata
  | MatchTerm TypeId Term (List (Case /\ CaseItemMetadata)) MatchTermMetadata
  | NeutralTerm TermId (List (Term /\ ArgItemMetadata)) NeutralTermMetadata

data Case = Case (List (TermId /\ TermIdItemMetadata)) Term CaseMetadata

data Type
  = ArrowType Parameter Type ArrowTypeMetadata
  | DataType TypeId DataTypeMetadata
  | HoleType HoleId TypeWeakening HoleTypeMetadata
  | ProxyHoleType HoleId

data Parameter = Parameter Type ParameterMetadata

type TypeWeakening
  = Set TypeId

data TermBinding = TermBinding TermId TermBindingMetadata

data TypeBinding = TypeBinding TypeId TypeBindingMetadata

data TermId
  = TermId UUID

data TypeId
  = TypeId UUID

data HoleId
  = HoleId UUID

-- Fresh
freshTermId :: Unit -> TermId
freshTermId _ = unsafePerformEffect $ TermId <$> genUUID

freshTypeId :: Unit -> TypeId
freshTypeId _ = unsafePerformEffect $ TypeId <$> genUUID

freshHoleId :: Unit -> HoleId
freshHoleId _ = unsafePerformEffect $ HoleId <$> genUUID

-- Generic instances
derive instance Generic Module _
derive instance Generic Block _
derive instance Generic Definition _
derive instance Generic Constructor _
derive instance Generic Type _
derive instance Generic Term _
derive instance Generic Case _
derive instance Generic Parameter _
derive instance Generic TypeBinding _
derive instance Generic TermBinding _
derive instance Generic TermId _
derive instance Generic TypeId  _
derive instance Generic HoleId _

-- Show instances
instance Show Module where show x = genericShow x 
instance Show Block where show x = genericShow x 
instance Show Definition where show x = genericShow x 
instance Show Constructor where show x = genericShow x 
instance Show Type where show x = genericShow x 
instance Show Term where show x = genericShow x 
instance Show Case where show x = genericShow x 
instance Show Parameter where show x = genericShow x 
instance Show TypeBinding where show x = genericShow x 
instance Show TermBinding where show x = genericShow x 
instance Show TermId where show x = genericShow x 
instance Show TypeId  where show x = genericShow x 
instance Show HoleId where show x = genericShow x 

-- Eq instances
derive instance Eq Module
derive instance Eq Block
derive instance Eq Definition
derive instance Eq Constructor
derive instance Eq Type
derive instance Eq Term
derive instance Eq Case
derive instance Eq Parameter
derive instance Eq TypeBinding
derive instance Eq TermBinding
derive instance Eq TermId
derive instance Eq HoleId
derive instance Eq TypeId

-- Ord instances
derive instance Ord TermId
derive instance Ord TypeId
derive instance Ord HoleId

data Syntax = 
    SyntaxModule Module
  | SyntaxBlock Block
  | SyntaxDefinition Definition
  | SyntaxConstructor Constructor
  | SyntaxTerm Term
  | SyntaxCase Case
  | SyntaxType Type
  | SyntaxParameter Parameter
  | SyntaxTermBinding TermBinding
  | SyntaxTypeBinding TypeBinding
  | SyntaxTermId TermId

toModule :: Syntax -> Module
toModule (SyntaxModule mod) = mod
toModule _ = Unsafe.error "impossible cast from Syntax"

toBlock :: Syntax -> Block
toBlock (SyntaxBlock block) = block
toBlock _ = Unsafe.error "impossible cast from Syntax"

toDefinition :: Syntax -> Definition
toDefinition (SyntaxDefinition def) = def 
toDefinition _ = Unsafe.error "impossible cast from Syntax"

toConstructor :: Syntax -> Constructor
toConstructor (SyntaxConstructor constr) = constr
toConstructor _ = Unsafe.error "impossible cast from Syntax"

toTerm :: Syntax -> Term
toTerm (SyntaxTerm a)  = a 
toTerm _ = Unsafe.error "impossible cast from Syntax"

toCase :: Syntax -> Case
toCase (SyntaxCase case_) = case_ 
toCase _ = Unsafe.error "impossible cast from Syntax"

toType :: Syntax -> Type
toType (SyntaxType alpha) = alpha
toType _ = Unsafe.error "impossible cast from Syntax"

toParameter :: Syntax -> Parameter
toParameter (SyntaxParameter prm) = prm 
toParameter _ = Unsafe.error "impossible cast from Syntax"

toTermBinding :: Syntax -> TermBinding
toTermBinding (SyntaxTermBinding termBinding) = termBinding
toTermBinding _ = Unsafe.error "impossible cast from Syntax"

toTypeBinding :: Syntax -> TypeBinding
toTypeBinding (SyntaxTypeBinding typeBinding) = typeBinding
toTypeBinding _ = Unsafe.error "impossible cast from Syntax"

toTermId :: Syntax -> TermId
toTermId (SyntaxTermId termId) = termId 
toTermId _ = Unsafe.error "impossible cast from Syntax"