module Language.Shape.Stlc.Syntax where

import Data.Tuple.Nested
import Language.Shape.Stlc.Metadata
import Prelude
import Prim hiding (Type)

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.Tuple (fst)
import Data.UUID (UUID, genUUID)
import Effect.Unsafe (unsafePerformEffect)
import Unsafe as Unsafe

data Module
  = Module (List DefinitionItem) ModuleMetadata

data Block
  = Block (List DefinitionItem) Term BlockMetadata

type DefinitionItem = Definition /\ DefinitionItemMetadata

data Definition
  = TermDefinition TermBinding Type Term TermDefinitionMetadata
  | DataDefinition TypeBinding (List ConstructorItem) DataDefinitionMetadata

type ConstructorItem = Constructor /\ ConstructorItemMetadata

data Constructor
  = Constructor TermBinding (List ParameterItem) ConstructorMetadata

type ParameterItem = Parameter /\ ParameterItemMetadata

data Term
  = LambdaTerm TermId Block LambdaTermMetadata
  | HoleTerm HoleTermMetadata
  | MatchTerm TypeId Term (List CaseItem) MatchTermMetadata
  | NeutralTerm TermId (List ArgItem) NeutralTermMetadata

type CaseItem = Case /\ CaseItemMetadata

type ArgItem = Term /\ ArgItemMetadata

data Case = Case (List TermIdItem) Block CaseMetadata

type TermIdItem = TermId /\ TermIdItemMetadata

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

-- mk

mkModule defItems = Module defItems defaultModuleMetadata 

mkBlock defItems a = Block defItems a defaultBlockMetadata
mkBlockInd defItems a = Block defItems a defaultBlockMetadata {indented=true}

mkDefItem def = def /\ defaultDefinitionItemMetadata

mkTermDef termBind alpha a = TermDefinition termBind alpha a defaultTermDefinitionMetadata

mkDataDef typeBind constrItems = DataDefinition typeBind constrItems defaultDataDefinitionMetadata

mkConstrItem constr = constr /\ defaultConstructorItemMetadata

mkConstr termBind paramItems = Constructor termBind paramItems defaultConstructorMetadata

mkParamItem param = param /\ defaultParameterItemMetadata

mkLambda termId block = LambdaTerm termId block defaultLambdaTermMetadata
mkLambdaInd termId block = LambdaTerm termId block defaultLambdaTermMetadata {indented=true}
mkHoleTerm = HoleTerm defaultHoleTermMetadata
mkMatch typeId a caseItems = MatchTerm typeId a caseItems defaultMatchTermMetadata
mkNeutral termId argItems = NeutralTerm termId argItems defaultNeutralTermMetadata

mkCaseItem case_ = case_ /\ defaultCaseItemMetadata

mkArgItem a = a /\ defaultArgItemMetadata

mkCase termIdItems block = Case termIdItems block defaultCaseMetadata
mkCaseInd termIdItems block = Case termIdItems block defaultCaseMetadata {indented=true}

mkTermIdItem termId = termId /\ defaultTermIdItemMetadata

mkArrow param alpha = ArrowType param alpha defaultArrowTypeMetadata
mkData typeId = DataType typeId defaultDataTypeMetadata
mkHoleType holeId wkn = HoleType holeId wkn defaultHoleTypeMetadata
mkProxyHoleType holeId = ProxyHoleType holeId 

mkParam name alpha = Parameter alpha defaultParameterMetadata {name = name}

mkTermBind termId name = TermBinding termId defaultTermBindingMetadata {name = name }

mkTypeBind typeId name  = TypeBinding typeId defaultTypeBindingMetadata {name = name}




-- Item

fromItem :: forall a md. a /\ md -> a
fromItem = fst

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
  -- items
  | SyntaxDefinitionItem DefinitionItem
  | SyntaxConstructorItem ConstructorItem
  | SyntaxParameterItem ParameterItem
  | SyntaxCaseItem CaseItem
  | SyntaxArgItem ArgItem 
  | SyntaxTermIdItem TermIdItem
  -- for lists
  | SyntaxList (List Syntax)

derive instance Generic Syntax _ 
instance Show Syntax where show x = genericShow x 

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
toParameter (SyntaxParameter param) = param 
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

toDefinitionItem :: Syntax -> DefinitionItem
toDefinitionItem (SyntaxDefinitionItem defItem) = defItem
toDefinitionItem _ = Unsafe.error "impossible to cast from Syntax"

toConstructorItem :: Syntax -> ConstructorItem
toConstructorItem (SyntaxConstructorItem defItem) = defItem
toConstructorItem _ = Unsafe.error "impossible to cast from Syntax"

toParameterItem :: Syntax -> ParameterItem
toParameterItem (SyntaxParameterItem defItem) = defItem
toParameterItem _ = Unsafe.error "impossible to cast from Syntax"

toCaseItem :: Syntax -> CaseItem
toCaseItem (SyntaxCaseItem defItem) = defItem
toCaseItem _ = Unsafe.error "impossible to cast from Syntax"

toArgItem :: Syntax -> ArgItem
toArgItem (SyntaxArgItem defItem) = defItem
toArgItem _ = Unsafe.error "impossible to cast from Syntax"

toTermIdItem :: Syntax -> TermIdItem
toTermIdItem (SyntaxTermIdItem defItem) = defItem
toTermIdItem _ = Unsafe.error "impossible to cast from Syntax"

toSyntaxList :: Syntax -> List Syntax 
toSyntaxList (SyntaxList syns) = syns
toSyntaxList _ = Unsafe.error "impossible to cast from Syntax"
