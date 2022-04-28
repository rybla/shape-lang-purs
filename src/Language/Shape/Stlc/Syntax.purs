module Language.Shape.Stlc.Syntax where

import Language.Shape.Stlc.Metadata
import Prelude
import Prim hiding (Type)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.UUID (UUID, genUUID)
import Effect.Unsafe (unsafePerformEffect)

-- | Type
data Type
  = ArrowType ArrowType
  | DataType DataType
  | HoleType HoleType

type ArrowType
  = { dom :: Type, cod :: Type, meta :: ArrowTypeMetadata }

type DataType
  = { typeId :: TypeId, meta :: DataTypeMetadata }

type HoleType
  = { holeId :: HoleId, weakening :: Set TypeId, meta :: HoleTypeMetadata }

derive instance genericType :: Generic Type _

instance showType :: Show Type where
  show x = genericShow x

-- | Term
data Term
  = Lam Lam
  | Neu Neu
  | Let Let
  | Buf Buf
  | Data Data
  | Match Match
  | Hole Hole

type Lam
  = { termBind :: TermBind, body :: Term, meta :: LamMetadata }

type Neu
  = { termId :: TermId, argItems :: List ArgItem, meta :: NeuMetadata }

type ArgItem
  = { term :: Term, meta :: ArgItemMetadata }

type Let
  = { termBind :: TermBind, type_ :: Type, term :: Term, body :: Term, meta :: LetMetadata }

type Buf
  = { type_ :: Type, term :: Term, body :: Term, meta :: BufMetadata }

type Data
  = { typeBind :: TypeBind, sumItems :: List SumItem, body :: Term, meta :: DataMetadata }

type Match
  = { type_ :: Type, term :: Term, caseItems :: List CaseItem, meta :: MatchMetadata }

type Hole
  = { meta :: HoleMetadata }

type TypeBind
  = { typeId :: TypeId, meta :: TypeBindMetadata }

type TermBind
  = { termId :: TermId, meta :: TermBindMetadata }

derive instance genericTerm :: Generic Term _

instance showTerm :: Show Term where
  show x = genericShow x

type SumItem
  = { termBind :: TermBind, params :: List Param, meta :: SumItemMetadata }

type CaseItem
  = { termBinds :: List TermBind, body :: Term, meta :: CaseItemMetadata }

type Param
  = { type_ :: Type, meta :: ParamMetadata }

-- | TypeId
newtype TypeId
  = TypeId UUID

derive newtype instance eqTypeId :: Eq TypeId

derive newtype instance ordTypeId :: Ord TypeId

derive newtype instance showTypeId :: Show TypeId

freshTypeId :: Unit -> TypeId
freshTypeId _ = unsafePerformEffect $ TypeId <$> genUUID

-- | TermId
newtype TermId
  = TermId UUID

derive newtype instance eqTermId :: Eq TermId

derive newtype instance ordTermId :: Ord TermId

derive newtype instance showTermId :: Show TermId

freshTermId :: Unit -> TermId
freshTermId _ = unsafePerformEffect $ TermId <$> genUUID

-- | HoleId
newtype HoleId
  = HoleId UUID

derive newtype instance eqHoleId :: Eq HoleId

derive newtype instance ordHoleId :: Ord HoleId

derive newtype instance showHoleId :: Show HoleId

freshHoleId :: Unit -> HoleId
freshHoleId _ = unsafePerformEffect $ HoleId <$> genUUID

-- | Syntax
data Syntax
  = Syntax_ArrowType ArrowType
  | Syntax_DataType DataType
  | Syntax_HoleType HoleType
  | Syntax_Lam Lam
  | Syntax_Neu Neu
  | Syntax_Let Let
  | Syntax_Buf Buf
  | Syntax_Data Data
  | Syntax_Match Match
  | Syntax_Hole Hole
  | Syntax_ArgItem ArgItem
  | Syntax_TypeBind TypeBind
  | Syntax_TermBind TermBind
  | Syntax_SumItem SumItem
  | Syntax_CaseItem CaseItem
  | Syntax_Param Param
  | Syntax_List (List Syntax)

derive instance genericSyntax :: Generic Syntax _

instance showSyntax :: Show Syntax where
  show x = genericShow x
