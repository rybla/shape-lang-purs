module Language.Shape.Stlc.Syntax where

import Data.Tuple
import Data.Tuple.Nested
import Language.Shape.Stlc.Metadata
import Prelude
import Prim hiding (Type)
import Record
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over, under)
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
  = { holeId :: HoleId, weakening :: Set HoleId, meta :: HoleTypeMetadata }

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
  = { termId :: TermId, args :: List ArgItem, meta :: NeuMetadata }

type ArgItem
  = { arg :: Arg, meta :: ArgItemMetadata }

type Arg
  = { term :: Term, meta :: ArgMetadata }

type Let
  = { termBind :: TermBind, type_ :: Type, term :: Term, body :: Term, meta :: LetMetadata }

type Buf
  = { type_ :: Type, term :: Term, body :: Term, meta :: BufMetadata }

type Data
  = { typeBind :: TypeBind, sum :: Sum, body :: Term, meta :: DataMetadata }

type Match
  = { type_ :: Type, term :: Term, case_ :: Case, meta :: MatchMetadata }

type Hole
  = { meta :: HoleMetadata }

type TypeBind
  = { typeId :: TypeId, meta :: TypeBindMetadata }

type TermBind
  = { termId :: TermId, meta :: TermBindMetadata }

derive instance genericTerm :: Generic Term _

instance showTerm :: Show Term where
  show x = genericShow x

type Sum
  = List { termBind :: TermBind, termBinds :: List TermBind, meta :: SumItemMetadata }

type Case
  = List { termBinds :: List TermBind, body :: Term, meta :: CaseItemMetadata }

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
