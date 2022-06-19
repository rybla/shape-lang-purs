module Language.Shape.Stlc.Syntax where

import Language.Shape.Stlc.Metadata
import Prelude
import Prim hiding (Type)
import Data.Default (default)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.UUID (UUID, genUUID)
import Data.UUID as UUID
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Unsafe (unsafePerformEffect)
import Type.Proxy (Proxy(..))
import Undefined (undefined)

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
  = { termBind :: TermBind, sign :: Type, impl :: Term, body :: Term, meta :: LetMetadata }

type Buf
  = { sign :: Type, impl :: Term, body :: Term, meta :: BufMetadata }

type Data
  = { typeBind :: TypeBind, sumItems :: List SumItem, body :: Term, meta :: DataMetadata }

type Match
  = { typeId :: TypeId, term :: Term, caseItems :: List CaseItem, meta :: MatchMetadata }

type Hole
  = { meta :: HoleMetadata }

type TypeBind
  = { typeId :: TypeId, meta :: TypeBindMetadata }

type TermBind
  = { termId :: TermId, meta :: TermBindMetadata }

derive instance genericTerm :: Generic Term _

instance showTerm :: Show Term where
  show x = genericShow x

-- | SumItem
type SumItem
  = { termBind :: TermBind, paramItems :: List ParamItem, meta :: SumItemMetadata }

-- | CaseItem
type CaseItem
  = { termBindItems :: List TermBindItem, body :: Term, meta :: CaseItemMetadata }

-- | ParamItem
type ParamItem
  = { type_ :: Type, meta :: ParamItemMetadata }

type TermBindItem
  = { termBind :: TermBind, meta :: TermBindItemMetadata }

-- | TypeId
newtype TypeId
  = TypeId UUID

derive newtype instance eqTypeId :: Eq TypeId

derive newtype instance ordTypeId :: Ord TypeId


freshTypeId :: Unit -> TypeId
freshTypeId _ = unsafePerformEffect $ TypeId <$> genUUID

freshTypeBind :: Unit -> TypeBind
freshTypeBind _ = { typeId: freshTypeId unit, meta: default }

-- | TermId
newtype TermId
  = TermId UUID

derive newtype instance eqTermId :: Eq TermId

derive newtype instance ordTermId :: Ord TermId

freshTermId :: Unit -> TermId
freshTermId _ = unsafePerformEffect $ TermId <$> genUUID

freshTermBind :: Unit -> TermBind
freshTermBind _ = { termId: freshTermId unit, meta: default }

-- | Type
derive instance eqType :: Eq Type

derive instance ordType :: Ord Type

-- | HoleId
newtype HoleId
  = HoleId UUID

derive newtype instance eqHoleId :: Eq HoleId

derive newtype instance ordHoleId :: Ord HoleId

freshHoleId :: Unit -> HoleId
freshHoleId _ = unsafePerformEffect $ HoleId <$> genUUID

freshHoleType :: Unit -> Type
freshHoleType _ = HoleType { holeId: freshHoleId unit, weakening: Set.empty, meta: default }

freshHole :: Unit -> Term
freshHole _ = Hole { meta: default }

-- | UUID
instance showTypeId :: Show TypeId where
  show (TypeId uuid) = "(TypeId " <> showUUID uuid <> ")"

instance showTermId :: Show TermId where
  show (TermId uuid) = "(TermId " <> showUUID uuid <> ")"

instance showHoleId :: Show HoleId where
  show (HoleId uuid) = "(HoleId " <> showUUID uuid <> ")"

showUUID :: UUID -> String
showUUID uuid = "(fromJust (UUID.parseUUID \"" <> UUID.toString uuid <> "\"))"

-- | Syntax
data Syntax
  = SyntaxType Type
  | SyntaxTerm Term
  | SyntaxTermBind TermBind
  | SyntaxTermId TermId
  | SyntaxTypeBind TypeBind
  | SyntaxArgItem ArgItem
  | SyntaxSumItem SumItem
  | SyntaxCaseItem CaseItem
  | SyntaxParamItem ParamItem
  | SyntaxTermBindItem TermBindItem
  | SyntaxList (List Syntax)

derive instance genericSyntax :: Generic Syntax _

instance showSyntax :: Show Syntax where
  show x = genericShow x

toType =
  ( case _ of
      SyntaxType t -> Just t
      _ -> Nothing
  ) ::
    Syntax -> Maybe Type

toTerm =
  ( case _ of
      SyntaxTerm t -> Just t
      _ -> Nothing
  ) ::
    Syntax -> Maybe Term

toTermBind =
  ( case _ of
      SyntaxTermBind t -> Just t
      _ -> Nothing
  ) ::
    Syntax -> Maybe TermBind

toTermId =
  ( case _ of
      SyntaxTermId t -> Just t
      _ -> Nothing
  ) ::
    Syntax -> Maybe TermId

toTypeBind =
  ( case _ of
      SyntaxTypeBind t -> Just t
      _ -> Nothing
  ) ::
    Syntax -> Maybe TypeBind

toArgItem =
  ( case _ of
      SyntaxArgItem t -> Just t
      _ -> Nothing
  ) ::
    Syntax -> Maybe ArgItem

toSumItem =
  ( case _ of
      SyntaxSumItem t -> Just t
      _ -> Nothing
  ) ::
    Syntax -> Maybe SumItem

toCaseItem =
  ( case _ of
      SyntaxCaseItem t -> Just t
      _ -> Nothing
  ) ::
    Syntax -> Maybe CaseItem

toParamItem =
  ( case _ of
      SyntaxParamItem t -> Just t
      _ -> Nothing
  ) ::
    Syntax -> Maybe ParamItem

toTermBindItem =
  ( case _ of
      SyntaxTermBindItem t -> Just t
      _ -> Nothing
  ) ::
    Syntax -> Maybe TermBindItem

toSyntaxList =
  ( case _ of
      SyntaxList t -> Just t
      _ -> Nothing
  ) ::
    Syntax -> Maybe (List Syntax)

-- overSyntax :: (a -> Syntax) -> (Syntax -> Maybe a) -> (Syntax -> Syntax)
-- overSyntax wrap unwrap f 
