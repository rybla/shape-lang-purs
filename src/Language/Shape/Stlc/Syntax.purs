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
  = { id :: Id, meta :: DataTypeMetadata }

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
  = { id :: Id, body :: Term, meta :: LamMetadata }

type Neu
  = { id :: Id, args :: List Arg, meta :: NeuMetadata }

type Arg
  = { term :: Term, meta :: ArgMetadata }

type Let
  = { id :: Id, type_ :: Type, term :: Term, body :: Term, meta :: LetMetadata }

type Buf
  = { type_ :: Type, term :: Term, body :: Term, meta :: BufMetadata }

type Data
  = { id :: Id, sum :: Sum, body :: Term, meta :: DataMetadata }

type Match
  = { type_ :: Type, term :: Term, cases :: SumCases, meta :: MatchMetadata }

type Hole
  = { meta :: HoleMetadata }

derive instance genericTerm :: Generic Term _

instance showTerm :: Show Term where
  show x = genericShow x

-- TODO
type Sum
  = Unit

type SumCases
  = Unit

--
-- -- TODO: probably will change these to just lists of lists, but I want to make sure I understand how its setup first
-- -- | Sum, Prod.
-- -- | A datatype is defined as a sum of products (SoP). For example, the natural
-- -- | numbers can be defined in a way similar to the syntax defined below as
-- -- | ```purescript
-- -- | Nat =
-- -- |   Add { name: "zero", prod: One, sum: 
-- -- |   Add { name: "suc" , prod: Mul { name: "n", type_: Nat, prod: One }, sum: 
-- -- |   Zero } }
-- -- | ```
-- data Sum
--   = Zero Zero
--   | Add Add
-- type Zero
--   = { meta :: ZeroMetadata }
-- type Add
--   = { id :: Id, prod :: Prod, sum :: Sum, meta :: AddMetadata }
-- data Prod
--   = One One
--   | Mul Mul
-- type One
--   = { meta :: OneMetadata }
-- type Mul
--   = { id :: Id, type_ :: Type, prod :: Prod, meta :: MulMetadata }
-- -- | SumCases, ProdCase.
-- -- | A pattern matching on a datatype is defined in correspondence to `Sum` and
-- -- | `Prod` above. For example, a pattern matching on `Nat` as given as an 
-- -- | example in the section for "Sum, Prod", can be defined along the lines of 
-- -- | the syntax given below as
-- -- | ```purescript
-- -- | isZero n =
-- -- |   Match { type_: N, arg: n, cases:
-- -- |     AddCase { prod: OneCase { body: <true> }, sum:
-- -- |     AddCase { prod: CasePair { name: "n", prod: One { body: <false> } } } }
-- -- |   }
-- -- | ```
-- data SumCases
--   = ZeroCase ZeroCase
--   | AddCase AddCase
-- type AddCase
--   = { prod :: ProdCase, sum :: SumCases, meta :: AddCaseMetadata }
-- type ZeroCase
--   = { meta :: ZeroCaseMetadata } -- no body, since this case is impossible
-- data ProdCase
--   = OneCase OneCase
--   | MulCase MulCase
-- type OneCase
--   = { body :: Term, meta :: OneCaseMetadata }
-- type MulCase
--   = { id :: Id, prod :: ProdCase, meta :: MulCaseMetadata }
-- | HoleId
newtype HoleId
  = HoleId UUID

derive newtype instance eqHoleId :: Eq HoleId

derive newtype instance ordHoleId :: Ord HoleId

derive newtype instance showHoleId :: Show HoleId

freshHoleId :: Unit -> HoleId
freshHoleId _ = unsafePerformEffect $ HoleId <$> genUUID

-- | Id
newtype Id
  = Id UUID

derive newtype instance eqId :: Eq Id

derive newtype instance ordId :: Ord Id

derive newtype instance showId :: Show Id

freshId :: Unit -> Id
freshId _ = unsafePerformEffect $ Id <$> genUUID
