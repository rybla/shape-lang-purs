module Language.Shape.Stlc.Syntax where

import Data.Tuple
import Data.Tuple.Nested
import Language.Shape.Stlc.Metadata
import Prelude
import Prim hiding (Type)
import Record
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over, under)
import Data.Set (Set)
import Data.UUID (UUID)

-- | Type
data Type
  = Arrow Arrow
  | Base Base
  | HoleType HoleType

type Arrow
  = { dom :: Type, cod :: Type, meta :: ArrowMetadata }

type Base
  = { name :: Name, meta :: BaseMetadata }

type HoleType
  = { holeId :: HoleId, weakening :: Set HoleId, meta :: HoleTypeMetadata }

-- | Term
data Term
  = Lam Lam
  | App App
  | Var Var
  | Let Let
  | Buf Buf
  | Data Data
  | Match Match
  | HoleTerm HoleTerm

type Lam
  = { name :: Name, body :: Term, meta :: LamMetadata }

type App
  = { app :: Term, arg :: Term, meta :: AppMetadata }

type Var
  = { name :: Name, meta :: VarMetadata }

type Let
  = { name :: Name, type_ :: Type, arg :: Term, body :: Term, meta :: LetMetadata }

type Buf
  = { type_ :: Type, buf :: Term, body :: Term, meta :: BufMetadata }

type Data
  = { name :: Name, sum :: Sum, body :: Term, meta :: DataMetadata }

type Match
  = { type_ :: Type, arg :: Term, cases :: CaseSum, meta :: MatchMetadata }

type HoleTerm
  = { type_ :: Type, meta :: HoleTermMetadata }

-- | Sum, Prod.
-- | A datatype is defined as a sum of products (SoP). For example, the natural
-- | numbers can be defined in a way similar to the syntax defined below as
-- | ```purescript
-- | Nat =
-- |   Plus { name: "zero", prod: One, sum: 
-- |   Plus { name: "suc" , prod: Mult { name: "n", type_: Nat, prod: One }, sum: 
-- |   Zero } }
-- | ```
data Sum
  = Zero Zero
  | Plus Plus

type Zero
  = {}

type Plus
  = { name :: Name, prod :: Prod, sum :: Sum, meta :: SumMetadata }

data Prod
  = One One
  | Mult Mult

type One
  = {}

type Mult
  = { name :: Name, type_ :: Type, prod :: Prod, meta :: ProdMetadata }

-- | CaseSum, CaseProd.
-- | A pattern matching on a datatype is defined in correspondence to `Sum` and
-- | `Prod` above. For example, a pattern matching on `Nat` as given as an 
-- | example in the section for "Sum, Prod", can be defined along the lines of 
-- | the syntax given below as
-- | ```purescript
-- | isZero n =
-- |   Match { type_: N, arg: n, cases:
-- |     CasePlus { prod: CaseOne { body: <true> }, sum:
-- |     CasePlus { prod: CasePair { name: "n", prod: One { body: <false> } } } }
-- |   }
data CaseSum
  = CaseZero CaseZero
  | CasePlus CasePlus

type CasePlus
  = { prod :: CaseProd, sum :: CaseSum, meta :: CaseSumMetadata }

type CaseZero
  = {} -- no body, since this case is impossible

data CaseProd
  = CaseOne CaseOne
  | CaseMult CaseMult

type CaseOne
  = { body :: Term }

type CaseMult
  = { name :: Name, prod :: CaseProd, meta :: CaseProdMetadata }

-- | HoleId
newtype HoleId
  = HoleId UUID

-- | Name 
newtype Name
  = Name (Maybe String /\ UUID)

derive instance newTypeName :: Newtype Name _

instance eqName :: Eq Name where
  eq (Name (_ /\ uuid1)) ((Name (_ /\ uuid2))) = uuid1 == uuid2
