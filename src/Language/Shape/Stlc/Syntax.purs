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
  = ArrowType ArrowType
  | DataType DataType
  | HoleType HoleType

type ArrowType
  = { dom :: Type, cod :: Type, meta :: ArrowTypeMetadata }

type DataType
  = { meta :: DataTypeMetadata }

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
  | Hole Hole

type Lam
  = { id :: Id, body :: Term, meta :: LamMetadata }

type App
  = { app :: Term, arg :: Term, meta :: AppMetadata }

type Var
  = { meta :: VarMetadata }

type Let
  = { id :: Id, type_ :: Type, arg :: Term, body :: Term, meta :: LetMetadata }

type Buf
  = { type_ :: Type, buf :: Term, body :: Term, meta :: BufMetadata }

type Data
  = { id :: Id, sum :: Sum, body :: Term, meta :: DataMetadata }

type Match
  = { type_ :: Type, arg :: Term, cases :: CaseSum, meta :: MatchMetadata }

type Hole
  = { type_ :: Type, meta :: HoleMetadata }

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
  = { meta :: ZeroMetadata }

type Plus
  = { id :: Id, prod :: Prod, sum :: Sum, meta :: PlusMetadata }

data Prod
  = One One
  | Mult Mult

type One
  = { meta :: OneMetadata }

type Mult
  = { id :: Id, type_ :: Type, prod :: Prod, meta :: MultMetadata }

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
-- | ```
data CaseSum
  = CaseZero CaseZero
  | CasePlus CasePlus

type CasePlus
  = { prod :: CaseProd, sum :: CaseSum, meta :: CasePlusMetadata }

type CaseZero
  = { meta :: CaseZeroMetadata } -- no body, since this case is impossible

data CaseProd
  = CaseOne CaseOne
  | CaseMult CaseMult

type CaseOne
  = { body :: Term, meta :: CaseOneMetadata }

type CaseMult
  = { id :: Id, prod :: CaseProd, meta :: CaseMultMetadata }

-- | HoleId
newtype HoleId
  = HoleId UUID

-- | Id
newtype Id
  = Id UUID
