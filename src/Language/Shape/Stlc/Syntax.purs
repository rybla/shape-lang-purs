module Language.Shape.Stlc.Syntax where

import Data.Tuple
import Data.Tuple.Nested
import Prelude
import Prim hiding (Type)
import Record

import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.UUID (UUID)

-- | Type
data Type
  = Arrow (Record Arrow)
  | Base (Record Base)
  | HoleType (Record HoleType)

type Arrow
  = ( dom :: Type, cod :: Type )

type Base
  = ( name :: Name )

type HoleType
  = ( holeId :: HoleId, weakening :: Weakening )

type Weakening
  = Set HoleId

-- | Term
data Term
  = Lam (Record Lam)
  | App (Record App)
  | Var (Record Var)
  | Let (Record Let)
  | Buf (Record Buf)
  | Data (Record Data)
  | Match (Record Match)
  | HoleTerm (Record HoleTerm)

type Lam
  = ( name :: Name, body :: Term )

type App
  = ( app :: Term, arg :: Term )

type Var
  = ( name :: Name )

type Let
  = ( name :: Name, type_ :: Type, arg :: Term, body :: Term )

type Buf
  = ( type_ :: Type, buf :: Term, body :: Term )

type Data
  = ( name :: Name, sum :: Sum, body :: Term )

type Match
  = ( type_ :: Type, arg :: Term, cases :: Cases )

type HoleTerm
  = ( type_ :: Type )

-- Sum 

data Sum = Zero | Sum Prod Sum 

-- Prod

data Prod = One | Prod Type Prod

-- Cases

data Cases = Cases

-- HoleId

newtype HoleId = HoleId UUID

-- Name 

newtype Name = Name (Maybe String /\ UUID)
