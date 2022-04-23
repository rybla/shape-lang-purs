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
  = Arrow (Record Arrow)
  | Base (Record Base)
  | HoleType (Record HoleType)

type Arrow
  = ( dom :: Type, cod :: Type, meta :: ArrowMetadata )

type Base
  = ( name :: Name, meta :: BaseMetadata )

type HoleType
  = ( holeId :: HoleId, weakening :: Set HoleId, meta :: HoleTypeMetadata )

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
  = ( name :: Name, body :: Term, meta :: LamMetadata )

type App
  = ( app :: Term, arg :: Term, meta :: AppMetadata )

type Var
  = ( name :: Name, meta :: VarMetadata )

type Let
  = ( name :: Name, type_ :: Type, arg :: Term, body :: Term, meta :: LetMetadata )

type Buf
  = ( type_ :: Type, buf :: Term, body :: Term, meta :: BufMetadata )

type Data
  = ( name :: Name, sum :: Sum, body :: Term, meta :: DataMetadata )

type Match
  = ( type_ :: Type, arg :: Term, cases :: DestructSum, meta :: MatchMetadata )

type HoleTerm
  = ( type_ :: Type, meta :: HoleTermMetadata )

-- | Sum, Prod
data Sum
  = Zero
  | Sum { name :: Name, prod :: Prod, sum :: Sum, meta :: SumMetadata }

data Prod
  = One
  | Prod { name :: Name, type_ :: Type, prod :: Prod, meta :: ProdMetadata }

-- | DestructSum, DestructProd
data DestructSum
  = DestructSumLeft { name :: Name, prod :: DestructProd, meta :: DestructSumMetadata }
  | DestructSumRight DestructSum

data DestructProd
  = DestructOne
  | DestructProd { name :: Name, prod :: DestructProd, meta :: DestructProdMetadata }

-- | HoleId
newtype HoleId
  = HoleId UUID

-- | Name 
newtype Name
  = Name (Maybe String /\ UUID)

derive instance newTypeName :: Newtype Name _

instance eqName :: Eq Name where
  eq (Name (_ /\ uuid1)) ((Name (_ /\ uuid2))) = uuid1 == uuid2
