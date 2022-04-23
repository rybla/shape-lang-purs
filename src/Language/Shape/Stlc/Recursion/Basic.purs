module Language.Shape.Stlc.Recursion.Basic where

import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Prim.Row
import Record
import Type.Proxy (Proxy(..))
import Undefined (undefined)

-- | RecType
type RecType r a
  = { type_ :: Type | r } -> a

type RecArrowType r a
  = { arrow :: ArrowType | r } -> a

type RecDataType r a
  = { base :: DataType | r } -> a

type RecHoleType r a
  = { holeType :: HoleType | r } -> a

recType :: forall a r. Lacks "type_" r => { arrow :: RecArrowType r a, base :: RecDataType r a, holeType :: RecHoleType r a } -> RecType r a
recType rec args = case args.type_ of
  ArrowType arrow -> rec.arrow $ union { arrow } $ delete (Proxy :: Proxy "type_") args
  DataType base -> rec.base $ union { base } $ delete (Proxy :: Proxy "type_") args
  HoleType holeType -> rec.holeType $ union { holeType } $ delete (Proxy :: Proxy "type_") args

-- | RecTerm
type RecTerm r a
  = { term :: Term | r } -> a

type RecLam r a
  = { lam :: Lam | r } -> a

type RecApp r a
  = { app :: App | r } -> a

type RecVar r a
  = { var :: Var | r } -> a

type RecLet r a
  = { let_ :: Let | r } -> a

type RecBuf r a
  = { buf :: Buf | r } -> a

type RecData r a
  = { data_ :: Data | r } -> a

type RecMatch r a
  = { match :: Match | r } -> a

type RecHole r a
  = { holeTerm :: Hole | r } -> a

recTerm :: forall r a. Lacks "term" r => { lam :: RecLam r a, app :: RecApp r a, var :: RecVar r a, let_ :: RecLet r a, buf :: RecBuf r a, data_ :: RecData r a, match :: RecMatch r a, holeTerm :: RecHole r a } -> RecTerm r a
recTerm rec args = case args.term of
  Lam lam -> rec.lam $ union { lam } $ delete (Proxy :: Proxy "term") args
  App app -> rec.app $ union { app } $ delete (Proxy :: Proxy "term") args
  Var var -> rec.var $ union { var } $ delete (Proxy :: Proxy "term") args
  Let let_ -> rec.let_ $ union { let_ } $ delete (Proxy :: Proxy "term") args
  Buf buf -> rec.buf $ union { buf } $ delete (Proxy :: Proxy "term") args
  Data data_ -> rec.data_ $ union { data_ } $ delete (Proxy :: Proxy "term") args
  Match match -> rec.match $ union { match } $ delete (Proxy :: Proxy "term") args
  Hole holeTerm -> rec.holeTerm $ union { holeTerm } $ delete (Proxy :: Proxy "term") args

-- | RecSum
type RecSum r a
  = { sum :: Sum | r } -> a

type RecZero r a
  = { zero :: Zero | r } -> a

type RecAdd r a
  = { add :: Add | r } -> a

recSum :: forall r a. Lacks "sum" r => { zero :: RecZero r a, add :: RecAdd r a } -> RecSum r a
recSum rec args = case args.sum of
  Zero zero -> rec.zero $ union { zero } $ delete (Proxy :: Proxy "sum") args
  Add add -> rec.add $ union { add } $ delete (Proxy :: Proxy "sum") args

-- | RecSumCase
type RecSumCase r a
  = { sumCase :: SumCase | r } -> a

type RecZeroCase r a
  = { zeroCase :: ZeroCase | r } -> a

type RecAddCase r a
  = { addCase :: AddCase | r } -> a

recSumCase :: forall r a. Lacks "sumCase" r => { zeroCase :: RecZeroCase r a, addCase :: RecAddCase r a } -> RecSumCase r a
recSumCase rec args = case args.sumCase of
  ZeroCase zeroCase -> rec.zeroCase $ union { zeroCase } $ delete (Proxy :: Proxy "sumCase") args
  AddCase addCase -> rec.addCase $ union { addCase } $ delete (Proxy :: Proxy "sumCase") args

-- | RecProd
type RecProd r a
  = { prod :: Prod | r } -> a

type RecOne r a
  = { one :: One | r } -> a

type RecMul r a
  = { mul :: Mul | r } -> a

recProd :: forall r a. Lacks "prod" r => { one :: RecOne r a, mul :: RecMul r a } -> RecProd r a
recProd rec args = case args.prod of
  One one -> rec.one $ union { one } $ delete (Proxy :: Proxy "prod") args
  Mul mul -> rec.mul $ union { mul } $ delete (Proxy :: Proxy "prod") args

-- | RecProdCase
type RecProdCase r a
  = { prodCase :: ProdCase | r } -> a

type RecOneCase r a
  = { oneCase :: OneCase | r } -> a

type RecMulCase r a
  = { mulCase :: MulCase | r } -> a

recProdCase :: forall r a. Lacks "prodCase" r => { oneCase :: RecOneCase r a, mulCase :: RecMulCase r a } -> RecProdCase r a
recProdCase rec args = case args.prodCase of
  OneCase oneCase -> rec.oneCase $ union { oneCase } $ delete (Proxy :: Proxy "prodCase") args
  MulCase mulCase -> rec.mulCase $ union { mulCase } $ delete (Proxy :: Proxy "prodCase") args
