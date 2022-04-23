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

type RecArrow r a
  = { arrow :: Arrow | r } -> a

type RecBase r a
  = { base :: Base | r } -> a

type RecHoleType r a
  = { holeType :: HoleType | r } -> a

recType :: forall a r. Lacks "type_" r => { arrow :: RecArrow r a, base :: RecBase r a, holeType :: RecHoleType r a } -> RecType r a
recType rec args = case args.type_ of
  Arrow arrow -> rec.arrow $ union { arrow } $ delete (Proxy :: Proxy "type_") args
  Base base -> rec.base $ union { base } $ delete (Proxy :: Proxy "type_") args
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

type RecHoleTerm r a
  = { holeTerm :: HoleTerm | r } -> a

recTerm :: forall r a. Lacks "term" r => { lam :: RecLam r a, app :: RecApp r a, var :: RecVar r a, let_ :: RecLet r a, buf :: RecBuf r a, data_ :: RecData r a, match :: RecMatch r a, holeTerm :: RecHoleTerm r a } -> RecTerm r a
recTerm rec args = case args.term of
  Lam lam -> rec.lam $ union { lam } $ delete (Proxy :: Proxy "term") args
  App app -> rec.app $ union { app } $ delete (Proxy :: Proxy "term") args
  Var var -> rec.var $ union { var } $ delete (Proxy :: Proxy "term") args
  Let let_ -> rec.let_ $ union { let_ } $ delete (Proxy :: Proxy "term") args
  Buf buf -> rec.buf $ union { buf } $ delete (Proxy :: Proxy "term") args
  Data data_ -> rec.data_ $ union { data_ } $ delete (Proxy :: Proxy "term") args
  Match match -> rec.match $ union { match } $ delete (Proxy :: Proxy "term") args
  HoleTerm holeTerm -> rec.holeTerm $ union { holeTerm } $ delete (Proxy :: Proxy "term") args

-- | RecSum
type RecSum r a
  = { sum :: Sum | r } -> a

type RecZero r a
  = { zero :: Zero | r } -> a

type RecPlus r a
  = { plus :: Plus | r } -> a

recSum :: forall r a. Lacks "sum" r => { zero :: RecZero r a, plus :: RecPlus r a } -> RecSum r a
recSum rec args = case args.sum of
  Zero zero -> rec.zero $ union { zero } $ delete (Proxy :: Proxy "sum") args
  Plus plus -> rec.plus $ union { plus } $ delete (Proxy :: Proxy "sum") args

-- | RecCaseSum
type RecCaseSum r a
  = { caseSum :: CaseSum | r } -> a

type RecCaseZero r a
  = { caseZero :: CaseZero | r } -> a

type RecCasePlus r a
  = { casePlus :: CasePlus | r } -> a

recCaseSum :: forall r a. Lacks "caseSum" r => { caseZero :: RecCaseZero r a, casePlus :: RecCasePlus r a } -> RecCaseSum r a
recCaseSum rec args = case args.caseSum of
  CaseZero caseZero -> rec.caseZero $ union { caseZero } $ delete (Proxy :: Proxy "caseSum") args
  CasePlus casePlus -> rec.casePlus $ union { casePlus } $ delete (Proxy :: Proxy "caseSum") args

-- | RecProd
type RecProd r a
  = { prod :: Prod | r } -> a

type RecOne r a
  = { one :: One | r } -> a

type RecMult r a
  = { mult :: Mult | r } -> a

-- | RecCaseProd
type RecCaseProd r a
  = { caseProd :: CaseProd | r } -> a

type RecCaseOne r a
  = { caseOne :: CaseOne | r } -> a

type RecCaseMult r a
  = { caseMult :: CaseMult | r } -> a
