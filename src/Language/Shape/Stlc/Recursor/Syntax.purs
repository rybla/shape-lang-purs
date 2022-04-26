module Language.Shape.Stlc.Recursion.Syntax where

import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Prim.Row
import Record
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Language.Shape.Stlc.Recursor.Record

type ProtoArgs r1 r2
  = ( argsSyn :: Record r1 | r2 )

type ProtoRec r1 r2 a
  = Record (ProtoArgs r1 r2) -> a

_argsSyn = Proxy :: Proxy "argsSyn"

-- | RecType
type ProtoRecType r1 r2 a
  = ProtoRec ( type_ :: Type | r1 ) r2 a

type RecType r a
  = ProtoRecType () r a

type RecArrowType r a
  = ProtoRecType ( arrow :: ArrowType ) r a

type RecDataType r a
  = ProtoRecType ( data_ :: DataType ) r a

type RecHoleType r a
  = ProtoRecType ( hole :: HoleType ) r a

recType :: forall r a. Lacks "argsSyn" r => { arrow :: RecArrowType r a, data_ :: RecDataType r a, hole :: RecHoleType r a } -> RecType r a
recType rec args@{ argsSyn } = case argsSyn.type_ of
  ArrowType arrow -> rec.arrow $ modifyHetero _argsSyn (union { arrow }) args
  DataType data_ -> rec.data_ $ modifyHetero _argsSyn (union { data_ }) args
  HoleType hole -> rec.hole $ modifyHetero _argsSyn (union { hole }) args

-- | RecTerm
type ProtoRecTerm r1 r2 a
  = ProtoRec ( term :: Term | r1 ) r2 a

type RecTerm r a
  = ProtoRecTerm () r a

type RecLam r a
  = ProtoRecTerm ( lam :: Lam ) r a

type RecNeu r a
  = ProtoRecTerm ( neu :: Neu ) r a

type RecLet r a
  = ProtoRecTerm ( let_ :: Let ) r a

type RecBuf r a
  = ProtoRecTerm ( buf :: Buf ) r a

type RecData r a
  = ProtoRecTerm ( data_ :: Data ) r a

type RecMatch r a
  = ProtoRecTerm ( match :: Match ) r a

type RecHole r a
  = ProtoRecTerm ( hole :: Hole ) r a

recTerm :: forall r a. Lacks "argsSyn" r => { lam :: RecLam r a, neu :: RecNeu r a, let_ :: RecLet r a, buf :: RecBuf r a, data_ :: RecData r a, match :: RecMatch r a, hole :: RecHole r a } -> RecTerm r a
recTerm rec args@{ argsSyn } = case argsSyn.term of
  Lam lam -> rec.lam $ modifyHetero _argsSyn (union { lam }) args
  Neu neu -> rec.neu $ modifyHetero _argsSyn (union { neu }) args
  Let let_ -> rec.let_ $ modifyHetero _argsSyn (union { let_ }) args
  Buf buf -> rec.buf $ modifyHetero _argsSyn (union { buf }) args
  Data data_ -> rec.data_ $ modifyHetero _argsSyn (union { data_ }) args
  Match match -> rec.match $ modifyHetero _argsSyn (union { match }) args
  Hole hole -> rec.hole $ modifyHetero _argsSyn (union { hole }) args
