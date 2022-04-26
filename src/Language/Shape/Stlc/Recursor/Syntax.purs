module Language.Shape.Stlc.Recursion.Syntax where

import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Prim as Prim
import Prim.Row
import Record
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Language.Shape.Stlc.Recursor.Record

type ProtoArgs r1 r2
  = ( argsSyn :: Record r1 | r2 )

type ProtoRec :: (Row Prim.Type -> Row Prim.Type) -> Row Prim.Type -> Prim.Type -> Prim.Type
type ProtoRec args r a
  = Record (args r) -> a

_argsSyn = Proxy :: Proxy "argsSyn"

-- | recType
type ProtoArgsType r1 r2
  = ProtoArgs ( type_ :: Type | r1 ) r2

type ArgsType r
  = ProtoArgsType () r

type ArgsArrowType r
  = ProtoArgsType ( arrow :: ArrowType ) r

type ArgsDataType r
  = ProtoArgsType ( data_ :: DataType ) r

type ArgsHoleType r
  = ProtoArgsType ( hole :: HoleType ) r

recType ::
  forall r a.
  Lacks "argsSyn" r =>
  { arrow :: ProtoRec ArgsArrowType r a
  , data_ :: ProtoRec ArgsDataType r a
  , hole :: ProtoRec ArgsHoleType r a
  } ->
  ProtoRec ArgsType r a
recType rec args@{ argsSyn } = case argsSyn.type_ of
  ArrowType arrow -> rec.arrow $ modifyHetero _argsSyn (union { arrow }) args
  DataType data_ -> rec.data_ $ modifyHetero _argsSyn (union { data_ }) args
  HoleType hole -> rec.hole $ modifyHetero _argsSyn (union { hole }) args

-- | recTerm
type ProtoArgsTerm r1 r2
  = ProtoArgs ( term :: Term | r1 ) r2

type ArgsTerm r
  = ProtoArgsTerm () r

type ArgsLam r
  = ProtoArgsTerm ( lam :: Lam ) r

type ArgsNeu r
  = ProtoArgsTerm ( neu :: Neu ) r

type ArgsLet r
  = ProtoArgsTerm ( let_ :: Let ) r

type ArgsBuf r
  = ProtoArgsTerm ( buf :: Buf ) r

type ArgsData r
  = ProtoArgsTerm ( data_ :: Data ) r

type ArgsMatch r
  = ProtoArgsTerm ( match :: Match ) r

type ArgsHole r
  = ProtoArgsTerm ( hole :: Hole ) r

recTerm ::
  forall r a.
  Lacks "argsSyn" r =>
  { lam :: ProtoRec ArgsLam r a
  , neu :: ProtoRec ArgsNeu r a
  , let_ :: ProtoRec ArgsLet r a
  , buf :: ProtoRec ArgsBuf r a
  , data_ :: ProtoRec ArgsData r a
  , match :: ProtoRec ArgsMatch r a
  , hole :: ProtoRec ArgsHole r a
  } ->
  ProtoRec ArgsTerm r a
recTerm rec args@{ argsSyn } = case argsSyn.term of
  Lam lam -> rec.lam $ modifyHetero _argsSyn (union { lam }) args
  Neu neu -> rec.neu $ modifyHetero _argsSyn (union { neu }) args
  Let let_ -> rec.let_ $ modifyHetero _argsSyn (union { let_ }) args
  Buf buf -> rec.buf $ modifyHetero _argsSyn (union { buf }) args
  Data data_ -> rec.data_ $ modifyHetero _argsSyn (union { data_ }) args
  Match match -> rec.match $ modifyHetero _argsSyn (union { match }) args
  Hole hole -> rec.hole $ modifyHetero _argsSyn (union { hole }) args
