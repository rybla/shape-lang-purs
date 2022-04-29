module Language.Shape.Stlc.Recursion.Syntax where

import Language.Shape.Stlc.Recursor.Record
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Prim.Row
import Record
import Data.List (List(..), mapWithIndex)
import Prim as Prim
import Type.Proxy (Proxy(..))
import Undefined (undefined)

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
  { arrow :: ProtoRec ArgsArrowType r a, data_ :: ProtoRec ArgsDataType r a, hole :: ProtoRec ArgsHoleType r a } ->
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
  { lam :: ProtoRec ArgsLam r a, neu :: ProtoRec ArgsNeu r a, let_ :: ProtoRec ArgsLet r a, buf :: ProtoRec ArgsBuf r a, data_ :: ProtoRec ArgsData r a, match :: ProtoRec ArgsMatch r a, hole :: ProtoRec ArgsHole r a } ->
  ProtoRec ArgsTerm r a
recTerm rec args@{ argsSyn } = case argsSyn.term of
  Lam lam -> rec.lam $ modifyHetero _argsSyn (union { lam }) args
  Neu neu -> rec.neu $ modifyHetero _argsSyn (union { neu }) args
  Let let_ -> rec.let_ $ modifyHetero _argsSyn (union { let_ }) args
  Buf buf -> rec.buf $ modifyHetero _argsSyn (union { buf }) args
  Data data_ -> rec.data_ $ modifyHetero _argsSyn (union { data_ }) args
  Match match -> rec.match $ modifyHetero _argsSyn (union { match }) args
  Hole hole -> rec.hole $ modifyHetero _argsSyn (union { hole }) args

-- | recArgItems
type ProtoArgsArgItems r1 r2
  = ProtoArgs ( argItems :: List ArgItem | r1 ) r2

type ArgsArgItems r
  = ProtoArgsArgItems () r

type ArgsArgItemsNil r
  = ProtoArgsArgItems () r

type ArgsArgItemsCons r
  = ProtoArgsArgItems ( argItem :: ArgItem, argItems :: List ArgItem ) r

recArgItems ::
  forall r a.
  Lacks "argsSyn" r =>
  { cons :: ProtoRec ArgsArgItemsCons r a, nil :: ProtoRec ArgsArgItemsNil r a } ->
  ProtoRec ArgsArgItems r a
recArgItems rec args@{ argsSyn } = case argsSyn.argItems of
  Nil -> rec.nil args
  Cons argItem argItems -> rec.cons $ modifyHetero _argsSyn (union { argItem, argItems }) args

-- | recSumItems
type ProtoArgsSumItems r1 r2
  = ProtoArgs ( sumItems :: List SumItem | r1 ) r2

type ArgsSumItems r
  = ProtoArgsSumItems () r

type ArgsSumItem r
  = ProtoArgsSumItems ( i :: Int, sumItem :: SumItem ) r

recSumItems ::
  forall r a.
  Lacks "argsSyn" r =>
  { sumItem :: ProtoRec ArgsSumItem r a } ->
  ProtoRec ArgsSumItems r (List a)
recSumItems rec args@{ argsSyn } =
  mapWithIndex
    (\i sumItem -> rec.sumItem $ modifyHetero _argsSyn (union { i, sumItem }) args)
    argsSyn.sumItems

-- | recCaseItem
type ProtoArgsCaseItems r1 r2
  = ProtoArgs ( caseItems :: List CaseItem | r1 ) r2

type ArgsCaseItems r
  = ProtoArgsCaseItems () r

type ArgsCaseItem r
  = ProtoArgsCaseItems ( i :: Int, caseItem :: CaseItem ) r

recCaseItems ::
  forall r a.
  Lacks "argsSyn" r =>
  { caseItem :: ProtoRec ArgsCaseItem r a } ->
  ProtoRec ArgsCaseItems r (List a)
recCaseItems rec args@{ argsSyn } =
  mapWithIndex
    (\i caseItem -> rec.caseItem $ modifyHetero _argsSyn (union { i, caseItem }) args)
    argsSyn.caseItems

-- | recParams
type ProtoArgsParams r1 r2
  = ProtoArgs ( params :: List Param | r1 ) r2

type ArgsParams r
  = ProtoArgsParams () r

type ArgsParam r
  = ProtoArgsParams ( i :: Int, param :: Param ) r

recParams ::
  forall r a.
  Lacks "argsSyn" r =>
  { param :: ProtoRec ArgsParam r a } ->
  ProtoRec ArgsParams r (List a)
recParams rec args@{ argsSyn } =
  mapWithIndex
    (\i param -> rec.param $ modifyHetero _argsSyn (union { i, param }) args)
    argsSyn.params

-- | recTermBinds
type ProtoArgsTermBinds r1 r2
  = ProtoArgs ( termBinds :: List TermBind | r1 ) r2

type ArgsTermBinds r
  = ProtoArgsTermBinds () r

type ArgsTermBind r
  = ProtoArgsTermBinds ( i :: Int, termBind :: TermBind ) r

recTermBinds ::
  forall r a.
  Lacks "argsSyn" r =>
  { termBind :: ProtoRec ArgsTermBind r a } ->
  ProtoRec ArgsTermBinds r (List a)
recTermBinds rec args@{ argsSyn } =
  mapWithIndex
    (\i termBind -> rec.termBind $ modifyHetero _argsSyn (union { i, termBind }) args)
    argsSyn.termBinds
