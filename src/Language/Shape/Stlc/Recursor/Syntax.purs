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
  = ( syn :: Record r1 | r2 )

type ProtoRec :: (Row Prim.Type -> Row Prim.Type) -> Row Prim.Type -> Prim.Type -> Prim.Type
type ProtoRec args r a
  = Record (args r) -> a

_syn = Proxy :: Proxy "syn"

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
  Lacks "syn" r =>
  { arrow :: ProtoRec ArgsArrowType r a, data_ :: ProtoRec ArgsDataType r a, hole :: ProtoRec ArgsHoleType r a } ->
  ProtoRec ArgsType r a
recType rec args@{ syn } = case syn.type_ of
  ArrowType arrow -> rec.arrow $ modifyHetero _syn (union { arrow }) args
  DataType data_ -> rec.data_ $ modifyHetero _syn (union { data_ }) args
  HoleType hole -> rec.hole $ modifyHetero _syn (union { hole }) args

argsArrowType_dom :: forall r. Lacks "syn" r => Record (ArgsArrowType r) -> Record (ArgsType r)
argsArrowType_dom args = args { syn = { type_: args.syn.arrow.dom } }

argsArrowType_cod :: forall r. Lacks "syn" r => Record (ArgsArrowType r) -> Record (ArgsType r)
argsArrowType_cod args = args { syn = { type_: args.syn.arrow.cod } }

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
  Lacks "syn" r =>
  { lam :: ProtoRec ArgsLam r a, neu :: ProtoRec ArgsNeu r a, let_ :: ProtoRec ArgsLet r a, buf :: ProtoRec ArgsBuf r a, data_ :: ProtoRec ArgsData r a, match :: ProtoRec ArgsMatch r a, hole :: ProtoRec ArgsHole r a } ->
  ProtoRec ArgsTerm r a
recTerm rec args@{ syn } = case syn.term of
  Lam lam -> rec.lam $ modifyHetero _syn (union { lam }) args
  Neu neu -> rec.neu $ modifyHetero _syn (union { neu }) args
  Let let_ -> rec.let_ $ modifyHetero _syn (union { let_ }) args
  Buf buf -> rec.buf $ modifyHetero _syn (union { buf }) args
  Data data_ -> rec.data_ $ modifyHetero _syn (union { data_ }) args
  Match match -> rec.match $ modifyHetero _syn (union { match }) args
  Hole hole -> rec.hole $ modifyHetero _syn (union { hole }) args

argsLam_termBind :: forall r. Lacks "syn" r => Record (ArgsLam r) -> Record (ArgsTermBind r)
argsLam_termBind args = args { syn = { termBind: args.syn.lam.termBind } }

argsLam_body :: forall r. Lacks "syn" r => Record (ArgsLam r) -> Record (ArgsTerm r)
argsLam_body args = args { syn = { term: args.syn.lam.body } }

argsNeu_termId :: forall r. Lacks "syn" r => Record (ArgsNeu r) -> Record (ArgsTermId r)
argsNeu_termId args = args { syn = { termId: args.syn.neu.termId } }

argsNeu_argItems :: forall r. Lacks "syn" r => Record (ArgsNeu r) -> Record (ArgsArgItems r)
argsNeu_argItems args = args { syn = { argItems: args.syn.neu.argItems } }

argsLet_termBind :: forall r. Lacks "syn" r => Record (ArgsLet r) -> Record (ArgsTermBind r)
argsLet_termBind args = args { syn = { termBind: args.syn.let_.termBind } }

argsLet_type :: forall r. Lacks "syn" r => Record (ArgsLet r) -> Record (ArgsType r)
argsLet_type args = args { syn = { type_: args.syn.let_.type_ } }

argsLet_term :: forall r. Lacks "syn" r => Record (ArgsLet r) -> Record (ArgsTerm r)
argsLet_term args = args { syn = { term: args.syn.let_.term } }

argsLet_body :: forall r. Lacks "syn" r => Record (ArgsLet r) -> Record (ArgsTerm r)
argsLet_body args = args { syn = { term: args.syn.let_.body } }

argsBuf_type :: forall r. Lacks "syn" r => Record (ArgsBuf r) -> Record (ArgsType r)
argsBuf_type args = args { syn = { type_: args.syn.buf.type_ } }

argsBuf_term :: forall r. Lacks "syn" r => Record (ArgsBuf r) -> Record (ArgsTerm r)
argsBuf_term args = args { syn = { term: args.syn.buf.term } }

argsBuf_body :: forall r. Lacks "syn" r => Record (ArgsBuf r) -> Record (ArgsTerm r)
argsBuf_body args = args { syn = { term: args.syn.buf.body } }

argsData_typeBind :: forall r. Lacks "syn" r => Record (ArgsData r) -> Record (ArgsTypeBind r)
argsData_typeBind args = args { syn = { typeBind: args.syn.data_.typeBind } }

argsData_sumItems :: forall r. Lacks "syn" r => Record (ArgsData r) -> Record (ArgsSumItems r)
argsData_sumItems args = args { syn = { sumItems: args.syn.data_.sumItems } }

argsData_body :: forall r. Lacks "syn" r => Record (ArgsData r) -> Record (ArgsTerm r)
argsData_body args = args { syn = { term: args.syn.data_.body } }

argsMatch_typeId :: forall r. Lacks "syn" r => Record (ArgsMatch r) -> Record (ArgsTypeId r)
argsMatch_typeId args = args { syn = { typeId: args.syn.match.typeId } }

argsMatch_term :: forall r. Lacks "syn" r => Record (ArgsMatch r) -> Record (ArgsTerm r)
argsMatch_term args = args { syn = { term: args.syn.match.term } }

argsMatch_caseItems :: forall r. Lacks "syn" r => Record (ArgsMatch r) -> Record (ArgsCaseItems r)
argsMatch_caseItems args = args { syn = { caseItems: args.syn.match.caseItems } }

type ProtoArgsArgItems r1 r2
  = ProtoArgs ( argItems :: List ArgItem | r1 ) r2

type ArgsArgItems r
  = ProtoArgsArgItems () r

type ArgsArgItem r
  = ProtoArgsArgItems ( i :: Int, argItem :: ArgItem ) r

recArgItems ::
  forall r a.
  Lacks "syn" r =>
  { argItem :: ProtoRec ArgsArgItem r a } ->
  ProtoRec ArgsArgItems r (List a)
recArgItems rec args@{ syn } = mapWithIndex (\i argItem -> rec.argItem $ modifyHetero _syn (union { i, argItem }) args) syn.argItems

argsArgItem_term :: forall r. Lacks "syn" r => Record (ArgsArgItem r) -> Record (ArgsTerm r)
argsArgItem_term args = args { syn = { term: args.syn.argItem.term } }

-- | recSumItems
type ProtoArgsSumItems r1 r2
  = ProtoArgs ( sumItems :: List SumItem | r1 ) r2

type ArgsSumItems r
  = ProtoArgsSumItems () r

type ArgsSumItem r
  = ProtoArgsSumItems ( i :: Int, sumItem :: SumItem ) r

recSumItems ::
  forall r a.
  Lacks "syn" r =>
  { sumItem :: ProtoRec ArgsSumItem r a } ->
  ProtoRec ArgsSumItems r (List a)
recSumItems rec args@{ syn } =
  mapWithIndex
    (\i sumItem -> rec.sumItem $ modifyHetero _syn (union { i, sumItem }) args)
    syn.sumItems

argsSumItem_termBind :: forall r. Lacks "syn" r => Record (ArgsSumItem r) -> Record (ArgsTermBind r)
argsSumItem_termBind args = args { syn = { termBind: args.syn.sumItem.termBind } }

argsSumItem_paramItems :: forall r. Lacks "syn" r => Record (ArgsSumItem r) -> Record (ArgsParamItems r)
argsSumItem_paramItems args = args { syn = { paramItems: args.syn.sumItem.paramItems } }

-- | recCaseItem
type ProtoArgsCaseItems r1 r2
  = ProtoArgs ( caseItems :: List CaseItem | r1 ) r2

type ArgsCaseItems r
  = ProtoArgsCaseItems () r

type ArgsCaseItem r
  = ProtoArgsCaseItems ( i :: Int, caseItem :: CaseItem ) r

recCaseItems ::
  forall r a.
  Lacks "syn" r =>
  { caseItem :: ProtoRec ArgsCaseItem r a } ->
  ProtoRec ArgsCaseItems r (List a)
recCaseItems rec args@{ syn } =
  mapWithIndex
    (\i caseItem -> rec.caseItem $ modifyHetero _syn (union { i, caseItem }) args)
    syn.caseItems

argsCaseItem_termBindItems :: forall r. Lacks "syn" r => Record (ArgsCaseItem r) -> Record (ArgsTermBindItems r)
argsCaseItem_termBindItems args = args { syn = { termBindItems: args.syn.caseItem.termBindItems } }

argsCaseItem_body :: forall r. Lacks "syn" r => Record (ArgsCaseItem r) -> Record (ArgsTerm r)
argsCaseItem_body args = args { syn = { term: args.syn.caseItem.body } }

-- | recParamItems
type ProtoArgsParamItems r1 r2
  = ProtoArgs ( paramItems :: List ParamItem | r1 ) r2

type ArgsParamItems r
  = ProtoArgsParamItems () r

type ArgsParamItem r
  = ProtoArgsParamItems ( i :: Int, paramItem :: ParamItem ) r

recParamItems ::
  forall r a.
  Lacks "syn" r =>
  { paramItem :: ProtoRec ArgsParamItem r a } ->
  ProtoRec ArgsParamItems r (List a)
recParamItems rec args@{ syn } =
  mapWithIndex
    (\i paramItem -> rec.paramItem $ modifyHetero _syn (union { i, paramItem }) args)
    syn.paramItems

argsParamItem_type :: forall r. Lacks "syn" r => Record (ArgsParamItem r) -> Record (ArgsType r)
argsParamItem_type args = args { syn = { type_: args.syn.paramItem.type_ } }

-- | recTermBindItems
type ProtoArgsTermBindItems r1 r2
  = ProtoArgs ( termBindItems :: List TermBindItem | r1 ) r2

type ArgsTermBindItems r
  = ProtoArgsTermBindItems () r

type ArgsTermBindItem r
  = ProtoArgsTermBindItems ( i :: Int, termBindItem :: TermBindItem ) r

recTermBindItems ::
  forall r a.
  Lacks "syn" r =>
  { termBindItem :: ProtoRec ArgsTermBindItem r a } ->
  ProtoRec ArgsTermBindItems r (List a)
recTermBindItems rec args@{ syn } =
  mapWithIndex
    (\i termBindItem -> rec.termBindItem $ modifyHetero _syn (union { i, termBindItem }) args)
    syn.termBindItems

argsTermBindItem_termBind :: forall r. Lacks "syn" r => Record (ArgsTermBindItem r) -> Record (ArgsTermBind r)
argsTermBindItem_termBind args = args { syn = { termBind: args.syn.termBindItem.termBind } }

-- | recTermBind
type ArgsTermBind r
  = ProtoArgs ( termBind :: TermBind ) r

recTermBind ::
  forall r a.
  Lacks "syn" r =>
  { termBind :: ProtoRec ArgsTermBind r a } ->
  ProtoRec ArgsTermBind r a
recTermBind rec = rec.termBind

-- | recTypeBind
type ArgsTypeBind r
  = ProtoArgs ( typeBind :: TypeBind ) r

recTypeBind ::
  forall r a.
  Lacks "syn" r =>
  { typeBind :: ProtoRec ArgsTypeBind r a } ->
  ProtoRec ArgsTypeBind r a
recTypeBind rec = rec.typeBind

-- | recTypeId
type ArgsTypeId r
  = ProtoArgs ( typeId :: TypeId ) r

recTypeId ::
  forall r a.
  Lacks "syn" r =>
  { typeId :: ProtoRec ArgsTypeId r a } ->
  ProtoRec ArgsTypeId r a
recTypeId rec = rec.typeId

-- | recTermId
type ArgsTermId r
  = ProtoArgs ( termId :: TermId ) r

recTermId ::
  forall r a.
  Lacks "syn" r =>
  { termId :: ProtoRec ArgsTermId r a } ->
  ProtoRec ArgsTermId r a
recTermId rec = rec.termId
