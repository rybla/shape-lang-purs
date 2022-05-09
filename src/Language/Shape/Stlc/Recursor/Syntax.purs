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

type ProtoArgs r
  = ( | r )

type ProtoRec :: (Row Prim.Type -> Row Prim.Type) -> Row Prim.Type -> Prim.Type -> Prim.Type
type ProtoRec args r a
  = Record (args r) -> a

_syn = Proxy :: Proxy "syn"

-- | recType
type ProtoArgsType r1 r2
  = ProtoArgs ( here :: (type_::Type | r1) | r2 )

type ArgsType r
  = ProtoArgsType () ( | r )

type ArgsArrowType r
  = ProtoArgsType ( here :: ArrowType, dom :: Record (ArgsType r), cod :: Record (ArgsType r) | r )

type ArgsDataType r
  = ProtoArgsType ( here :: DataType | r )

type ArgsHoleType r
  = ProtoArgsType ( here :: HoleType | r )

recType ::
  forall r a.
  Lacks "syn" r =>
  { arrow :: ProtoRec ArgsArrowType r a, data_ :: ProtoRec ArgsDataType r a, hole :: ProtoRec ArgsHoleType r a } ->
  ProtoRec ArgsType r a
recType rec args = case args.type_ of
  ArrowType arrow -> rec.arrow $ union { here: arrow, dom: argsArrowType_dom args, cod: ?args } args
  DataType data_ -> rec.data_ $ union { data_ } args
  HoleType hole -> rec.hole $ union { hole } args

argsArrowType_dom :: forall r. Lacks "syn" r => Record (ArgsArrowType r) -> Record (ArgsType r)
argsArrowType_dom args = 
  ?a
  -- delete (Proxy :: Proxy "arrow")
  -- >>> delete (Proxy :: Proxy "dom")
  -- >>> delete (Proxy :: Proxy "cod")
  -- >>> insert (Proxy :: Proxy "type_") args.arrow.dom
  -- $ args 

-- | recTerm
type ProtoArgsTerm r
  = ProtoArgs ( term :: Term | r )

type ArgsTerm r
  = ProtoArgsTerm (| r )

type ArgsLam r
  = ProtoArgsTerm ( lam :: Lam | r )

type ArgsNeu r
  = ProtoArgsTerm ( neu :: Neu | r )

type ArgsLet r
  = ProtoArgsTerm ( let_ :: Let | r )

type ArgsBuf r
  = ProtoArgsTerm ( buf :: Buf | r )

type ArgsData r
  = ProtoArgsTerm ( data_ :: Data | r )

type ArgsMatch r
  = ProtoArgsTerm ( match :: Match | r )

type ArgsHole r
  = ProtoArgsTerm ( hole :: Hole | r )

recTerm ::
  forall r a.
  Lacks "syn" r =>
  { lam :: ProtoRec ArgsLam r a, neu :: ProtoRec ArgsNeu r a, let_ :: ProtoRec ArgsLet r a, buf :: ProtoRec ArgsBuf r a, data_ :: ProtoRec ArgsData r a, match :: ProtoRec ArgsMatch r a, hole :: ProtoRec ArgsHole r a } ->
  ProtoRec ArgsTerm r a
recTerm rec args = case args.term of
  Lam lam -> rec.lam $ union { lam } args
  Neu neu -> rec.neu $ union { neu } args
  Let let_ -> rec.let_ $ union { let_ } args
  Buf buf -> rec.buf $ union { buf } args
  Data data_ -> rec.data_ $ union { data_ } args
  Match match -> rec.match $ union { match } args
  Hole hole -> rec.hole $ union { hole } args

type ProtoArgsArgItems r
  = ProtoArgs ( argItems :: List ArgItem | r )

type ArgsArgItems r
  = ProtoArgsArgItems (| r )

type ArgsArgItem r
  = ProtoArgsArgItems ( i :: Int, argItem :: ArgItem | r )

recArgItems ::
  forall r a.
  Lacks "syn" r =>
  { argItem :: ProtoRec ArgsArgItem r a } ->
  ProtoRec ArgsArgItems r (List a)
recArgItems rec args = mapWithIndex (\i argItem -> rec.argItem $ union {i, argItem} args) args.argItems
-- $ union { i, argItem } args) args.argItems

-- | recSumItems
type ProtoArgsSumItems r
  = ProtoArgs ( sumItems :: List SumItem | r )

type ArgsSumItems r
  = ProtoArgsSumItems (| r )

type ArgsSumItem r
  = ProtoArgsSumItems ( i :: Int, sumItem :: SumItem | r )

recSumItems ::
  forall r a.
  Lacks "syn" r =>
  { sumItem :: ProtoRec ArgsSumItem r a } ->
  ProtoRec ArgsSumItems r (List a)
recSumItems rec args =
  mapWithIndex
    (\i sumItem -> rec.sumItem $ union { i, sumItem } args)
    args.sumItems

-- | recCaseItem
type ProtoArgsCaseItems r
  = ProtoArgs ( caseItems :: List CaseItem | r )

type ArgsCaseItems r
  = ProtoArgsCaseItems (| r )

type ArgsCaseItem r
  = ProtoArgsCaseItems ( i :: Int, caseItem :: CaseItem | r )

recCaseItems ::
  forall r a.
  Lacks "syn" r =>
  { caseItem :: ProtoRec ArgsCaseItem r a } ->
  ProtoRec ArgsCaseItems r (List a)
recCaseItems rec args =
  mapWithIndex
    (\i caseItem -> rec.caseItem $ union { i, caseItem } args)
    args.caseItems

-- | recParamItems
type ProtoArgsParamItems r
  = ProtoArgs ( paramItems :: List ParamItem | r )

type ArgsParamItems r
  = ProtoArgsParamItems (| r )

type ArgsParamItem r
  = ProtoArgsParamItems ( i :: Int, paramItem :: ParamItem | r )

recParamItems ::
  forall r a.
  Lacks "syn" r =>
  { paramItem :: ProtoRec ArgsParamItem r a } ->
  ProtoRec ArgsParamItems r (List a)
recParamItems rec args =
  mapWithIndex
    (\i paramItem -> rec.paramItem $ union { i, paramItem } args)
    args.paramItems

-- | recTermBindItems
type ProtoArgsTermBindItems r
  = ProtoArgs ( termBindItems :: List TermBindItem | r )

type ArgsTermBindItems r
  = ProtoArgsTermBindItems (| r )

type ArgsTermBindItem r
  = ProtoArgsTermBindItems ( i :: Int, termBindItem :: TermBindItem | r )

recTermBindItems ::
  forall r a.
  Lacks "syn" r =>
  { termBindItem :: ProtoRec ArgsTermBindItem r a } ->
  ProtoRec ArgsTermBindItems r (List a)
recTermBindItems rec args =
  mapWithIndex
    (\i termBindItem -> rec.termBindItem $ union { i, termBindItem } args)
    args.termBindItems

-- | recTermBind
type ArgsTermBind r
  = ProtoArgs ( termBind :: TermBind | r )

recTermBind ::
  forall r a.
  Lacks "syn" r =>
  { termBind :: ProtoRec ArgsTermBind r a } ->
  ProtoRec ArgsTermBind r a
recTermBind rec = rec.termBind

-- | recTypeBind
type ArgsTypeBind r
  = ProtoArgs ( typeBind :: TypeBind | r)

recTypeBind ::
  forall r a.
  Lacks "syn" r =>
  { typeBind :: ProtoRec ArgsTypeBind r a } ->
  ProtoRec ArgsTypeBind r a
recTypeBind rec = rec.typeBind

-- | recTypeId
type ArgsTypeId r
  = ProtoArgs ( typeId :: TypeId | r) 

recTypeId ::
  forall r a.
  Lacks "syn" r =>
  { typeId :: ProtoRec ArgsTypeId r a } ->
  ProtoRec ArgsTypeId r a
recTypeId rec = rec.typeId

-- | recTermId
type ArgsTermId r
  = ProtoArgs ( termId :: TermId | r )

recTermId ::
  forall r a.
  Lacks "syn" r =>
  { termId :: ProtoRec ArgsTermId r a } ->
  ProtoRec ArgsTermId r a
recTermId rec = rec.termId
