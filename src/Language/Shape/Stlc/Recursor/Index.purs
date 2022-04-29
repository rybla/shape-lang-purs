module Language.Shape.Stlc.Recursor.Index where

import Data.Tuple.Nested
import Language.Shape.Stlc.Context
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Prim.Row
import Record
import Data.List (List(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (over, unwrap, wrap)
import Language.Shape.Stlc.Recursor.Context as Rec
import Language.Shape.Stlc.Recursor.Record (modifyHetero)
import Partial.Unsafe (unsafeCrashWith)
import Prim as Prim
import Type.Proxy (Proxy(..))
import Undefined (undefined)

type Cursor
  = Maybe IxDown

type Visit
  = { ix :: IxUp, csr :: Cursor }

isHere :: Visit -> Boolean
isHere { csr } = csr == Just (wrap Nil)

visitIxStep :: Visit -> IxStep -> Visit
visitIxStep { ix, csr } ixStep =
  { ix: over wrap (Cons ixStep) ix
  , csr:
      do
        ixSteps <- unwrap <$> csr
        case ixSteps of
          Cons ixStep' ixSteps' -> if ixStep == ixStep' then Just (wrap ixSteps') else Nothing
          Nil -> Nothing
  }

-- | ProtoRec
type ProtoArgs r1 r2
  = ( argsIx :: Record ( visit :: Visit | r1 ) | r2 )

type ProtoRec args r a
  = Rec.ProtoRec args r a

_argsIx = Proxy :: Proxy "argsIx"

-- | recType
type ProtoArgsType r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsType r
  = Rec.ArgsType (ProtoArgsType () r)

type ArgsArrowType r
  = Rec.ArgsArrowType (ProtoArgsType ( visit_dom :: Visit, visit_cod :: Visit ) r)

type ArgsDataType r
  = Rec.ArgsDataType (ProtoArgsType () r)

type ArgsHoleType r
  = Rec.ArgsHoleType (ProtoArgsType () r)

recType ::
  forall r a.
  Lacks "argsSyn" r =>
  Lacks "argsCtx" r =>
  Lacks "argsIx" r =>
  { arrow :: ProtoRec ArgsArrowType r a, data_ :: ProtoRec ArgsDataType r a, hole :: ProtoRec ArgsHoleType r a } ->
  ProtoRec ArgsType r a
recType rec =
  Rec.recType
    { arrow: rec.arrow <<< modifyHetero _argsIx (\argsIx@{ visit } -> union { visit_dom: visitIxStep visit ixStepArrowType.dom, visit_cod: visitIxStep visit ixStepArrowType.cod } argsIx)
    , data_: rec.data_
    , hole: rec.hole
    }

-- -- | recTerm
type ProtoArgsTerm r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsTerm r
  = Rec.ArgsTerm (ProtoArgsTerm () r)

type ArgsLam r
  = Rec.ArgsLam (ProtoArgsTerm ( visit_termBind :: Visit, visit_body :: Visit ) r)

type ArgsNeu r
  = Rec.ArgsNeu (ProtoArgsTerm ( visit_termId :: Visit, visit_argItems :: Visit ) r)

type ArgsLet r
  = Rec.ArgsLet (ProtoArgsTerm ( visit_termBind :: Visit, visit_type :: Visit, visit_term :: Visit, visit_body :: Visit ) r)

type ArgsBuf r
  = Rec.ArgsBuf (ProtoArgsTerm ( visit_term :: Visit, visit_body :: Visit ) r)

type ArgsData r
  = Rec.ArgsData (ProtoArgsTerm ( visit_typeBind :: Visit, visit_sum :: Visit, visit_body :: Visit ) r)

type ArgsMatch r
  = Rec.ArgsMatch (ProtoArgsTerm ( visit_term :: Visit, visit_caseItems :: Visit ) r)

type ArgsHole r
  = Rec.ArgsHole (ProtoArgsTerm () r)

recTerm ::
  forall r a.
  Lacks "argsSyn" r =>
  Lacks "argsCtx" r =>
  Lacks "argsIx" r =>
  { lam :: ProtoRec ArgsLam r a, neu :: ProtoRec ArgsNeu r a, let_ :: ProtoRec ArgsLet r a, buf :: ProtoRec ArgsBuf r a, data_ :: ProtoRec ArgsData r a, match :: ProtoRec ArgsMatch r a, hole :: ProtoRec ArgsHole r a } ->
  ProtoRec ArgsTerm r a
recTerm rec =
  Rec.recTerm
    { lam: rec.lam <<< modifyHetero _argsIx (\argsIx@{ visit } -> union { visit_termBind: visitIxStep visit ixStepLam.termBind, visit_body: visitIxStep visit ixStepLam.body } argsIx)
    , neu: rec.neu <<< modifyHetero _argsIx (\argsIx@{ visit } -> union { visit_termId: visitIxStep visit ixStepNeu.termId, visit_argItems: visitIxStep visit ixStepNeu.argItems } argsIx)
    , let_: rec.let_ <<< modifyHetero _argsIx (\argsIx@{ visit } -> union { visit_termBind: visitIxStep visit ixStepLet.termBind, visit_type: visitIxStep visit ixStepLet.type_, visit_term: visitIxStep visit ixStepLet.term, visit_body: visitIxStep visit ixStepLet.body } argsIx)
    , buf: rec.buf <<< modifyHetero _argsIx (\argsIx@{ visit } -> union { visit_term: visitIxStep visit ixStepBuf.term, visit_body: visitIxStep visit ixStepBuf.body } argsIx)
    , data_: rec.data_ <<< modifyHetero _argsIx (\argsIx@{ visit } -> union { visit_typeBind: visitIxStep visit ixStepData.typeBind, visit_sum: visitIxStep visit ixStepData.sum, visit_body: visitIxStep visit ixStepData.body } argsIx)
    , match: rec.match <<< modifyHetero _argsIx (\argsIx@{ visit } -> union { visit_term: visitIxStep visit ixStepMatch.term, visit_caseItems: visitIxStep visit ixStepMatch.caseItems } argsIx)
    , hole: rec.hole
    }

-- | recArgItems
type ProtoArgsArgItems r1 r2
  = ProtoArgs r1 r2

type ArgsArgItems r
  = Rec.ArgsArgItems (ProtoArgsArgItems () r)

type ArgsArgItemsCons r
  = Rec.ArgsArgItemsCons (ProtoArgsArgItems ( visit_argItem :: Visit, visit_argItems :: Visit ) r)

type ArgsArgItemsNil r
  = Rec.ArgsArgItemsNil (ProtoArgsArgItems () r)

recArgItems ::
  forall r a.
  Lacks "argsSyn" r =>
  Lacks "argsCtx" r =>
  Lacks "argsIx" r =>
  { cons :: ProtoRec ArgsArgItemsCons r a, nil :: ProtoRec ArgsArgItemsNil r a } ->
  ProtoRec ArgsArgItems r a
recArgItems rec =
  Rec.recArgItems
    { cons: rec.cons <<< modifyHetero _argsIx (\argsIx@{ visit } -> union { visit_argItem: visitIxStep visit ixStepArgItems.argItem, visit_argItems: visitIxStep visit ixStepArgItems.argItems } argsIx)
    , nil: rec.nil
    }
