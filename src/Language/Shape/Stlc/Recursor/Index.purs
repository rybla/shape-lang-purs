module Language.Shape.Stlc.Recursor.Index where

import Data.Tuple.Nested
import Language.Shape.Stlc.Context
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Prim.Row
import Record
import Data.List (List(..), foldl, foldr, snoc)
import Data.List.Unsafe (index')
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

visitIxDown :: Visit -> IxDown -> Visit
visitIxDown = undefined

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
  = Rec.ArgsArrowType (ProtoArgsType ( dom :: Visit, cod :: Visit ) r)

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
    { arrow: rec.arrow <<< modifyHetero _argsIx (\argsIx@{ visit } -> union { dom: visitIxStep visit ixStepArrowType.dom, cod: visitIxStep visit ixStepArrowType.cod } argsIx)
    , data_: rec.data_
    , hole: rec.hole
    }

-- -- | recTerm
type ProtoArgsTerm r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsTerm r
  = Rec.ArgsTerm (ProtoArgsTerm () r)

type ArgsLam r
  = Rec.ArgsLam (ProtoArgsTerm ( termBind :: Visit, body :: Visit ) r)

type ArgsNeu r
  = Rec.ArgsNeu (ProtoArgsTerm ( termId :: Visit, argItems :: Visit ) r)

type ArgsLet r
  = Rec.ArgsLet (ProtoArgsTerm ( termBind :: Visit, type :: Visit, term :: Visit, body :: Visit ) r)

type ArgsBuf r
  = Rec.ArgsBuf (ProtoArgsTerm ( term :: Visit, body :: Visit ) r)

type ArgsData r
  = Rec.ArgsData (ProtoArgsTerm ( typeBind :: Visit, sum :: Visit, body :: Visit ) r)

type ArgsMatch r
  = Rec.ArgsMatch (ProtoArgsTerm ( term :: Visit, caseItems :: Visit ) r)

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
    { lam: rec.lam <<< modifyHetero _argsIx (\argsIx@{ visit } -> union { termBind: visitIxStep visit ixStepLam.termBind, body: visitIxStep visit ixStepLam.body } argsIx)
    , neu: rec.neu <<< modifyHetero _argsIx (\argsIx@{ visit } -> union { termId: visitIxStep visit ixStepNeu.termId, argItems: visitIxStep visit ixStepNeu.argItems } argsIx)
    , let_: rec.let_ <<< modifyHetero _argsIx (\argsIx@{ visit } -> union { termBind: visitIxStep visit ixStepLet.termBind, type: visitIxStep visit ixStepLet.type_, term: visitIxStep visit ixStepLet.term, body: visitIxStep visit ixStepLet.body } argsIx)
    , buf: rec.buf <<< modifyHetero _argsIx (\argsIx@{ visit } -> union { term: visitIxStep visit ixStepBuf.term, body: visitIxStep visit ixStepBuf.body } argsIx)
    , data_: rec.data_ <<< modifyHetero _argsIx (\argsIx@{ visit } -> union { typeBind: visitIxStep visit ixStepData.typeBind, sum: visitIxStep visit ixStepData.sum, body: visitIxStep visit ixStepData.body } argsIx)
    , match: rec.match <<< modifyHetero _argsIx (\argsIx@{ visit } -> union { term: visitIxStep visit ixStepMatch.term, caseItems: visitIxStep visit ixStepMatch.caseItems } argsIx)
    , hole: rec.hole
    }

-- -- | recArgItems
-- type ProtoArgsArgItems r1 r2
--   = ProtoArgs r1 r2
-- type ArgsArgItems r
--   = Rec.ArgsArgItems (ProtoArgsArgItems () r)
-- type ArgsArgItemsCons r
--   = Rec.ArgsArgItemsCons (ProtoArgsArgItems ( argItem :: Visit, argItems :: Visit ) r)
-- type ArgsArgItemsNil r
--   = Rec.ArgsArgItemsNil (ProtoArgsArgItems () r)
-- recArgItems ::
--   forall r a.
--   Lacks "argsSyn" r =>
--   Lacks "argsCtx" r =>
--   Lacks "argsIx" r =>
--   { cons :: ProtoRec ArgsArgItemsCons r a, nil :: ProtoRec ArgsArgItemsNil r a } ->
--   ProtoRec ArgsArgItems r a
-- recArgItems rec =
--   Rec.recArgItems
--     { cons: rec.cons <<< modifyHetero _argsIx (\argsIx@{ visit } -> union { argItem: visitIxStep visit ixStepArgItems.argItem, argItems: visitIxStep visit ixStepArgItems.argItems } argsIx)
--     , nil: rec.nil
--     }
-- | recArgItems
type ProtoArgsArgItems r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsArgItems r
  = Rec.ArgsArgItems (ProtoArgsArgItems () r)

type ArgsArgItem r
  = Rec.ArgsArgItem (ProtoArgsArgItems ( argItems :: List Visit, argItem :: Visit ) r)

recArgItems ::
  forall r a.
  Lacks "argsSyn" r =>
  Lacks "argsCtx" r =>
  Lacks "argsIx" r =>
  { argItem :: ProtoRec ArgsArgItem r a } ->
  ProtoRec ArgsArgItems r (List a)
recArgItems rec =
  Rec.recArgItems { argItem: \args@{ argsSyn, argsIx } -> rec.argItem $ modifyHetero _argsIx (union { argItem: index' argsIx.argItems argsSyn.i }) args }
    <<< \args@{ argsSyn, argsIx } ->
        modifyHetero _argsIx
          ( union
              { argItems:
                  ( foldl
                        (\{ visit, argItems } _ -> { visit: visitIxStep visit ixStepList.head, argItems: snoc argItems (visitIxStep visit ixStepList.tail) })
                        { visit: argsIx.visit, argItems: mempty }
                        argsSyn.argItems
                    )
                    .argItems
              }
          )
          args

-- | recSumItems
type ProtoArgsSumItems r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsSumItems r
  = Rec.ArgsSumItems (ProtoArgsSumItems () r)

type ArgsSumItem r
  = Rec.ArgsSumItem (ProtoArgsSumItems ( sumItems :: List Visit, sumItem :: Visit, termBind :: Visit, paramItems :: Visit ) r)

recSumItems ::
  forall r a.
  Lacks "argsSyn" r =>
  Lacks "argsCtx" r =>
  Lacks "argsIx" r =>
  { sumItem :: ProtoRec ArgsSumItem r a } ->
  ProtoRec ArgsSumItems r (List a)
recSumItems rec =
  Rec.recSumItems
    { sumItem:
        \args@{ argsSyn, argsIx } ->
          let
            sumItem = index' argsIx.sumItems argsSyn.i

            termBind = visitIxStep sumItem ixStepSumItem.termBind

            paramItems = visitIxStep sumItem ixStepSumItem.paramItems
          in
            rec.sumItem $ modifyHetero _argsIx (union { sumItem, termBind, paramItems }) args
    }
    <<< \args@{ argsSyn, argsIx } ->
        modifyHetero _argsIx
          ( union
              { sumItems:
                  ( foldl
                        (\{ visit, sumItems } _ -> { visit: visitIxStep visit ixStepList.head, sumItems: snoc sumItems (visitIxStep visit ixStepList.tail) })
                        { visit: argsIx.visit, sumItems: mempty }
                        argsSyn.sumItems
                    )
                    .sumItems
              }
          )
          args

-- | recCaseItems
type ProtoArgsCaseItems r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsCaseItems r
  = Rec.ArgsCaseItems (ProtoArgsCaseItems () r)

type ArgsCaseItem r
  = Rec.ArgsCaseItem (ProtoArgsCaseItems ( caseItems :: List Visit, caseItem :: Visit, termBindItems :: Visit, body :: Visit ) r)

recCaseItems ::
  forall r a.
  Lacks "argsSyn" r =>
  Lacks "argsCtx" r =>
  Lacks "argsIx" r =>
  { caseItem :: ProtoRec ArgsCaseItem r a } ->
  ProtoRec ArgsCaseItems r (List a)
recCaseItems rec =
  Rec.recCaseItems
    { caseItem:
        \args@{ argsSyn, argsIx } ->
          let
            caseItem = index' argsIx.caseItems argsSyn.i

            termBindItems = visitIxStep caseItem ixStepCaseItem.termBindItems

            body = visitIxStep caseItem ixStepCaseItem.body
          in
            rec.caseItem $ modifyHetero _argsIx (union { caseItem, termBindItems, body }) args
    }
    <<< \args@{ argsSyn, argsIx } ->
        modifyHetero _argsIx
          ( union
              { caseItems:
                  ( foldl
                        (\{ visit, caseItems } _ -> { visit: visitIxStep visit ixStepList.head, caseItems: snoc caseItems (visitIxStep visit ixStepList.tail) })
                        { visit: argsIx.visit, caseItems: mempty }
                        argsSyn.caseItems
                    )
                    .caseItems
              }
          )
          args

-- | recParamItems
type ProtoArgsParamItems r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsParamItems r
  = Rec.ArgsParamItems (ProtoArgsParamItems () r)

type ArgsParamItem r
  = Rec.ArgsParamItem (ProtoArgsParamItems ( paramItems :: List Visit, param :: Visit, type_ :: Visit ) r)

recParamItems ::
  forall r a.
  Lacks "argsSyn" r =>
  Lacks "argsCtx" r =>
  Lacks "argsIx" r =>
  { paramItem :: ProtoRec ArgsParamItem r a } ->
  ProtoRec ArgsParamItems r (List a)
recParamItems rec =
  -- Rec.recParamItems { paramItem:\args@{ argsSyn, argsIx } -> rec.param $ modifyHetero _argsIx (union { paramItem:index' argsIx.paramItems argsSyn.i }) args }
  Rec.recParamItems
    { paramItem:
        \args@{ argsSyn, argsIx } ->
          let
            param = index' argsIx.paramItems argsSyn.i

            type_ = visitIxStep param ixStepParamItem.type_
          in
            rec.paramItem $ modifyHetero _argsIx (union { param, type_ }) args
    }
    <<< \args@{ argsSyn, argsIx } ->
        modifyHetero _argsIx
          -- TODO: same fixes as to recCaseItems
          ( union
              { paramItems:
                  ( foldl
                        (\{ visit, paramItems } _ -> { visit: visitIxStep visit ixStepList.head, paramItems: snoc paramItems (visitIxStep visit ixStepList.tail) })
                        { visit: argsIx.visit, paramItems: mempty }
                        argsSyn.paramItems
                    )
                    .paramItems
              }
          )
          args

-- | recTermBindItems
type ProtoArgsTermBindItems r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsTermBindItems r
  = Rec.ArgsTermBindItems (ProtoArgsTermBindItems () r)

type ArgsTermBindItem r
  = Rec.ArgsTermBindItem (ProtoArgsTermBindItems ( termBindItems :: List Visit, termBindItem :: Visit, termBind :: Visit ) r)

recTermBindItems ::
  forall r a.
  Lacks "argsSyn" r =>
  Lacks "argsCtx" r =>
  Lacks "argsIx" r =>
  { termBindItem :: ProtoRec ArgsTermBindItem r a } ->
  ProtoRec ArgsTermBindItems r (List a)
recTermBindItems rec =
  Rec.recTermBindItems
    { termBindItem:
        \args@{ argsSyn, argsIx } ->
          let
            visit = index' argsIx.termBindItems argsSyn.i
          in
            rec.termBindItem $ modifyHetero _argsIx (union { termBindItem: visit, termBind: visitIxStep visit ixStepTermBindItem.termBind }) args
    }
    <<< \args@{ argsSyn, argsIx } ->
        modifyHetero _argsIx
          ( union
              { termBindItems:
                  ( foldl
                        (\{ visit, termBindItems } _ -> { visit: visitIxStep visit ixStepList.head, termBindItems: snoc termBindItems (visitIxStep visit ixStepList.tail) })
                        { visit: argsIx.visit, termBindItems: mempty }
                        argsSyn.termBindItems
                    )
                    .termBindItems
              }
          )
          args

-- | recTermBind
type ArgsTermBind r
  = Rec.ArgsTermBind (ProtoArgs () r)

recTermBind ::
  forall r a.
  Lacks "argsSyn" r =>
  Lacks "argsCtx" r =>
  Lacks "argsIx" r =>
  { termBind :: ProtoRec ArgsTermBind r a } ->
  ProtoRec ArgsTermBind r a
recTermBind rec = Rec.recTermBind { termBind: rec.termBind }

-- | recTypeBind
type ArgsTypeBind r
  = Rec.ArgsTypeBind (ProtoArgs () r)

recTypeBind ::
  forall r a.
  Lacks "argsSyn" r =>
  Lacks "argsCtx" r =>
  Lacks "argsIx" r =>
  { typeBind :: ProtoRec ArgsTypeBind r a } ->
  ProtoRec ArgsTypeBind r a
recTypeBind rec = Rec.recTypeBind { typeBind: rec.typeBind }
