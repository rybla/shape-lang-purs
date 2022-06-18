module Language.Shape.Stlc.Recursor.Index where

import Data.Tuple.Nested
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Recursor.Proxy
import Language.Shape.Stlc.Syntax
import Prelude
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (over, unwrap, wrap)
import Language.Shape.Stlc.Recursor.Context as Rec
import Language.Shape.Stlc.Recursor.Record as RH
import Partial.Unsafe (unsafeCrashWith)
import Prim (Boolean, Int, Record, Row)
import Prim as Prim
import Prim.Row (class Lacks)
import Record as R
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe (fromJust)

type Visit
  = { ix :: Maybe IxUp, csr :: Maybe IxDown }

visitVia :: forall r. IxStep -> { visit :: Visit | r } -> { visit :: Visit | r }
visitVia ixStep args@{ visit: { ix, csr } } =
  args
    { visit =
      { ix: over wrap (Cons ixStep) <$> ix
      , csr:
          do
            ixSteps <- unwrap <$> csr
            case ixSteps of
              Cons ixStep' ixSteps' -> if ixStep == ixStep' then Just (wrap ixSteps') else Nothing
              Nil -> Nothing
      }
    }

visitItemsVia :: forall r. IxStep -> List { visit :: Visit | r } -> List { visit :: Visit | r }
visitItemsVia ixStep argss = go (visitVia ixStep <$> argss)
  where
  go Nil = Nil

  go (Cons args argss) =
    Cons
      (visitVia ixStepList.head args)
      (go (visitVia ixStepList.tail <$> argss))

nonVisit :: Visit
nonVisit = { ix: Nothing, csr: Nothing }

nilVisit :: Maybe IxDown -> Visit
nilVisit csr = { ix: Just (nilIxUp), csr }

dontVisit :: forall r. { visit :: Visit | r } -> { visit :: Visit | r }
dontVisit args = args { visit = nonVisit }

isSelected :: Visit -> Boolean
isSelected visit = visit.csr == Just nilIxDown

isSelectionAncestor :: Visit -> Boolean
isSelectionAncestor visit = isJust visit.csr

-- | recType
type ArgsType r
  = Rec.ArgsType ( visit :: Visit | r )

type ArgsArrowType r rType
  = Rec.ArgsArrowType ( visit :: Visit | r ) rType

type ArgsDataType r rTypeId
  = Rec.ArgsDataType ( visit :: Visit | r ) rTypeId

type ArgsHoleType r rHoleId
  = Rec.ArgsHoleType ( visit :: Visit | r ) rHoleId

recType ::
  forall r a.
  Lacks "type_" r =>
  { arrowType :: Record (ArgsArrowType r (ArgsType r)) -> a
  , dataType :: Record (ArgsDataType r (ArgsTypeId r)) -> a
  , holeType :: Record (ArgsHoleType r (ArgsHoleId r)) -> a
  } ->
  Record (ArgsType r) -> a
recType rec =
  Rec.recType
    { arrowType:
        \args ->
          rec.arrowType
            args
              { dom = visitVia ixStepArrowType.dom args.dom
              , cod = visitVia ixStepArrowType.cod args.cod
              }
    , dataType:
        \args ->
          rec.dataType
            args
              { typeId = args.typeId }
    , holeType:
        \args ->
          rec.holeType
            args
              { holeId = args.holeId }
    }

-- | recTerm
type ArgsTerm r
  = Rec.ArgsTerm ( visit :: Visit | r )

type ArgsLam r rTermBind rTerm
  = Rec.ArgsLam ( visit :: Visit | r ) rTermBind rTerm

type ArgsNeu r rTermId rArgItems
  = Rec.ArgsNeu ( visit :: Visit | r ) rTermId rArgItems

type ArgsLet r termBind rType rTerm
  = Rec.ArgsLet ( visit :: Visit | r ) termBind rType rTerm

type ArgsBuf r rType rTerm
  = Rec.ArgsBuf ( visit :: Visit | r ) rType rTerm

type ArgsData r rTypeBind rTerm rSumItems
  = Rec.ArgsData ( visit :: Visit | r ) rTypeBind rTerm rSumItems

type ArgsMatch r rTypeId rTerm rCaseItems
  = Rec.ArgsMatch ( visit :: Visit | r ) rTypeId rTerm rCaseItems

type ArgsHole r
  = Rec.ArgsHole ( visit :: Visit | r )

recTerm ::
  forall r a.
  Lacks "term" r =>
  Lacks "alpha" r =>
  { lam :: Record (ArgsLam r (ArgsTermBind r) (ArgsTerm r)) -> a
  , neu :: Record (ArgsNeu r (ArgsTermId r) (ArgsArgItem r)) -> a
  , let_ :: Record (ArgsLet r (ArgsTermBind r) (ArgsType r) (ArgsTerm r)) -> a
  , buf :: Record (ArgsBuf r (ArgsType r) (ArgsTerm r)) -> a
  , data_ :: Record (ArgsData r (ArgsTypeBind r) (ArgsSumItem r) (ArgsTerm r)) -> a
  , match :: Record (ArgsMatch r (ArgsTypeId r) (ArgsTerm r) (ArgsCaseItem r)) -> a
  , hole :: Record (ArgsHole r) -> a
  } ->
  Record (ArgsTerm r) -> a
recTerm rec =
  Rec.recTerm
    { lam:
        \args ->
          rec.lam
            args
              { termBind = visitVia ixStepLam.termBind args.termBind
              , body = visitVia ixStepLam.body args.body
              }
    , neu:
        \args ->
          rec.neu
            args
              { termId = dontVisit args.termId -- CHANGE visitVia ixStepNeu.termId args.termId
              , argItems = visitItemsVia ixStepNeu.argItems args.argItems
              }
    , let_:
        \args ->
          rec.let_
            args
              { termBind = visitVia ixStepLet.termBind args.termBind
              , sign = visitVia ixStepLet.sign args.sign
              , impl = visitVia ixStepLet.impl args.impl
              , body = visitVia ixStepLet.body args.body
              }
    , buf:
        \args ->
          rec.buf
            args
              { sign = visitVia ixStepBuf.sign args.sign
              , impl = visitVia ixStepBuf.impl args.impl
              , body = visitVia ixStepBuf.body args.body
              }
    , data_:
        \args ->
          rec.data_
            args
              { typeBind = visitVia ixStepData.typeBind args.typeBind
              , sumItems = visitItemsVia ixStepData.sumItems args.sumItems
              , body = visitVia ixStepData.body args.body
              }
    , match:
        \args ->
          rec.match
            args
              { typeId = args.typeId { visit = nonVisit }
              , term = visitVia ixStepMatch.term args.term
              , caseItems = visitItemsVia ixStepMatch.caseItems args.caseItems
              }
    , hole: rec.hole
    }

-- | recArgItem
type ArgsArgItem r
  = Rec.ArgsArgItem ( visit :: Visit | r )

type ArgsArgItem_ArgItem r rTerm
  = Rec.ArgsArgItem_ArgItem ( visit :: Visit | r ) rTerm

recArgItem ::
  forall r a.
  Lacks "argItem" r =>
  Lacks "gamma" r =>
  Lacks "doms" r =>
  Lacks "cod" r =>
  { argItem :: Record (ArgsArgItem_ArgItem r (ArgsTerm r)) -> a } ->
  Record (ArgsArgItem r) -> a
recArgItem rec =
  Rec.recArgItem
    { argItem:
        \args ->
          rec.argItem
            args
              { term = visitVia ixStepArgItem.term args.term }
    }

-- | recSumItems
type ArgsSumItem r
  = Rec.ArgsSumItem ( visit :: Visit | r )

type ArgsSumItem_SumItem r rTermBind rParamItems
  = Rec.ArgsSumItem_SumItem ( visit :: Visit | r ) rTermBind rParamItems

recSumItem ::
  forall r a.
  Lacks "sumItem" r =>
  { sumItem :: Record (ArgsSumItem_SumItem r (ArgsTermBind r) (ArgsParamItem r)) -> a } ->
  Record (ArgsSumItem r) -> a
recSumItem rec =
  Rec.recSumItem
    { sumItem:
        \args ->
          rec.sumItem
            args
              { termBind = visitVia ixStepSumItem.termBind args.termBind
              , paramItems = visitItemsVia ixStepSumItem.paramItems args.paramItems
              }
    }

-- | recCaseItem
type ArgsCaseItem r
  = Rec.ArgsCaseItem ( visit :: Visit | r )

type ArgsCaseItem_CaseItem r rTermBindItem rTerm
  = Rec.ArgsCaseItem_CaseItem ( visit :: Visit | r ) rTermBindItem rTerm

recCaseItem ::
  forall r a.
  Lacks "caseItem" r =>
  Lacks "alpha" r =>
  Lacks "typeId" r =>
  { caseItem :: Record (ArgsCaseItem_CaseItem r (ArgsTermBindItem r) (ArgsTerm r)) -> a } ->
  Record (ArgsCaseItem r) -> a
recCaseItem rec =
  Rec.recCaseItem
    { caseItem:
        \args ->
          rec.caseItem
            args
              { termBindItems = visitItemsVia ixStepCaseItem.termBindItems args.termBindItems
              , body = visitVia ixStepCaseItem.body args.body
              }
    }

-- | recParamItems
type ArgsParamItem r
  = Rec.ArgsParamItem ( visit :: Visit | r )

type ArgsParamItem_ParamItem r rType
  = Rec.ArgsParamItem_ParamItem ( visit :: Visit | r ) rType

recParamItem ::
  forall r a.
  Lacks "paramItem" r =>
  { paramItem :: Record (ArgsParamItem_ParamItem r (ArgsType r)) -> a } ->
  Record (ArgsParamItem r) -> a
recParamItem rec =
  Rec.recParamItem
    { paramItem:
        \args ->
          rec.paramItem
            args
              { type_ = visitVia ixStepParamItem.type_ args.type_ }
    }

-- | recTermBindItems
type ArgsTermBindItem r
  = Rec.ArgsTermBindItem ( visit :: Visit | r )

type ArgsTermBindItem_TermBindItem r rTermBind
  = Rec.ArgsTermBindItem_TermBindItem ( visit :: Visit | r ) rTermBind

recTermBindItem ::
  forall r a.
  Lacks "termBindItem" r =>
  { termBindItem :: Record (ArgsTermBindItem_TermBindItem r (ArgsTermBind r)) -> a } ->
  Record (ArgsTermBindItem r) -> a
recTermBindItem rec =
  Rec.recTermBindItem
    { termBindItem:
        \args ->
          rec.termBindItem
            args
              { termBind = visitVia ixStepTermBindItem.termBind args.termBind }
    }

-- | recTypeBind
type ArgsTypeBind r
  = Rec.ArgsTypeBind ( visit :: Visit | r )

type ArgsTypeBind_TypeBind r rTypeId
  = Rec.ArgsTypeBind_TypeBind ( visit :: Visit | r ) rTypeId

recTypeBind ::
  forall r a.
  Lacks "typeBind" r =>
  { typeBind :: Record (ArgsTypeBind_TypeBind r (ArgsTypeId r)) -> a } ->
  Record (ArgsTypeBind r) -> a
recTypeBind rec =
  Rec.recTypeBind
    { typeBind:
        \args ->
          rec.typeBind
            args
              { typeId = args.typeId { visit = nonVisit } }
    }

-- | recTermBind
type ArgsTermBind r
  = Rec.ArgsTermBind ( visit :: Visit | r )

type ArgsTermBind_TermBind r rTermId
  = Rec.ArgsTermBind_TermBind ( visit :: Visit | r ) rTermId

recTermBind ::
  forall r a.
  Lacks "termBind" r =>
  { termBind :: Record (ArgsTermBind_TermBind r (ArgsTermId r)) -> a } ->
  Record (ArgsTermBind r) -> a
recTermBind rec =
  Rec.recTermBind
    { termBind:
        \args ->
          rec.termBind
            args
              { termId = args.termId { visit = nonVisit } }
    }

-- | recTypeId
type ArgsTypeId r
  = Rec.ArgsTypeId ( visit :: Visit | r )

-- | recTermId
type ArgsTermId r
  = Rec.ArgsTermId ( visit :: Visit | r )

-- | recHoleId 
type ArgsHoleId r
  = Rec.ArgsHoleId ( visit :: Visit | r )
