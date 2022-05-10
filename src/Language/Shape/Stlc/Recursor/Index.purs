module Language.Shape.Stlc.Recursor.Index where

import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Recursor.Proxy
import Language.Shape.Stlc.Syntax
import Prelude

import Data.List (List(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (over, unwrap, wrap)
import Language.Shape.Stlc.Recursor.Context as Rec
import Language.Shape.Stlc.Recursor.Record as RH
import Prim (Boolean, Record, Row)
import Prim as Prim
import Prim.Row (class Lacks)
import Record as R
import Type.Proxy (Proxy(..))
import Undefined (undefined)

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

nonVisit :: Visit
nonVisit = { ix: Nothing, csr: Nothing }

nilVisit :: Maybe IxDown -> Visit
nilVisit csr = { ix: Just (nilIxUp), csr }

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
  , neu :: Record (ArgsNeu r (ArgsTermId r) (ArgsArgItems r)) -> a
  , let_ :: Record (ArgsLet r (ArgsTermBind r) (ArgsType r) (ArgsTerm r)) -> a
  , buf :: Record (ArgsBuf r (ArgsType r) (ArgsTerm r)) -> a
  , data_ :: Record (ArgsData r (ArgsTypeBind r) (ArgsSumItems r) (ArgsTerm r)) -> a
  , match :: Record (ArgsMatch r (ArgsTypeId r) (ArgsTerm r) (ArgsCaseItems r)) -> a
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
              { termId = visitVia ixStepNeu.termId args.termId
              , argItems = visitVia ixStepNeu.argItems args.argItems
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
              , sumItems = visitVia ixStepData.sumItems args.sumItems
              , body = visitVia ixStepData.body args.body
              }
    , match:
        \args ->
          rec.match
            args
              { typeId = args.typeId { visit = nonVisit }
              , term = visitVia ixStepMatch.term args.term
              , caseItems = visitVia ixStepMatch.caseItems args.caseItems
              }
    , hole: rec.hole
    }

-- | recArgItems
type ArgsArgItems r
  = Rec.ArgsArgItems ( visit :: Visit | r )

type ArgsArgItem r rTerm
  = Rec.ArgsArgItem ( visit :: Visit | r ) rTerm

-- | recSumItems
type ArgsSumItems r
  = Rec.ArgsSumItems ( visit :: Visit | r )

type ArgsSumItem r rTermBind rParamItems
  = Rec.ArgsSumItem ( visit :: Visit | r ) rTermBind rParamItems

-- | recCaseItems
type ArgsCaseItems r
  = Rec.ArgsCaseItems ( visit :: Visit | r )

type ArgsCaseItem r rTermBindItems rTerm
  = Rec.ArgsCaseItem ( visit :: Visit | r ) rTermBindItems rTerm

-- | recParamItems
type ArgsParamItems r
  = Rec.ArgsParamItems ( visit :: Visit | r )

type ArgsParamItem r rType
  = Rec.ArgsParamItem ( visit :: Visit | r ) rType

-- | recTermBindItems
type ArgsTermBindItems r
  = Rec.ArgsTermBindItems ( visit :: Visit | r )

type ArgsTermBindItem r rTermBind
  = Rec.ArgsTermBindItem ( visit :: Visit | r ) rTermBind

-- | recTypeBind
type ArgsTypeBind r
  = Rec.ArgsTypeBind ( visit :: Visit | r )

type ArgsTypeBind_TypeBind r rTypeId
  = Rec.ArgsTypeBind_TypeBind ( visit :: Visit | r ) rTypeId

-- | recTermBind
type ArgsTermBind r
  = Rec.ArgsTermBind ( visit :: Visit | r )

type ArgsTermBind_TermBind r rTermId
  = Rec.ArgsTermBind_TermBind ( visit :: Visit | r ) rTermId

-- | recTypeId
type ArgsTypeId r
  = Rec.ArgsTypeId ( visit :: Visit | r )

-- | recTermId
type ArgsTermId r
  = Rec.ArgsTermId ( visit :: Visit | r )

-- | recHoleId 
type ArgsHoleId r
  = Rec.ArgsHoleId ( visit :: Visit | r )
