module Language.Shape.Stlc.Recursor.Action where

import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Recursor.Proxy
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Types
import Prelude
import Data.Newtype (over, unwrap, wrap)
import Language.Shape.Stlc.Metacontext (Metacontext(..), incrementIndentation, insertData, insertVar)
import Language.Shape.Stlc.Recursor.Metacontext as Rec
import Prim (Array, Record, Row)
import Prim as Prim
import Prim.Row (class Lacks)
import Record as R
import Undefined (undefined)

-- | recType
type ArgsType r
  = Rec.ArgsType ( | r )

type ArgsArrowType r rType
  = Rec.ArgsArrowType ( actions :: Array Action | r ) rType

type ArgsDataType r rTypeId
  = Rec.ArgsDataType ( actions :: Array Action | r ) rTypeId

type ArgsHoleType r rHoleId
  = Rec.ArgsHoleType ( actions :: Array Action | r ) rHoleId

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
            $ R.union { actions: [] }
                args
    , dataType:
        \args ->
          rec.dataType
            $ R.union { actions: [] }
                args
    , holeType:
        \args ->
          rec.holeType
            $ R.union { actions: [] }
                args
    }

-- | recTerm
type ArgsTerm r
  = Rec.ArgsTerm ( | r )

type ArgsLam r rTermBind rTerm
  = Rec.ArgsLam ( actions :: Array Action | r ) rTermBind rTerm

type ArgsNeu r rTermId rArgItems
  = Rec.ArgsNeu ( actions :: Array Action | r ) rTermId rArgItems

type ArgsLet r termBind rType rTerm
  = Rec.ArgsLet ( actions :: Array Action | r ) termBind rType rTerm

type ArgsBuf r rType rTerm
  = Rec.ArgsBuf ( actions :: Array Action | r ) rType rTerm

type ArgsData r rTypeBind rTerm rSumItems
  = Rec.ArgsData ( actions :: Array Action | r ) rTypeBind rTerm rSumItems

type ArgsMatch r rTypeId rTerm rCaseItems
  = Rec.ArgsMatch ( actions :: Array Action | r ) rTypeId rTerm rCaseItems

type ArgsHole r
  = Rec.ArgsHole ( actions :: Array Action | r )

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
            $ R.union { actions: [] }
                args
    , neu:
        \args ->
          rec.neu
            $ R.union { actions: [] }
                args
    , let_:
        \args ->
          rec.let_
            $ R.union { actions: [] }
                args
    , buf:
        \args ->
          rec.buf
            $ R.union { actions: [] }
                args
    , data_:
        \args ->
          rec.data_
            $ R.union { actions: [] }
                args
    , match:
        \args ->
          rec.match
            $ R.union { actions: [] }
                args
    , hole:
        \args ->
          rec.hole
            $ R.union { actions: [] }
                args
    }

-- | recArgItem
type ArgsArgItem r
  = Rec.ArgsArgItem ( | r )

type ArgsArgItem_ArgItem r rTerm
  = Rec.ArgsArgItem_ArgItem ( actions :: Array Action | r ) rTerm

recArgItem ::
  forall r a.
  Lacks "argItem" r =>
  Lacks "gamma" r =>
  Lacks "doms" r =>
  Lacks "cod" r =>
  { argItem :: Record (ArgsArgItem_ArgItem r (ArgsTerm r)) -> a } ->
  Record (ArgsArgItem r) -> a
recArgItem rec = Rec.recArgItem { argItem: \args -> rec.argItem $ R.union { actions: [] } args }

-- | recSumItem
type ArgsSumItem r
  = Rec.ArgsSumItem ( | r )

type ArgsSumItem_SumItem r rTermBind rParamItems
  = Rec.ArgsSumItem_SumItem ( actions :: Array Action | r ) rTermBind rParamItems

recSumItem ::
  forall r a.
  Lacks "sumItem" r =>
  { sumItem :: Record (ArgsSumItem_SumItem r (ArgsTermBind r) (ArgsParamItem r)) -> a } ->
  Record (ArgsSumItem r) -> a
recSumItem rec = Rec.recSumItem { sumItem: \args -> rec.sumItem $ R.union { actions: [] } args }

-- | recCaseItem
type ArgsCaseItem r
  = Rec.ArgsCaseItem ( | r )

type ArgsCaseItem_CaseItem r rTermBindItem rTerm
  = Rec.ArgsCaseItem_CaseItem ( actions :: Array Action | r ) rTermBindItem rTerm

recCaseItem ::
  forall r a.
  Lacks "caseItem" r =>
  Lacks "alpha" r =>
  Lacks "typeId" r =>
  { caseItem :: Record (ArgsCaseItem_CaseItem r (ArgsTermBindItem r) (ArgsTerm r)) -> a } ->
  Record (ArgsCaseItem r) -> a
recCaseItem rec = Rec.recCaseItem { caseItem: \args -> rec.caseItem $ R.union { actions: [] } args }

-- | recParamItems
type ArgsParamItem r
  = Rec.ArgsParamItem ( | r )

type ArgsParamItem_ParamItem r rType
  = Rec.ArgsParamItem_ParamItem ( actions :: Array Action | r ) rType

recParamItem ::
  forall r a.
  Lacks "paramItem" r =>
  { paramItem :: Record (ArgsParamItem_ParamItem r (ArgsType r)) -> a } ->
  Record (ArgsParamItem r) -> a
recParamItem rec = Rec.recParamItem { paramItem: \args -> rec.paramItem $ R.union { actions: [] } args }

-- | recTermBindItems
type ArgsTermBindItem r
  = Rec.ArgsTermBindItem ( | r )

type ArgsTermBindItem_TermBindItem r rTermBind
  = Rec.ArgsTermBindItem_TermBindItem ( actions :: Array Action | r ) rTermBind

recTermBindItem ::
  forall r a.
  Lacks "termBindItem" r =>
  { termBindItem :: Record (ArgsTermBindItem_TermBindItem r (ArgsTermBind r)) -> a } ->
  Record (ArgsTermBindItem r) -> a
recTermBindItem rec = Rec.recTermBindItem { termBindItem: \args -> rec.termBindItem $ R.union { actions: [] } args }

-- | recTypeBind
type ArgsTypeBind r
  = Rec.ArgsTypeBind ( | r )

type ArgsTypeBind_TypeBind r rTypeId
  = Rec.ArgsTypeBind_TypeBind ( actions :: Array Action | r ) rTypeId

-- | recTermBind
type ArgsTermBind r
  = Rec.ArgsTermBind ( | r )

type ArgsTermBind_TermBind r rTermId
  = Rec.ArgsTermBind_TermBind ( actions :: Array Action | r ) rTermId

-- | recTypeId
type ArgsTypeId r
  = Rec.ArgsTypeId ( | r )

type ArgsTypeId_TypeID r
  = Rec.ArgsTypeId ( actions :: Array Action | r )

-- | recTermId
type ArgsTermId r
  = Rec.ArgsTermId ( | r )

type ArgsTermId_TermId r
  = Rec.ArgsTermId ( actions :: Array Action | r )

-- | recHoleId 
type ArgsHoleId r
  = Rec.ArgsHoleId ( | r )

type ArgsHoleId_HoleId r
  = Rec.ArgsHoleId ( actions :: Array Action | r )
