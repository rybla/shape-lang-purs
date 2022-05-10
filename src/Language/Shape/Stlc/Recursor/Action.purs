module Language.Shape.Stlc.Recursor.Action where

import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Recursor.Proxy
import Language.Shape.Stlc.Syntax
import Prelude
import Language.Shape.Stlc.Types
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
  , neu :: Record (ArgsNeu r (ArgsTermId r) (ArgsArgItems r)) -> a
  , let_ :: Record (ArgsLet r (ArgsTermBind r) (ArgsType r) (ArgsTerm r)) -> a
  , buf :: Record (ArgsBuf r (ArgsType r) (ArgsTerm r)) -> a
  , data_ :: Record (ArgsData r (ArgsTypeBind r) (ArgsSumItems r) (ArgsTerm r)) -> a
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
          -- rec.match
          --   $ R.union { actions: [] }
          --       args
          undefined
    , hole:
        \args ->
          rec.hole
            $ R.union { actions: [] }
                args
    }

-- | recArgItems
type ArgsArgItems r
  = Rec.ArgsArgItems ( | r )

type ArgsArgItem r rTerm
  = Rec.ArgsArgItem ( actions :: Array Action | r ) rTerm

-- | recSumItems
type ArgsSumItems r
  = Rec.ArgsSumItems ( | r )

type ArgsSumItem r rTermBind rParamItems
  = Rec.ArgsSumItem ( actions :: Array Action | r ) rTermBind rParamItems

-- -- | recCaseItems
-- type ArgsCaseItems r
--   = Rec.ArgsCaseItems ( | r )

-- type ArgsCaseItem r rTermBindItems rTerm
--   = Rec.ArgsCaseItem ( actions :: Array Action | r ) rTermBindItems rTerm

-- | recCaseItem
type ArgsCaseItem r
  = Rec.ArgsCaseItem ( | r )

type ArgsCaseItem_CaseItem r rTermBindItems rTerm
  = Rec.ArgsCaseItem_CaseItem ( actions :: Array Action | r ) rTermBindItems rTerm

-- | recParamItems
type ArgsParamItems r
  = Rec.ArgsParamItems ( | r )

type ArgsParamItem r rType
  = Rec.ArgsParamItem ( actions :: Array Action | r ) rType

-- | recTermBindItems
type ArgsTermBindItems r
  = Rec.ArgsTermBindItems ( actions :: Array Action | r )

type ArgsTermBindItem r rTermBind
  = Rec.ArgsTermBindItem ( actions :: Array Action | r ) rTermBind

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
