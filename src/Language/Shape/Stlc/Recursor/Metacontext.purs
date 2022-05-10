module Language.Shape.Stlc.Recursor.Metacontext where

import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Recursor.Proxy
import Language.Shape.Stlc.Syntax
import Prelude
import Data.Newtype (over, unwrap, wrap)
import Language.Shape.Stlc.Metacontext (Metacontext(..), incrementIndentation, insertData, insertVar)
import Language.Shape.Stlc.Recursor.Index as Rec
import Prim (Record, Row)
import Prim as Prim
import Prim.Row (class Lacks)
import Record as R
import Undefined (undefined)

mapArgsMeta :: forall r. (Metacontext -> Metacontext) -> { meta :: Metacontext | r } -> { meta :: Metacontext | r }
mapArgsMeta f args = args { meta = f args.meta }

-- | recType
type ArgsType r
  = Rec.ArgsType ( meta :: Metacontext | r )

type ArgsArrowType r rType
  = Rec.ArgsArrowType ( meta :: Metacontext | r ) rType

type ArgsDataType r rTypeId
  = Rec.ArgsDataType ( meta :: Metacontext | r ) rTypeId

type ArgsHoleType r rHoleId
  = Rec.ArgsHoleType ( meta :: Metacontext | r ) rHoleId

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
              { dom = args.dom
              , cod = incrementIndentation `mapArgsMeta` args.cod
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
  = Rec.ArgsTerm ( meta :: Metacontext | r )

type ArgsLam r rTermBind rTerm
  = Rec.ArgsLam ( meta :: Metacontext | r ) rTermBind rTerm

type ArgsNeu r rTermId rArgItems
  = Rec.ArgsNeu ( meta :: Metacontext | r ) rTermId rArgItems

type ArgsLet r termBind rType rTerm
  = Rec.ArgsLet ( meta :: Metacontext | r ) termBind rType rTerm

type ArgsBuf r rType rTerm
  = Rec.ArgsBuf ( meta :: Metacontext | r ) rType rTerm

type ArgsData r rTypeBind rTerm rSumItems
  = Rec.ArgsData ( meta :: Metacontext | r ) rTypeBind rTerm rSumItems

type ArgsMatch r rTypeId rTerm rCaseItems
  = Rec.ArgsMatch ( meta :: Metacontext | r ) rTypeId rTerm rCaseItems

type ArgsHole r
  = Rec.ArgsHole ( meta :: Metacontext | r )

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
            args
              { termBind = args.termBind
              , body = incrementIndentation `mapArgsMeta` args.body
              }
    , neu:
        \args ->
          rec.neu
            args
              { termId = args.termId
              , argItems = incrementIndentation `mapArgsMeta` args.argItems
              }
    , let_:
        \args ->
          rec.let_
            args
              { termBind = incrementIndentation `mapArgsMeta` args.termBind
              , sign = incrementIndentation `mapArgsMeta` args.sign
              , impl = (incrementIndentation <<< insertVar args.let_.termBind.termId (unwrap args.let_.termBind.meta).name) `mapArgsMeta` args.impl
              , body = (incrementIndentation <<< insertVar args.let_.termBind.termId (unwrap args.let_.termBind.meta).name) `mapArgsMeta` args.body
              }
    , buf:
        \args ->
          rec.buf
            args
              { sign = incrementIndentation `mapArgsMeta` args.sign
              , impl = incrementIndentation `mapArgsMeta` args.impl
              , body = incrementIndentation `mapArgsMeta` args.body
              }
    , data_:
        \args ->
          rec.data_
            args
              { typeBind = args.typeBind
              , sumItems = incrementIndentation `mapArgsMeta` args.sumItems
              , body = (incrementIndentation <<< insertData args.data_) `mapArgsMeta` args.body
              }
    , match:
        \args ->
          rec.match
            args
              { typeId = args.typeId
              , term = incrementIndentation `mapArgsMeta` args.term
              , caseItems = undefined -- incrementIndentation `mapArgsMeta` args.caseItems
              }
    , hole: rec.hole
    }

-- | recArgItems
type ArgsArgItems r
  = Rec.ArgsArgItems ( meta :: Metacontext | r )

type ArgsArgItem r rTerm
  = Rec.ArgsArgItem ( meta :: Metacontext | r ) rTerm

-- | recSumItems
type ArgsSumItems r
  = Rec.ArgsSumItems ( meta :: Metacontext | r )

type ArgsSumItem r rTermBind rParamItems
  = Rec.ArgsSumItem ( meta :: Metacontext | r ) rTermBind rParamItems

-- | recCaseItem
type ArgsCaseItem r
  = Rec.ArgsCaseItem ( meta :: Metacontext | r )

type ArgsCaseItem_CaseItem r rTermBindItems rTerm
  = Rec.ArgsCaseItem_CaseItem ( meta :: Metacontext | r ) rTermBindItems rTerm

-- | recParamItems
type ArgsParamItems r
  = Rec.ArgsParamItems ( meta :: Metacontext | r )

type ArgsParamItem r rType
  = Rec.ArgsParamItem ( meta :: Metacontext | r ) rType

-- | recTermBindItems
type ArgsTermBindItems r
  = Rec.ArgsTermBindItems ( meta :: Metacontext | r )

type ArgsTermBindItem r rTermBind
  = Rec.ArgsTermBindItem ( meta :: Metacontext | r ) rTermBind

-- | recTypeBind
type ArgsTypeBind r
  = Rec.ArgsTypeBind ( meta :: Metacontext | r )

type ArgsTypeBind_TypeBind r rTypeId
  = Rec.ArgsTypeBind_TypeBind ( meta :: Metacontext | r ) rTypeId

-- | recTermBind
type ArgsTermBind r
  = Rec.ArgsTermBind ( meta :: Metacontext | r )

type ArgsTermBind_TermBind r rTermId
  = Rec.ArgsTermBind_TermBind ( meta :: Metacontext | r ) rTermId

-- | recTypeId
type ArgsTypeId r
  = Rec.ArgsTypeId ( meta :: Metacontext | r )

-- | recTermId
type ArgsTermId r
  = Rec.ArgsTermId ( meta :: Metacontext | r )

-- | recHoleId 
type ArgsHoleId r
  = Rec.ArgsHoleId ( meta :: Metacontext | r )
