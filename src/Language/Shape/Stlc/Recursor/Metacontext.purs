module Language.Shape.Stlc.Recursor.Metacontext where

import Data.Foldable
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
              { termBind = insertVar args.lam.termBind.termId (unwrap args.lam.termBind.meta).name `mapArgsMeta` args.termBind
              , body = (incrementIndentation <<< insertVar args.lam.termBind.termId (unwrap args.lam.termBind.meta).name) `mapArgsMeta` args.body
              }
    , neu:
        \args ->
          rec.neu
            args
              { termId = args.termId
              , argItems = (incrementIndentation `mapArgsMeta` _) <$> args.argItems
              }
    , let_:
        \args ->
          rec.let_
            args
              { termBind = (incrementIndentation <<< insertVar args.let_.termBind.termId (unwrap args.let_.termBind.meta).name) `mapArgsMeta` args.termBind
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
              { typeBind = insertData args.data_ `mapArgsMeta` args.typeBind
              , sumItems = ((incrementIndentation <<< insertData args.data_) `mapArgsMeta` _) <$> args.sumItems
              , body = (incrementIndentation <<< insertData args.data_) `mapArgsMeta` args.body
              }
    , match:
        \args ->
          rec.match
            args
              { typeId = args.typeId
              , term = incrementIndentation `mapArgsMeta` args.term
              , caseItems = map (incrementIndentation `mapArgsMeta` _) args.caseItems
              }
    , hole: rec.hole
    }

-- | recArgItems
type ArgsArgItem r
  = Rec.ArgsArgItem ( meta :: Metacontext | r )

type ArgsArgItem_ArgItem r rTerm
  = Rec.ArgsArgItem_ArgItem ( meta :: Metacontext | r ) rTerm

recArgItem ::
  forall r a.
  Lacks "argItem" r =>
  Lacks "gamma" r =>
  Lacks "doms" r =>
  Lacks "cod" r =>
  { argItem :: Record (ArgsArgItem_ArgItem r (ArgsTerm r)) -> a } ->
  Record (ArgsArgItem r) -> a
recArgItem = Rec.recArgItem

-- | recSumItems
type ArgsSumItem r
  = Rec.ArgsSumItem ( meta :: Metacontext | r )

type ArgsSumItem_SumItem r rTermBind rParamItems
  = Rec.ArgsSumItem_SumItem ( meta :: Metacontext | r ) rTermBind rParamItems

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
              { termBind = incrementIndentation `mapArgsMeta` args.termBind
              , paramItems = (incrementIndentation `mapArgsMeta` _) <$> args.paramItems
              }
    }

-- | recCaseItem
type ArgsCaseItem r
  = Rec.ArgsCaseItem ( meta :: Metacontext | r )

type ArgsCaseItem_CaseItem r rTermBindItem rTerm
  = Rec.ArgsCaseItem_CaseItem ( meta :: Metacontext | r ) rTermBindItem rTerm

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
              { termBindItems = args.termBindItems
              , body =
                foldl
                  (\term { termBindItem: { termBind } } -> insertVar termBind.termId (unwrap termBind.meta).name `mapArgsMeta` term)
                  (incrementIndentation `mapArgsMeta` args.body)
                  (args.termBindItems)
              }
    }

-- | recParamItems
type ArgsParamItem r
  = Rec.ArgsParamItem ( meta :: Metacontext | r )

type ArgsParamItem_ParamItem r rType
  = Rec.ArgsParamItem_ParamItem ( meta :: Metacontext | r ) rType

recParamItem ::
  forall r a.
  Lacks "paramItem" r =>
  { paramItem :: Record (ArgsParamItem_ParamItem r (ArgsType r)) -> a } ->
  Record (ArgsParamItem r) -> a
recParamItem = Rec.recParamItem

-- | recTermBindItems
type ArgsTermBindItem r
  = Rec.ArgsTermBindItem ( meta :: Metacontext | r )

type ArgsTermBindItem_TermBindItem r rTermBind
  = Rec.ArgsTermBindItem_TermBindItem ( meta :: Metacontext | r ) rTermBind

recTermBindItem ::
  forall r a.
  Lacks "termBindItem" r =>
  { termBindItem :: Record (ArgsTermBindItem_TermBindItem r (ArgsTermBind r)) -> a } ->
  Record (ArgsTermBindItem r) -> a
recTermBindItem = Rec.recTermBindItem

-- | recTypeBind
type ArgsTypeBind r
  = Rec.ArgsTypeBind ( meta :: Metacontext | r )

type ArgsTypeBind_TypeBind r rTypeId
  = Rec.ArgsTypeBind_TypeBind ( meta :: Metacontext | r ) rTypeId

recTypeBind ::
  forall r a.
  Lacks "typeBind" r =>
  { typeBind :: Record (ArgsTypeBind_TypeBind r (ArgsTypeId r)) -> a } ->
  Record (ArgsTypeBind r) -> a
recTypeBind = Rec.recTypeBind

-- | recTermBind
type ArgsTermBind r
  = Rec.ArgsTermBind ( meta :: Metacontext | r )

type ArgsTermBind_TermBind r rTermId
  = Rec.ArgsTermBind_TermBind ( meta :: Metacontext | r ) rTermId

recTermBind ::
  forall r a.
  Lacks "termBind" r =>
  { termBind :: Record (ArgsTermBind_TermBind r (ArgsTermId r)) -> a } ->
  Record (ArgsTermBind r) -> a
recTermBind = Rec.recTermBind

-- | recTypeId
type ArgsTypeId r
  = Rec.ArgsTypeId ( meta :: Metacontext | r )

-- | recTermId
type ArgsTermId r
  = Rec.ArgsTermId ( meta :: Metacontext | r )

-- | recHoleId 
type ArgsHoleId r
  = Rec.ArgsHoleId ( meta :: Metacontext | r )
