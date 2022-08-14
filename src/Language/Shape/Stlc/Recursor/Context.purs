module Language.Shape.Stlc.Recursor.Context where

import Data.Foldable
import Data.Tuple.Nested
import Language.Shape.Stlc.Recursor.Proxy
import Language.Shape.Stlc.Syntax
import Prelude
import Data.Default (default)
import Data.List (List)
import Data.List.Unsafe as List
import Debug as Debug
import Language.Shape.Stlc.Context (Context(..), flattenType, insertData, insertVarType, lookupData, lookupVarType, typeOfSumItem)
import Language.Shape.Stlc.Recursor.Base (mapHere)
import Language.Shape.Stlc.Recursor.Syntax as Rec
import Partial.Unsafe (unsafeCrashWith)
import Prim (Record, Row)
import Prim as Prim
import Prim.Row (class Lacks)
import Record as R
import Type.Proxy (Proxy(..))
import Undefined (undefined)

mapArgsCtx :: forall r. (Context -> Context) -> { gamma :: Context | r } -> { gamma :: Context | r }
mapArgsCtx f args = args { gamma = f args.gamma }

-- | recType
type ArgsType r
  = Rec.ArgsType ( gamma :: Context | r )

type ArgsArrowType r rType
  = Rec.ArgsArrowType ( gamma :: Context | r ) rType

type ArgsDataType r rTypeId
  = Rec.ArgsDataType ( gamma :: Context | r ) rTypeId

type ArgsHoleType r rHoleId
  = Rec.ArgsHoleType ( gamma :: Context | r ) rHoleId

recType ::
  forall r a.
  Lacks "type_" r =>
  { arrowType :: Record (ArgsArrowType r (ArgsType r)) -> a
  , dataType :: Record (ArgsDataType r (ArgsTypeId r)) -> a
  , holeType :: Record (ArgsHoleType r (ArgsHoleId r)) -> a
  } ->
  Record (ArgsType r) -> a
recType = Rec.recType

-- | recTerm
type ArgsTerm r
  = Rec.ArgsTerm ( gamma :: Context, alpha :: Type | r )

type ArgsLam r rTermBind rTerm
  = Rec.ArgsLam ( gamma :: Context, alpha :: Type | r ) rTermBind rTerm

type ArgsNeu r rTermId rArgItem
  = Rec.ArgsNeu ( gamma :: Context, alpha :: Type | r ) rTermId rArgItem

type ArgsLet r termBind rType rTerm
  = Rec.ArgsLet ( gamma :: Context, alpha :: Type | r ) termBind rType rTerm

type ArgsBuf r rType rTerm
  = Rec.ArgsBuf ( gamma :: Context, alpha :: Type | r ) rType rTerm

type ArgsData r rTypeBind rTerm rSumItem
  = Rec.ArgsData ( gamma :: Context, alpha :: Type | r ) rTypeBind rTerm rSumItem

type ArgsMatch r rTypeId rTerm rCaseItem
  = Rec.ArgsMatch ( gamma :: Context, alpha :: Type | r ) rTypeId rTerm rCaseItem

type ArgsHole r
  = Rec.ArgsHole ( gamma :: Context, alpha :: Type | r )

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
              { termBind = prune args.termBind
              , body =
                case args.alpha of
                  ArrowType arrowType ->
                    (insertVarType args.lam.termBind.termId arrowType.dom `mapArgsCtx` args.body)
                      { alpha = arrowType.cod }
                  type_ -> unsafeCrashWith $ "badly-typed lam with non-arrow type: " <> show type_
              }
    , neu:
        \args ->
          rec.neu
            let
              { doms, cod } = flattenType (lookupVarType args.neu.termId args.gamma)
            in
              args
                { termId = prune args.termId
                , argItems = (\(argItem /\ dom) -> argItem { alpha = dom }) <$> (args.argItems `List.zip` doms)
                }
    , let_:
        \args ->
          rec.let_
            args
              { termBind = prune args.termBind
              , sign = prune args.sign
              , impl =
                args.impl
                  { gamma = insertVarType args.let_.termBind.termId args.let_.sign args.gamma
                  , alpha = args.let_.sign
                  }
              , body =
                args.body { gamma = insertVarType args.let_.termBind.termId args.let_.sign args.gamma }
              }
    , buf:
        \args ->
          rec.buf
            args
              { sign = prune args.sign
              , impl = args.impl { alpha = args.buf.sign }
              }
    , data_:
        \args ->
          rec.data_
            args
              { typeBind = prune args.typeBind
              , sumItems = prune <$> args.sumItems
              , body = args.body { gamma = insertData args.data_ args.body.gamma }
              }
    , match:
        \args ->
          rec.match
            args
              { typeId = prune args.typeId
              , term = args.term { alpha = DataType { typeId: args.match.typeId, meta: default } }
              , caseItems = (\(argItem /\ sumItem) -> { typeId: args.match.typeId, termId: sumItem.termBind.termId } `R.union` argItem) <$> List.zip args.caseItems (lookupData args.match.typeId args.gamma).sumItems
              }
    , hole: rec.hole
    }
  where
  prune :: forall r. Lacks "alpha" r => Record ( alpha :: Type | r ) -> Record r
  prune args = R.delete _alpha args

-- | recArgItems
type ArgsArgItem r
  = Rec.ArgsArgItem ( gamma :: Context, alpha :: Type | r )

type ArgsArgItem_ArgItem r rTerm
  = Rec.ArgsArgItem_ArgItem ( gamma :: Context, alpha :: Type | r ) rTerm

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
  = Rec.ArgsSumItem ( gamma :: Context | r )

type ArgsSumItem_SumItem r rTermBind rParamItems
  = Rec.ArgsSumItem_SumItem ( gamma :: Context | r ) rTermBind rParamItems

recSumItem ::
  forall r a.
  Lacks "sumItem" r =>
  { sumItem :: Record (ArgsSumItem_SumItem r (ArgsTermBind r) (ArgsParamItem r)) -> a } ->
  Record (ArgsSumItem r) -> a
recSumItem = Rec.recSumItem

-- | recCaseItem
type ArgsCaseItem r
  = Rec.ArgsCaseItem ( gamma :: Context, alpha :: Type, typeId :: TypeId, termId :: TermId | r )

type ArgsCaseItem_CaseItem r rTermBind rTerm
  = Rec.ArgsCaseItem_CaseItem ( gamma :: Context, alpha :: Type, typeId :: TypeId, termId :: TermId | r ) rTermBind rTerm

recCaseItem ::
  forall r a.
  Lacks "caseItem" r =>
  Lacks "alpha" r =>
  Lacks "typeId" r =>
  Lacks "termId" r =>
  { caseItem :: Record (ArgsCaseItem_CaseItem r (ArgsTermBindItem r) (ArgsTerm r)) -> a } ->
  Record (ArgsCaseItem r) -> a
recCaseItem rec =
  Rec.recCaseItem
    { caseItem:
        \args ->
          let
            data_ = lookupData args.typeId args.gamma

            constr = lookupVarType args.termId args.gamma
          in
            rec.caseItem
              args
                { termBindItems = (R.delete _alpha <<< R.delete _typeId <<< R.delete _termId) <$> args.termBindItems
                , body =
                  foldr
                    (\(termBindItem /\ alpha) -> (insertVarType termBindItem.termBindItem.termBind.termId alpha `mapArgsCtx` _))
                    (R.delete _typeId <<< R.delete _termId $ args.body)
                    (args.termBindItems `List.zip` (flattenType constr).doms)
                }
    }

-- | recParamItems
type ArgsParamItem r
  = Rec.ArgsParamItem ( gamma :: Context | r )

type ArgsParamItem_ParamItem r rType
  = Rec.ArgsParamItem_ParamItem ( gamma :: Context | r ) rType

recParamItem ::
  forall r a.
  Lacks "paramItem" r =>
  { paramItem :: Record (ArgsParamItem_ParamItem r (ArgsType r)) -> a } ->
  Record (ArgsParamItem r) -> a
recParamItem = Rec.recParamItem

-- | recTermBindItems
type ArgsTermBindItem r
  = Rec.ArgsTermBindItem ( gamma :: Context | r )

type ArgsTermBindItem_TermBindItem r rTermBind
  = Rec.ArgsTermBindItem_TermBindItem ( gamma :: Context | r ) rTermBind

recTermBindItem ::
  forall r a.
  Lacks "termBindItem" r =>
  { termBindItem :: Record (ArgsTermBindItem_TermBindItem r (ArgsTermBind r)) -> a } ->
  Record (ArgsTermBindItem r) -> a
recTermBindItem = Rec.recTermBindItem

-- | recTypeBind
type ArgsTypeBind r
  = Rec.ArgsTypeBind ( gamma :: Context | r )

type ArgsTypeBind_TypeBind r rTypeId
  = Rec.ArgsTypeBind_TypeBind ( gamma :: Context | r ) rTypeId

recTypeBind ::
  forall r a.
  Lacks "typeBind" r =>
  { typeBind :: Record (ArgsTypeBind_TypeBind r (ArgsTypeId r)) -> a } ->
  Record (ArgsTypeBind r) -> a
recTypeBind = Rec.recTypeBind

-- | recTermBind
type ArgsTermBind r
  = Rec.ArgsTermBind ( gamma :: Context | r )

type ArgsTermBind_TermBind r rTermId
  = Rec.ArgsTermBind_TermBind ( gamma :: Context | r ) rTermId

recTermBind ::
  forall r a.
  Lacks "termBind" r =>
  { termBind :: Record (ArgsTermBind_TermBind r (ArgsTermId r)) -> a } ->
  Record (ArgsTermBind r) -> a
recTermBind = Rec.recTermBind

-- | recTypeId
type ArgsTypeId r
  = Rec.ArgsTypeId ( gamma :: Context | r )

-- | recTermId
type ArgsTermId r
  = Rec.ArgsTermId ( gamma :: Context | r )

-- | recHoleId 
type ArgsHoleId r
  = Rec.ArgsHoleId ( gamma :: Context | r )
