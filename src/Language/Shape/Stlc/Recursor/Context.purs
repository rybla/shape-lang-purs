module Language.Shape.Stlc.Recursor.Context where

import Language.Shape.Stlc.Recursor.Proxy
import Language.Shape.Stlc.Syntax
import Prelude
import Data.Default (default)
import Language.Shape.Stlc.Context (Context(..), insertData, insertVarType, lookupVarType)
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

type ArgsNeu r rTermId rArgItems
  = Rec.ArgsNeu ( gamma :: Context, alpha :: Type | r ) rTermId rArgItems

type ArgsLet r termBind rType rTerm
  = Rec.ArgsLet ( gamma :: Context, alpha :: Type | r ) termBind rType rTerm

type ArgsBuf r rType rTerm
  = Rec.ArgsBuf ( gamma :: Context, alpha :: Type | r ) rType rTerm

type ArgsData r rTypeBind rTerm rSumItems
  = Rec.ArgsData ( gamma :: Context, alpha :: Type | r ) rTypeBind rTerm rSumItems

type ArgsMatch r rTypeId rTerm rCaseItems
  = Rec.ArgsMatch ( gamma :: Context, alpha :: Type | r ) rTypeId rTerm rCaseItems

type ArgsHole r
  = Rec.ArgsHole ( gamma :: Context, alpha :: Type | r )

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
              { termBind = prune args.termBind
              , body =
                case args.alpha of
                  ArrowType arrowType -> insertVarType args.lam.termBind.termId arrowType.dom `mapArgsCtx` args.body
                  _ -> unsafeCrashWith "badly-typed lam"
              }
    , neu:
        \args ->
          rec.neu
            args
              { termId = prune args.termId
              , argItems = R.union { appType: lookupVarType args.neu.termId args.gamma } $ prune args.argItems
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
              , sumItems = prune args.sumItems
              , body = args.body { gamma = insertData args.data_ args.body.gamma }
              }
    , match:
        \args ->
          rec.match
            args
              { typeId = prune args.typeId
              , term = args.term { alpha = DataType { typeId: args.match.typeId, meta: default } }
              , caseItems = args.caseItems
              }
    , hole: rec.hole
    }
  where
  prune :: forall r. Lacks "alpha" r => Record ( alpha :: Type | r ) -> Record r
  prune args = R.delete _alpha args

-- | recArgItems
type ArgsArgItems r
  = Rec.ArgsArgItems ( gamma :: Context, appType :: Type | r )

type ArgsArgItem r rTerm
  = Rec.ArgsArgItem ( gamma :: Context, alpha :: Type | r ) rTerm

-- | recSumItems
type ArgsSumItems r
  = Rec.ArgsSumItems ( gamma :: Context | r )

type ArgsSumItem r rTermBind rParamItems
  = Rec.ArgsSumItem ( gamma :: Context | r ) rTermBind rParamItems

-- | recCaseItems
type ArgsCaseItems r
  = Rec.ArgsCaseItems ( gamma :: Context, alpha :: Type | r )

type ArgsCaseItem r rTermBindItems rTerm
  = Rec.ArgsCaseItem ( gamma :: Context, alpha :: Type | r ) rTermBindItems rTerm

-- | recParamItems
type ArgsParamItems r
  = Rec.ArgsParamItems ( gamma :: Context | r )

type ArgsParamItem r rType
  = Rec.ArgsParamItem ( gamma :: Context | r ) rType

-- | recTermBindItems
type ArgsTermBindItems r
  = Rec.ArgsTermBindItems ( gamma :: Context | r )

type ArgsTermBindItem r rTermBind
  = Rec.ArgsTermBindItem ( gamma :: Context | r ) rTermBind

-- | recTypeBind
type ArgsTypeBind r
  = Rec.ArgsTypeBind ( gamma :: Context | r )

type ArgsTypeBind_TypeBind r rTypeId
  = Rec.ArgsTypeBind_TypeBind ( gamma :: Context | r ) rTypeId

-- | recTermBind
type ArgsTermBind r
  = Rec.ArgsTermBind ( gamma :: Context | r )

type ArgsTermBind_TermBind r rTermId
  = Rec.ArgsTermBind_TermBind ( gamma :: Context | r ) rTermId

-- | recTypeId
type ArgsTypeId r
  = Rec.ArgsTypeId ( gamma :: Context | r )

-- | recTermId
type ArgsTermId r
  = Rec.ArgsTermId ( gamma :: Context | r )

-- | recHoleId 
type ArgsHoleId r
  = Rec.ArgsHoleId ( gamma :: Context | r )
