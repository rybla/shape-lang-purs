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

mapArgsCtx :: forall r. (Context -> Context) -> { ctx :: Context | r } -> { ctx :: Context | r }
mapArgsCtx f args = args { ctx = f args.ctx }

-- | recType
type ProtoArgsType r
  = ( ctx :: Context | r )

type ArgsType r
  = Rec.ArgsType (ProtoArgsType r)

type ArgsArrowType r rType
  = Rec.ArgsArrowType (ProtoArgsType r) rType

type ArgsDataType r rTypeId
  = Rec.ArgsDataType (ProtoArgsType r) rTypeId

type ArgsHoleType r rHoleId
  = Rec.ArgsHoleType (ProtoArgsType r) rHoleId

recType ::
  forall r a.
  Lacks "typeId" r =>
  Lacks "type_" r =>
  { arrowType :: Record (ArgsArrowType r (ArgsType r)) -> a
  , dataType :: Record (ArgsDataType r (ArgsTypeId r)) -> a
  , holeType :: Record (ArgsHoleType r (ArgsHoleId r)) -> a
  } ->
  Record (ArgsType r) -> a
recType = Rec.recType

-- | recTerm
type ProtoArgsTerm r
  = ( ctx :: Context, goal :: Type | r )

type ArgsTerm r
  = Rec.ArgsTerm (ProtoArgsTerm r)

type ArgsLam r rTermBind rTerm
  = Rec.ArgsLam (ProtoArgsTerm ( | r )) rTermBind rTerm

type ArgsNeu r rTermId rArgItems
  = Rec.ArgsNeu (ProtoArgsTerm ( | r )) rTermId rArgItems

type ArgsLet r termBind rType rTerm
  = Rec.ArgsLet (ProtoArgsTerm ( | r )) termBind rType rTerm

type ArgsBuf r rType rTerm
  = Rec.ArgsBuf (ProtoArgsTerm ( | r )) rType rTerm

type ArgsData r rTypeBind rTerm rSumItems
  = Rec.ArgsData (ProtoArgsTerm ( | r )) rTypeBind rTerm rSumItems

type ArgsMatch r rTypeId rTerm rCaseItems
  = Rec.ArgsMatch (ProtoArgsTerm ( | r )) rTypeId rTerm rCaseItems

type ArgsHole r
  = Rec.ArgsHole (ProtoArgsTerm ( | r ))

recTerm ::
  forall r a.
  Lacks "term" r =>
  Lacks "goal" r =>
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
                case args.goal of
                  ArrowType arrowType -> insertVarType args.lam.termBind.termId arrowType.dom `mapArgsCtx` args.body
                  _ -> unsafeCrashWith "badly-typed lam"
              }
    , neu:
        \args ->
          rec.neu
            args
              { termId = prune args.termId
              , argItems = R.union { appType: lookupVarType args.neu.termId args.ctx } $ prune args.argItems
              }
    , let_:
        \args ->
          rec.let_
            args
              { termBind = prune args.termBind
              , type_ = prune args.type_
              , term =
                args.term
                  { ctx = insertVarType args.let_.termBind.termId args.let_.type_ args.ctx
                  , goal = args.let_.type_
                  }
              , body =
                args.body { ctx = insertVarType args.let_.termBind.termId args.let_.type_ args.ctx }
              }
    , buf:
        \args ->
          rec.buf
            args
              { type_ = prune args.type_
              , term = args.term { goal = args.buf.type_ }
              }
    , data_:
        \args ->
          rec.data_
            args
              { typeBind = prune args.typeBind
              , sumItems = prune args.sumItems
              , body = args.body { ctx = insertData args.data_ args.body.ctx }
              }
    , match:
        \args ->
          rec.match
            args
              { typeId = prune args.typeId
              , term = args.term { goal = DataType { typeId: args.match.typeId, meta: default } }
              , caseItems = args.caseItems
              }
    , hole: rec.hole
    }
  where
  prune :: forall r. Lacks "goal" r => Record ( goal :: Type | r ) -> Record r
  prune args = R.delete _goal args

-- | recArgItems
type ArgsArgItems r
  = Rec.ArgsArgItems ( ctx :: Context, appType :: Type | r )

type ArgsArgItem r rTerm
  = Rec.ArgsArgItem ( ctx :: Context, goal :: Type | r ) rTerm

-- | recSumItems
type ArgsSumItems r
  = Rec.ArgsSumItems ( ctx :: Context | r )

type ArgsSumItem r rTermBind rParamItems
  = Rec.ArgsSumItem ( ctx :: Context | r ) rTermBind rParamItems

-- | recCaseItems
type ArgsCaseItems r
  = Rec.ArgsCaseItems ( ctx :: Context, goal :: Type | r )

type ArgsCaseItem r rTermBindItems rTerm
  = Rec.ArgsCaseItem ( ctx :: Context, goal :: Type | r ) rTermBindItems rTerm

-- | recParamItems
type ArgsParamItems r
  = Rec.ArgsParamItems ( ctx :: Context | r )

type ArgsParamItem r rType
  = Rec.ArgsParamItem ( ctx :: Context | r ) rType

-- | recTermBindItems
type ArgsTermBindItems r
  = Rec.ArgsTermBindItems ( ctx :: Context | r )

type ArgsTermBindItem r rTermBind
  = Rec.ArgsTermBindItem ( ctx :: Context | r ) rTermBind

-- | recTypeBind
type ArgsTypeBind r
  = Rec.ArgsTypeBind ( ctx :: Context | r )

type ArgsTypeBind_TypeBind r rTypeId
  = Rec.ArgsTypeBind_TypeBind ( ctx :: Context | r ) rTypeId

-- | recTermBind
type ArgsTermBind r
  = Rec.ArgsTermBind ( ctx :: Context | r )

type ArgsTermBind_TermBind r rTermId
  = Rec.ArgsTermBind_TermBind ( ctx :: Context | r ) rTermId

-- | recTypeId
type ArgsTypeId r
  = Rec.ArgsTypeId ( ctx :: Context | r )

-- | recTermId
type ArgsTermId r
  = Rec.ArgsTermId ( ctx :: Context | r )

-- | recHoleId 
type ArgsHoleId r
  = Rec.ArgsHoleId ( ctx :: Context | r )
