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

mapContext :: forall r1 r2. (Context -> Context) -> { here :: { context :: Context | r1 } | r2 } -> { here :: { context :: Context | r1 } | r2 }
mapContext f args = args { here = args.here { context = f args.here.context } }

-- | recType
type ProtoArgsType r
  = ( context :: Context | r )

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
  = ( context :: Context, goal :: Type | r )

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
              { termBind = R.delete _goal `mapHere` args.termBind
              , body =
                case args.here.goal of
                  ArrowType arrowType -> insertVarType args.here.lam.termBind.termId arrowType.dom `mapContext` args.body
                  _ -> unsafeCrashWith "badly-typed lam"
              }
    , neu:
        \args ->
          rec.neu
            args
              { termId = R.delete _goal `mapHere` args.termId
              , argItems = (R.delete _goal `mapHere` _) $ (R.union { appType: lookupVarType args.here.neu.termId args.here.context } `mapHere` _) $ args.argItems
              }
    , let_:
        \args ->
          rec.let_
            args
              { termBind = R.delete _goal `mapHere` args.termBind
              , type_ = R.delete _goal `mapHere` args.type_
              , term = (insertVarType args.here.let_.termBind.termId args.here.let_.type_ `mapContext` _) $ (_ { goal = args.here.let_.type_ } `mapHere` _) $ args.term
              , body = insertVarType args.here.let_.termBind.termId args.here.let_.type_ `mapContext` args.body
              }
    , buf:
        \args ->
          rec.buf
            args
              { type_ = R.delete _goal `mapHere` args.type_
              , term = (_ { goal = args.here.buf.type_ }) `mapHere` args.term
              }
    , data_:
        \args ->
          rec.data_
            args
              { typeBind = R.delete _goal `mapHere` args.typeBind
              , sumItems = R.delete _goal `mapHere` args.sumItems
              , body = insertData args.here.data_ `mapContext` args.body
              }
    , match:
        \args ->
          rec.match
            args
              { typeId = R.delete _goal `mapHere` args.typeId
              , term = (_ { goal = DataType { typeId: args.here.match.typeId, meta: default } }) `mapHere` args.term
              , caseItems = args.caseItems
              }
    , hole: rec.hole
    }

-- | recArgItems
type ArgsArgItems r
  = Rec.ArgsArgItems ( context :: Context, appType :: Type | r )

type ArgsArgItem r rTerm
  = Rec.ArgsArgItem ( context :: Context, goal :: Type | r ) rTerm

-- | recSumItems
type ArgsSumItems r
  = Rec.ArgsSumItems ( context :: Context | r )

type ArgsSumItem r rTermBind rParamItems
  = Rec.ArgsSumItem ( context :: Context | r ) rTermBind rParamItems

-- | recCaseItems
type ArgsCaseItems r
  = Rec.ArgsCaseItems ( context :: Context, goal :: Type | r )

type ArgsCaseItem r rTermBindItems rTerm
  = Rec.ArgsCaseItem ( context :: Context, goal :: Type | r ) rTermBindItems rTerm

-- | recParamItems
type ArgsParamItems r
  = Rec.ArgsParamItems ( context :: Context | r )

type ArgsParamItem r rType
  = Rec.ArgsParamItem ( context :: Context | r ) rType

-- | recTermBindItems
type ArgsTermBindItems r
  = Rec.ArgsTermBindItems ( context :: Context | r )

type ArgsTermBindItem r rTermBind
  = Rec.ArgsTermBindItem ( context :: Context | r ) rTermBind

-- | recTypeBind
type ArgsTypeBind r
  = Rec.ArgsTypeBind ( context :: Context | r )

type ArgsTypeBind_TypeBind r rTypeId
  = Rec.ArgsTypeBind_TypeBind ( context :: Context | r ) rTypeId

-- | recTermBind
type ArgsTermBind r
  = Rec.ArgsTermBind ( context :: Context | r )

type ArgsTermBind_TermBind r rTermId
  = Rec.ArgsTermBind_TermBind ( context :: Context | r ) rTermId

-- | recTypeId
type ArgsTypeId r
  = Rec.ArgsTypeId ( context :: Context | r )

-- | recTermId
type ArgsTermId r
  = Rec.ArgsTermId ( context :: Context | r )

-- | recHoleId 
type ArgsHoleId r
  = Rec.ArgsHoleId ( context :: Context | r )
