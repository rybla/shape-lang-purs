module Language.Shape.Stlc.Recursor.Action where

import Language.Shape.Stlc.Types
import Prelude
import Prim.Row

import Control.Monad.State (StateT)
import Control.Monad.State as State
import Control.Monad.Trans.Class (lift)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Language.Shape.Stlc.Index (IxDown(..), IxUp(..))
import Language.Shape.Stlc.Key (keys)
import Language.Shape.Stlc.Recursor.Index (Cursor)
import Language.Shape.Stlc.Recursor.Metacontext as Rec
import Language.Shape.Stlc.Recursor.Record (modifyHetero)
import Language.Shape.Stlc.Syntax (HoleId(..))
import Record (insert, set)
import Type.Proxy (Proxy(..))
import Undefined (undefined)

-- | ProtoRec
type ProtoArgs r1 r2
  = ( act :: Record ( | r1 ) | r2 )

type ProtoRec args r m a
  -- = Rec.ProtoRec args r m a
  = Rec.ProtoRec args r (StateT (Array Action) m) a

_act = Proxy :: Proxy "act"

_actions = Proxy :: Proxy "actions"

-- | recType
type ProtoArgsType r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsType r
  = Rec.ArgsType (ProtoArgsType () r)

type ArgsArrowType r
  = Rec.ArgsArrowType (ProtoArgsType ( actions :: Array Action ) r)

type ArgsDataType r
  = Rec.ArgsDataType (ProtoArgsType ( actions :: Array Action ) r)

type ArgsHoleType r
  = Rec.ArgsHoleType (ProtoArgsType ( actions :: Array Action ) r)

checkActionsHere :: forall r1 r2 r3 m. Monad m => { ix :: { visit :: { csr :: Cursor | r1 } | r2 } | r3 } -> Array Action -> StateT (List HoleId) (StateT (Array Action) m) Unit
checkActionsHere { ix: { visit: { csr } } } actions = case csr of
  Just (IxDown Nil) -> lift $ State.put actions
  _ -> pure unit

recType ::
  forall r m a.
  Monad m =>
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  Lacks "act" r =>
  { arrow :: ProtoRec ArgsArrowType r m a, data_ :: ProtoRec ArgsDataType r m a, hole :: ProtoRec ArgsHoleType r m a } ->
  ProtoRec ArgsType r m a
recType rec =
  Rec.recType
    { arrow:
        \args ->
          let
            actions =
              [ Action
                  { label: Just "delete"
                  , triggers: [ ActionTrigger_Keypress { keys: keys.delete } ]
                  , effect: undefined
                  }
              ]
                <> common args
          in
            do
              checkActionsHere args actions
              rec.arrow $ modifyHetero _act (insert _actions actions) args
    , data_:
        \args ->
          let
            actions = [] <> common args
          in
            do
              checkActionsHere args actions
              rec.data_ $ modifyHetero _act (insert _actions actions) args
    , hole:
        \args ->
          let
            actions = [] <> common args
          in
            do
              checkActionsHere args actions
              rec.hole $ modifyHetero _act (insert _actions actions) args
    }
  where
  common :: forall r1 r2. Record (Rec.ProtoArgsType r1 r2) -> Array Action
  common args =
    [ Action
        { label: Just "dig"
        , triggers: [ ActionTrigger_Keypress { keys: keys.dig } ]
        , effect: undefined
        }
    , Action
        { label: Just "enarrow"
        , triggers: [ ActionTrigger_Keypress { keys: keys.lambda } ]
        , effect: undefined
        }
    , Action
        { label: Just "copy"
        , triggers: [ ActionTrigger_Keypress { keys: keys.copy } ]
        , effect: undefined
        }
    -- , toggleIndentation_Action args.ix.visit.ix
    ]

-- | recTerm
type ProtoArgsTerm r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsTerm r
  = Rec.ArgsTerm (ProtoArgsTerm () r)

type ArgsLam r
  = Rec.ArgsLam (ProtoArgsTerm ( actions :: Array Action ) r)

type ArgsNeu r
  = Rec.ArgsNeu (ProtoArgsTerm ( actions :: Array Action ) r)

type ArgsLet r
  = Rec.ArgsLet (ProtoArgsTerm ( actions :: Array Action ) r)

type ArgsBuf r
  = Rec.ArgsBuf (ProtoArgsTerm ( actions :: Array Action ) r)

type ArgsData r
  = Rec.ArgsData (ProtoArgsTerm ( actions :: Array Action ) r)

type ArgsMatch r
  = Rec.ArgsMatch (ProtoArgsTerm ( actions :: Array Action ) r)

type ArgsHole r
  = Rec.ArgsHole (ProtoArgsTerm ( actions :: Array Action ) r)

recTerm ::
  forall r m a.
  Monad m =>
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  Lacks "act" r =>
  { lam :: ProtoRec ArgsLam r m a, neu :: ProtoRec ArgsNeu r m a, let_ :: ProtoRec ArgsLet r m a, buf :: ProtoRec ArgsBuf r m a, data_ :: ProtoRec ArgsData r m a, match :: ProtoRec ArgsMatch r m a, hole :: ProtoRec ArgsHole r m a } ->
  ProtoRec ArgsTerm r m a
recTerm rec =
  Rec.recTerm
    { lam:
        \args ->
          let
            actions =
              [ Action
                  { label: Just "unlambda"
                  , triggers: [ ActionTrigger_Keypress { keys: keys.unlambda } ]
                  , effect: undefined
                  }
              , Action
                  { label: Just "uneta"
                  , triggers: [ ActionTrigger_Keypress { keys: keys.uneta } ]
                  , effect: undefined
                  }
              ]
                <> common args
          in
            do
              checkActionsHere args actions
              rec.lam $ modifyHetero _act (insert _actions actions) args
    , neu:
        \args ->
          let
            actions =
              [ Action
                  { label: Just "eta"
                  , triggers: [ ActionTrigger_Keypress { keys: keys.eta } ]
                  , effect: undefined
                  }
              ]
                <> common args
          in
            do
              checkActionsHere args actions
              rec.neu $ modifyHetero _act (insert _actions actions) args
    , let_:
        \args ->
          let
            actions =
              [ Action
                  { label: Just "unlet"
                  , triggers: [ ActionTrigger_Keypress { keys: keys.unlet } ]
                  , effect: undefined
                  }
              ]
                <> common args
          in
            do
              checkActionsHere args actions
              rec.let_ $ modifyHetero _act (insert _actions actions) args
    , buf:
        \args ->
          let
            actions =
              [ Action
                  { label: Just "unbuf"
                  , triggers: [ ActionTrigger_Keypress { keys: keys.unbuf } ]
                  , effect: undefined
                  }
              ]
                <> common args
          in
            do
              checkActionsHere args actions
              rec.buf $ modifyHetero _act (insert _actions actions) args
    , data_:
        \args ->
          let
            actions =
              [ Action
                  { label: Just "undata"
                  , triggers: [ ActionTrigger_Keypress { keys: keys.undata } ]
                  , effect: undefined
                  }
              ]
                <> common args
          in
            do
              checkActionsHere args actions
              rec.data_ $ modifyHetero _act (insert _actions actions) args
    , match:
        \args ->
          let
            actions = [] <> common args
          in
            do
              checkActionsHere args actions
              rec.match $ modifyHetero _act (insert _actions actions) args
    , hole:
        \args ->
          let
            actions =
              [ Action
                  { label: Just "fill"
                  , triggers: [] -- TODO
                  , effect: undefined
                  }
              ]
                <> common args
          in
            do
              checkActionsHere args actions
              rec.hole $ modifyHetero _act (insert _actions actions) args
    }
  where
  common :: forall r1 r2. Record (Rec.ProtoArgsTerm r1 r2) -> Array Action
  common args =
    [ Action
        { label: Just "dig"
        , triggers: [ ActionTrigger_Keypress { keys: keys.dig } ]
        , effect: undefined
        }
    , Action
        { label: Just "enlambda"
        , triggers: [ ActionTrigger_Keypress { keys: keys.lambda } ]
        , effect: undefined
        }
    , Action
        { label: Just "enlet"
        , triggers: [ ActionTrigger_Keypress { keys: keys.let_ } ]
        , effect: undefined
        }
    , Action
        { label: Just "endata"
        , triggers: [ ActionTrigger_Keypress { keys: keys.data_ } ]
        , effect: undefined
        }
    , Action
        { label: Just "enbuffer"
        , triggers: [ ActionTrigger_Keypress { keys: keys.buf } ]
        , effect: undefined
        }
    , Action
        { label: Just "copy"
        , triggers: [ ActionTrigger_Keypress { keys: keys.dig } ]
        , effect: undefined
        }
    -- , toggleIndentation_Action args.ix.visit.ix
    ]

-- | Generic Array Action
toggleIndentation_Action :: IxUp -> Action
toggleIndentation_Action ixUp =
  Action
    { label: Just "toggle indentation"
    , triggers: [ ActionTrigger_Keypress { keys: keys.indent } ]
    , effect: undefined
    }

-- -- | recArgItems
-- type ProtoArgsArgItems r1 r2
--   = ProtoArgs r1 r2
-- type ArgsArgItems r
--   = Rec.ArgsArgItems (ProtoArgsArgItems () r)
-- type ArgsArgItemsCons r
--   = Rec.ArgsArgItemsCons (ProtoArgsArgItems ( actions :: Array Action ) r)
-- type ArgsArgItemsNil r
--   = Rec.ArgsArgItemsNil (ProtoArgsArgItems ( actions :: Array Action ) r)
-- recArgItems ::
--   forall r m a. Monad m =>
--   Lacks "syn" r =>
--   Lacks "ctx" r =>
--   Lacks "ix" r =>
--   Lacks "meta" r =>
--   Lacks "act" r =>
--   { cons :: ProtoRec ArgsArgItemsCons r m a, nil :: ProtoRec ArgsArgItemsNil r m a } ->
--   ProtoRec ArgsArgItems r m a
-- recArgItems rec =
--   Rec.recArgItems
--     { cons: \args -> rec.cons $ modifyHetero _act (insert _actions []) args
--     , nil: \args -> rec.nil $ modifyHetero _act (insert _actions []) args
--     }
-- | recArgItems
type ProtoArgsArgItems r1 r2
  = ProtoArgs r1 r2

type ArgsArgItems r
  = Rec.ArgsArgItems (ProtoArgsArgItems () r)

type ArgsArgItem r
  = Rec.ArgsArgItem (ProtoArgsArgItems ( actions :: Array Action ) r)

recArgItems ::
  forall r m a.
  Monad m =>
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  Lacks "act" r =>
  { argItem :: ProtoRec ArgsArgItem r m a } ->
  ProtoRec ArgsArgItems r m (List a)
recArgItems rec =
  Rec.recArgItems
    { argItem:
        \args ->
          let
            actions = []
          in
            do
              checkActionsHere args actions
              rec.argItem $ modifyHetero _act (insert _actions []) args
    }

-- | recSumItems
type ProtoArgsSumItems r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsSumItems r
  = Rec.ArgsSumItems (ProtoArgsSumItems () r)

type ArgsSumItem r
  = Rec.ArgsSumItem (ProtoArgsSumItems ( actions :: Array Action ) r)

recSumItems ::
  forall r m a.
  Monad m =>
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  Lacks "act" r =>
  { sumItem :: ProtoRec ArgsSumItem r m a } ->
  ProtoRec ArgsSumItems r m (List a)
recSumItems rec =
  Rec.recSumItems
    { sumItem:
        \args ->
          let
            actions = []
          in
            do
              checkActionsHere args actions
              rec.sumItem $ modifyHetero _act (insert _actions []) args
    }

-- | recCaseItem
type ProtoArgsCaseItems r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsCaseItems r
  = Rec.ArgsCaseItems (ProtoArgsCaseItems () r)

type ArgsCaseItem r
  = Rec.ArgsCaseItem (ProtoArgsCaseItems ( actions :: Array Action ) r)

recCaseItems ::
  forall r m a.
  Monad m =>
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  Lacks "act" r =>
  { caseItem :: ProtoRec ArgsCaseItem r m a } ->
  ProtoRec ArgsCaseItems r m (List a)
recCaseItems rec =
  Rec.recCaseItems
    { caseItem:
        \args ->
          let
            actions = []
          in
            do
              checkActionsHere args actions
              rec.caseItem $ modifyHetero _act (insert _actions []) args
    }

-- | recParamItems
type ProtoArgsParamItems r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsParamItems r
  = Rec.ArgsParamItems (ProtoArgsParamItems () r)

type ArgsParamItem r
  = Rec.ArgsParamItem (ProtoArgsParamItems ( actions :: Array Action ) r)

recParamItems ::
  forall r m a.
  Monad m =>
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  Lacks "act" r =>
  { paramItem :: ProtoRec ArgsParamItem r m a } ->
  ProtoRec ArgsParamItems r m (List a)
recParamItems rec =
  Rec.recParamItems
    { paramItem:
        \args ->
          let
            actions = []
          in
            do
              checkActionsHere args actions
              rec.paramItem $ modifyHetero _act (insert _actions []) args
    }

-- | recTermBindItems
type ProtoArgsTermBindItems r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsTermBindItems r
  = Rec.ArgsTermBindItems (ProtoArgsTermBindItems () r)

type ArgsTermBindItem r
  = Rec.ArgsTermBindItem (ProtoArgsTermBindItems ( actions :: Array Action ) r)

recTermBindItems ::
  forall r m a.
  Monad m =>
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  Lacks "act" r =>
  { termBindItem :: ProtoRec ArgsTermBindItem r m a } ->
  ProtoRec ArgsTermBindItems r m (List a)
recTermBindItems rec =
  Rec.recTermBindItems
    { termBindItem:
        \args ->
          let
            actions = []
          in
            do
              checkActionsHere args actions
              rec.termBindItem $ modifyHetero _act (insert _actions []) args
    }

-- | recTermBind
type ArgsTermBind r
  = Rec.ArgsTermBind (ProtoArgs () r)

type ArgsTermBind_TermBind r
  = Rec.ArgsTermBind (ProtoArgs ( actions :: Array Action ) r)

recTermBind ::
  forall r m a.
  Monad m =>
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  Lacks "act" r =>
  { termBind :: ProtoRec ArgsTermBind_TermBind r m a } ->
  ProtoRec ArgsTermBind r m a
recTermBind rec =
  Rec.recTermBind
    { termBind:
        \args ->
          let
            actions = []
          in
            do
              checkActionsHere args actions
              rec.termBind $ modifyHetero _act (insert _actions []) args
    }

-- | recTypeBind
type ArgsTypeBind r
  = Rec.ArgsTypeBind (ProtoArgs () r)

type ArgsTypeBind_TypeBind r
  = Rec.ArgsTypeBind (ProtoArgs ( actions :: Array Action ) r)

recTypeBind ::
  forall r m a.
  Monad m =>
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  Lacks "act" r =>
  { typeBind :: ProtoRec ArgsTypeBind_TypeBind r m a } ->
  ProtoRec ArgsTypeBind r m a
recTypeBind rec =
  Rec.recTypeBind
    { typeBind:
        \args ->
          let
            actions = []
          in
            do
              checkActionsHere args actions
              rec.typeBind $ modifyHetero _act (insert _actions []) args
    }

-- | recTypeId
type ArgsTypeId r
  = Rec.ArgsTypeId (ProtoArgs () r)

type ArgsTypeId_TypeId r
  = Rec.ArgsTypeId (ProtoArgs ( actions :: Array Action ) r)

recTypeId ::
  forall r m a.
  Monad m =>
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  Lacks "act" r =>
  { typeId :: ProtoRec ArgsTypeId_TypeId r m a } ->
  ProtoRec ArgsTypeId r m a
recTypeId rec =
  Rec.recTypeId
    { typeId:
        \args ->
          let
            actions = []
          in
            do
              checkActionsHere args actions
              rec.typeId $ modifyHetero _act (insert _actions []) args
    }

-- | recTermId
type ArgsTermId r
  = Rec.ArgsTermId (ProtoArgs () r)

type ArgsTermId_TermId r
  = Rec.ArgsTermId (ProtoArgs ( actions :: Array Action ) r)

recTermId ::
  forall r m a.
  Monad m =>
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  Lacks "act" r =>
  { termId :: ProtoRec ArgsTermId_TermId r m a } ->
  ProtoRec ArgsTermId r m a
recTermId rec =
  Rec.recTermId
    { termId:
        \args ->
          let
            actions = []
          in
            do
              checkActionsHere args actions
              rec.termId $ modifyHetero _act (insert _actions []) args
    }
