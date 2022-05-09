module Language.Shape.Stlc.Recursor.Action where

import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Key
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Types
import Prelude
import Prim.Row
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Language.Shape.Stlc.Recursor.Metacontext as Rec
import Language.Shape.Stlc.Recursor.Record (modifyHetero)
import Record as Record
import Type.Proxy (Proxy(..))
import Undefined (undefined)

unimplementedEffect :: String -> This -> Effect Unit
unimplementedEffect label _ = do
  Console.log $ "undefined effect: " <> label

-- | ProtoRec
type ProtoArgs r1 r2
  = ( act :: Record ( | r1 ) | r2 )

type ProtoRec args r a
  -- = Rec.ProtoRec args r a
  = Rec.ProtoRec args r a

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

recType ::
  forall r a.
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  Lacks "act" r =>
  { arrow :: ProtoRec ArgsArrowType r a, data_ :: ProtoRec ArgsDataType r a, hole :: ProtoRec ArgsHoleType r a } ->
  ProtoRec ArgsType r a
recType rec =
  Rec.recType
    { arrow:
        \args ->
          let
            actions =
              [ Action
                  { label: Just "delete"
                  , triggers: [ ActionTrigger_Keypress { keys: keys.delete } ]
                  , effect: unimplementedEffect "delete"
                  }
              ]
                <> common args
          in
            rec.arrow $ modifyHetero _act (Record.insert _actions actions) args
    , data_:
        \args ->
          let
            actions = [] <> common args
          in
            rec.data_ $ modifyHetero _act (Record.insert _actions actions) args
    , hole:
        \args ->
          let
            actions = [] <> common args
          in
            rec.hole $ modifyHetero _act (Record.insert _actions actions) args
    }
  where
  common :: forall r1 r2. Record (Rec.ProtoArgsType r1 r2) -> Array Action
  common args =
    [ Action
        { label: Just "dig"
        , triggers: [ ActionTrigger_Keypress { keys: keys.dig } ]
        , effect: unimplementedEffect "dig"
        }
    , Action
        { label: Just "enarrow"
        , triggers: [ ActionTrigger_Keypress { keys: keys.lambda } ]
        , effect: unimplementedEffect "enarrow"
        }
    , Action
        { label: Just "copy"
        , triggers: [ ActionTrigger_Keypress { keys: keys.copy } ]
        , effect: unimplementedEffect "copy"
        }
    -- , toggleIndentation_Action args.ix.visit.ix
    ]

argsArrowType_dom :: forall r. Lacks "syn" r => Lacks "ctx" r => Lacks "ix" r => Lacks "meta" r => Lacks "act" r => Record (ArgsArrowType r) -> Record (ArgsType r)
argsArrowType_dom = Rec.argsArrowType_dom >>> \args -> args { act = {} }

argsArrowType_cod :: forall r. Lacks "syn" r => Lacks "ctx" r => Lacks "ix" r => Lacks "meta" r => Lacks "act" r => Record (ArgsArrowType r) -> Record (ArgsType r)
argsArrowType_cod = Rec.argsArrowType_cod >>> \args -> args { act = {} }

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
  forall r a.
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  Lacks "act" r =>
  { lam :: ProtoRec ArgsLam r a, neu :: ProtoRec ArgsNeu r a, let_ :: ProtoRec ArgsLet r a, buf :: ProtoRec ArgsBuf r a, data_ :: ProtoRec ArgsData r a, match :: ProtoRec ArgsMatch r a, hole :: ProtoRec ArgsHole r a } ->
  ProtoRec ArgsTerm r a
recTerm rec =
  Rec.recTerm
    { lam:
        \args ->
          let
            actions =
              [ Action
                  { label: Just "unlambda"
                  , triggers: [ ActionTrigger_Keypress { keys: keys.unlambda } ]
                  , effect: unimplementedEffect "unlambda"
                  }
              , Action
                  { label: Just "uneta"
                  , triggers: [ ActionTrigger_Keypress { keys: keys.uneta } ]
                  , effect: unimplementedEffect "uneta"
                  }
              ]
                <> common args
          in
            rec.lam $ modifyHetero _act (Record.insert _actions actions) args
    , neu:
        \args ->
          let
            actions =
              [ Action
                  { label: Just "eta"
                  , triggers: [ ActionTrigger_Keypress { keys: keys.eta } ]
                  , effect: unimplementedEffect "eta"
                  }
              ]
                <> common args
          in
            rec.neu $ modifyHetero _act (Record.insert _actions actions) args
    , let_:
        \args ->
          let
            actions =
              [ Action
                  { label: Just "unlet"
                  , triggers: [ ActionTrigger_Keypress { keys: keys.unlet } ]
                  , effect: unimplementedEffect "unlet"
                  }
              ]
                <> common args
          in
            rec.let_ $ modifyHetero _act (Record.insert _actions actions) args
    , buf:
        \args ->
          let
            actions =
              [ Action
                  { label: Just "unbuf"
                  , triggers: [ ActionTrigger_Keypress { keys: keys.unbuf } ]
                  , effect: unimplementedEffect "unbuf"
                  }
              ]
                <> common args
          in
            rec.buf $ modifyHetero _act (Record.insert _actions actions) args
    , data_:
        \args ->
          let
            actions =
              [ Action
                  { label: Just "undata"
                  , triggers: [ ActionTrigger_Keypress { keys: keys.undata } ]
                  , effect: unimplementedEffect "undata"
                  }
              ]
                <> common args
          in
            rec.data_ $ modifyHetero _act (Record.insert _actions actions) args
    , match:
        \args ->
          let
            actions = [] <> common args
          in
            rec.match $ modifyHetero _act (Record.insert _actions actions) args
    , hole:
        \args ->
          let
            actions =
              [ Action
                  { label: Just "fill"
                  , triggers: [] -- TODO
                  , effect: unimplementedEffect "fill"
                  }
              ]
                <> common args
          in
            rec.hole $ modifyHetero _act (Record.insert _actions actions) args
    }
  where
  common :: forall r1 r2. Record (Rec.ProtoArgsTerm r1 r2) -> Array Action
  common args =
    [ Action
        { label: Just "dig"
        , triggers: [ ActionTrigger_Keypress { keys: keys.dig } ]
        , effect: unimplementedEffect "dig"
        }
    , Action
        { label: Just "enlambda"
        , triggers: [ ActionTrigger_Keypress { keys: keys.lambda } ]
        , effect: unimplementedEffect "enlambda"
        }
    , Action
        { label: Just "enlet"
        , triggers: [ ActionTrigger_Keypress { keys: keys.let_ } ]
        , effect: unimplementedEffect "enlet"
        }
    , Action
        { label: Just "endata"
        , triggers: [ ActionTrigger_Keypress { keys: keys.data_ } ]
        , effect: unimplementedEffect "endata"
        }
    , Action
        { label: Just "enbuffer"
        , triggers: [ ActionTrigger_Keypress { keys: keys.buf } ]
        , effect: unimplementedEffect "enbuffer"
        }
    , Action
        { label: Just "copy"
        , triggers: [ ActionTrigger_Keypress { keys: keys.dig } ]
        , effect: unimplementedEffect "copy"
        }
    -- , toggleIndentation_Action args.ix.visit.ix
    ]

-- | Generic Array Action
toggleIndentation_Action :: IxUp -> Action
toggleIndentation_Action ixUp =
  Action
    { label: Just "toggle indentation"
    , triggers: [ ActionTrigger_Keypress { keys: keys.indent } ]
    , effect: unimplementedEffect "toggle indentation"
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
--   forall r a. 
--   Lacks "syn" r =>
--   Lacks "ctx" r =>
--   Lacks "ix" r =>
--   Lacks "meta" r =>
--   Lacks "act" r =>
--   { cons :: ProtoRec ArgsArgItemsCons r a, nil :: ProtoRec ArgsArgItemsNil r a } ->
--   ProtoRec ArgsArgItems r a
-- recArgItems rec =
--   Rec.recArgItems
--     { cons: \args -> rec.cons $ modifyHetero _act (Record.insert _actions actions) args
--     , nil: \args -> rec.nil $ modifyHetero _act (Record.insert _actions actions) args
--     }
-- | recArgItems
type ProtoArgsArgItems r1 r2
  = ProtoArgs r1 r2

type ArgsArgItems r
  = Rec.ArgsArgItems (ProtoArgsArgItems () r)

type ArgsArgItem r
  = Rec.ArgsArgItem (ProtoArgsArgItems ( actions :: Array Action ) r)

recArgItems ::
  forall r a.
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  Lacks "act" r =>
  { argItem :: ProtoRec ArgsArgItem r a } ->
  ProtoRec ArgsArgItems r (List a)
recArgItems rec =
  Rec.recArgItems
    { argItem:
        \args ->
          let
            actions = []
          in
            rec.argItem $ modifyHetero _act (Record.insert _actions actions) args
    }

-- | recSumItems
type ProtoArgsSumItems r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsSumItems r
  = Rec.ArgsSumItems (ProtoArgsSumItems () r)

type ArgsSumItem r
  = Rec.ArgsSumItem (ProtoArgsSumItems ( actions :: Array Action ) r)

recSumItems ::
  forall r a.
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  Lacks "act" r =>
  { sumItem :: ProtoRec ArgsSumItem r a } ->
  ProtoRec ArgsSumItems r (List a)
recSumItems rec =
  Rec.recSumItems
    { sumItem:
        \args ->
          let
            actions = []
          in
            rec.sumItem $ modifyHetero _act (Record.insert _actions actions) args
    }

-- | recCaseItem
type ProtoArgsCaseItems r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsCaseItems r
  = Rec.ArgsCaseItems (ProtoArgsCaseItems () r)

type ArgsCaseItem r
  = Rec.ArgsCaseItem (ProtoArgsCaseItems ( actions :: Array Action ) r)

recCaseItems ::
  forall r a.
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  Lacks "act" r =>
  { caseItem :: ProtoRec ArgsCaseItem r a } ->
  ProtoRec ArgsCaseItems r (List a)
recCaseItems rec =
  Rec.recCaseItems
    { caseItem:
        \args ->
          let
            actions = []
          in
            rec.caseItem $ modifyHetero _act (Record.insert _actions actions) args
    }

-- | recParamItems
type ProtoArgsParamItems r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsParamItems r
  = Rec.ArgsParamItems (ProtoArgsParamItems () r)

type ArgsParamItem r
  = Rec.ArgsParamItem (ProtoArgsParamItems ( actions :: Array Action ) r)

recParamItems ::
  forall r a.
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  Lacks "act" r =>
  { paramItem :: ProtoRec ArgsParamItem r a } ->
  ProtoRec ArgsParamItems r (List a)
recParamItems rec =
  Rec.recParamItems
    { paramItem:
        \args ->
          let
            actions = []
          in
            rec.paramItem $ modifyHetero _act (Record.insert _actions actions) args
    }

-- | recTermBindItems
type ProtoArgsTermBindItems r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsTermBindItems r
  = Rec.ArgsTermBindItems (ProtoArgsTermBindItems () r)

type ArgsTermBindItem r
  = Rec.ArgsTermBindItem (ProtoArgsTermBindItems ( actions :: Array Action ) r)

recTermBindItems ::
  forall r a.
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  Lacks "act" r =>
  { termBindItem :: ProtoRec ArgsTermBindItem r a } ->
  ProtoRec ArgsTermBindItems r (List a)
recTermBindItems rec =
  Rec.recTermBindItems
    { termBindItem:
        \args ->
          let
            actions = []
          in
            rec.termBindItem $ modifyHetero _act (Record.insert _actions actions) args
    }

-- | recTermBind
type ArgsTermBind r
  = Rec.ArgsTermBind (ProtoArgs () r)

type ArgsTermBind_TermBind r
  = Rec.ArgsTermBind (ProtoArgs ( actions :: Array Action ) r)

recTermBind ::
  forall r a.
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  Lacks "act" r =>
  { termBind :: ProtoRec ArgsTermBind_TermBind r a } ->
  ProtoRec ArgsTermBind r a
recTermBind rec =
  Rec.recTermBind
    { termBind:
        \args ->
          let
            actions = []
          in
            rec.termBind $ modifyHetero _act (Record.insert _actions actions) args
    }

-- | recTypeBind
type ArgsTypeBind r
  = Rec.ArgsTypeBind (ProtoArgs () r)

type ArgsTypeBind_TypeBind r
  = Rec.ArgsTypeBind (ProtoArgs ( actions :: Array Action ) r)

recTypeBind ::
  forall r a.
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  Lacks "act" r =>
  { typeBind :: ProtoRec ArgsTypeBind_TypeBind r a } ->
  ProtoRec ArgsTypeBind r a
recTypeBind rec =
  Rec.recTypeBind
    { typeBind:
        \args ->
          let
            actions = []
          in
            rec.typeBind $ modifyHetero _act (Record.insert _actions actions) args
    }

-- | recTypeId
type ArgsTypeId r
  = Rec.ArgsTypeId (ProtoArgs () r)

type ArgsTypeId_TypeId r
  = Rec.ArgsTypeId (ProtoArgs ( actions :: Array Action ) r)

recTypeId ::
  forall r a.
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  Lacks "act" r =>
  { typeId :: ProtoRec ArgsTypeId_TypeId r a } ->
  ProtoRec ArgsTypeId r a
recTypeId rec =
  Rec.recTypeId
    { typeId:
        \args ->
          let
            actions = []
          in
            rec.typeId $ modifyHetero _act (Record.insert _actions actions) args
    }

-- | recTermId
type ArgsTermId r
  = Rec.ArgsTermId (ProtoArgs () r)

type ArgsTermId_TermId r
  = Rec.ArgsTermId (ProtoArgs ( actions :: Array Action ) r)

recTermId ::
  forall r a.
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  Lacks "act" r =>
  { termId :: ProtoRec ArgsTermId_TermId r a } ->
  ProtoRec ArgsTermId r a
recTermId rec =
  Rec.recTermId
    { termId:
        \args ->
          let
            actions = []
          in
            rec.termId $ modifyHetero _act (Record.insert _actions actions) args
    }
