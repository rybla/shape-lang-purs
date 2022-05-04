module Language.Shape.Stlc.Recursor.Action where

import Language.Shape.Stlc.Types
import Prelude
import Prim.Row
import Data.List (List)
import Data.Maybe (Maybe(..))
import Language.Shape.Stlc.Index (IxDown(..), IxUp(..))
import Language.Shape.Stlc.Key (keys)
import Language.Shape.Stlc.Recursor.Metacontext as Rec
import Language.Shape.Stlc.Recursor.Record (modifyHetero)
import Record (insert, set)
import Type.Proxy (Proxy(..))
import Undefined (undefined)

-- | ProtoRec
type ProtoArgs r1 r2
  = ( act :: Record ( | r1 ) | r2 )

type ProtoRec args r a
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
          rec.arrow
            $ modifyHetero _act
                ( insert _actions
                    $ [ Action
                          { label: Just "delete"
                          , triggers: [ ActionTrigger_Keypress { keys: keys.delete } ]
                          , effect: undefined
                          }
                      ]
                    <> common args
                )
                args
    , data_:
        \args ->
          rec.data_
            $ modifyHetero _act
                ( insert _actions
                    $ []
                    <> common args
                )
                args
    , hole:
        \args ->
          rec.hole
            $ modifyHetero _act
                ( insert _actions
                    $ []
                    <> common args
                )
                args
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
          rec.lam
            $ modifyHetero _act
                ( insert _actions
                    $ [ Action
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
                )
                args
    , neu:
        \args ->
          rec.neu
            $ modifyHetero _act
                ( insert _actions
                    $ [ Action
                          { label: Just "eta"
                          , triggers: [ ActionTrigger_Keypress { keys: keys.eta } ]
                          , effect: undefined
                          }
                      ]
                    <> common args
                )
                args
    , let_:
        \args ->
          rec.let_
            $ modifyHetero _act
                ( insert _actions
                    $ [ Action
                          { label: Just "unlet"
                          , triggers: [ ActionTrigger_Keypress { keys: keys.unlet } ]
                          , effect: undefined
                          }
                      ]
                    <> common args
                )
                args
    , buf:
        \args ->
          rec.buf
            $ modifyHetero _act
                ( insert _actions
                    $ [ Action
                          { label: Just "unbuf"
                          , triggers: [ ActionTrigger_Keypress { keys: keys.unbuf } ]
                          , effect: undefined
                          }
                      ]
                    <> common args
                )
                args
    , data_:
        \args ->
          rec.data_
            $ modifyHetero _act
                ( insert _actions
                    $ [ Action
                          { label: Just "undata"
                          , triggers: [ ActionTrigger_Keypress { keys: keys.undata } ]
                          , effect: undefined
                          }
                      ]
                    <> common args
                )
                args
    , match:
        \args ->
          rec.match
            $ modifyHetero _act
                ( insert _actions
                    $ []
                    <> common args
                )
                args
    , hole:
        \args ->
          rec.hole
            $ modifyHetero _act
                ( insert _actions
                    $ [ Action
                          { label: Just "fill"
                          , triggers: [] -- TODO
                          , effect: undefined
                          }
                      ]
                    <> common args
                )
                args
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
  forall r a.
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  Lacks "act" r =>
  { argItem :: ProtoRec ArgsArgItem r a } ->
  ProtoRec ArgsArgItems r (List a)
recArgItems rec = Rec.recArgItems { argItem: \args -> rec.argItem $ modifyHetero _act (insert _actions []) args }

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
recSumItems rec = Rec.recSumItems { sumItem: \args -> rec.sumItem $ modifyHetero _act (insert _actions []) args }

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
recCaseItems rec = Rec.recCaseItems { caseItem: \args -> rec.caseItem $ modifyHetero _act (insert _actions []) args }

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
  { param :: ProtoRec ArgsParamItem r a } ->
  ProtoRec ArgsParamItems r (List a)
recParamItems rec = Rec.recParamItems { paramItem: \args -> rec.param $ modifyHetero _act (insert _actions []) args }

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
  { termBind :: ProtoRec ArgsTermBindItem r a } ->
  ProtoRec ArgsTermBindItems r (List a)
recTermBindItems rec = Rec.recTermBindItems { termBindItem: \args -> rec.termBind $ modifyHetero _act (insert _actions []) args }

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
recTermBind rec = Rec.recTermBind { termBind: \args -> rec.termBind $ modifyHetero _act (insert _actions []) args }

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
recTypeBind rec = Rec.recTypeBind { typeBind: \args -> rec.typeBind $ modifyHetero _act (insert _actions []) args }

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
recTypeId rec = Rec.recTypeId { typeId: \args -> rec.typeId $ modifyHetero _act (insert _actions []) args }

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
recTermId rec = Rec.recTermId { termId: \args -> rec.termId $ modifyHetero _act (insert _actions []) args }
