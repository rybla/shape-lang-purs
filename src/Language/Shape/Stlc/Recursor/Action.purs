module Language.Shape.Stlc.Recursor.Action where

import Data.Tuple.Nested
import Language.Shape.Stlc.ChAtIndex
import Language.Shape.Stlc.Changes
import Language.Shape.Stlc.Context
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Key
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Recursor.Proxy
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Syntax.Metadata
import Language.Shape.Stlc.Syntax.Modify
import Language.Shape.Stlc.Types
import Prelude
import Data.Array ((:))
import Data.Array as Array
import Data.Default (default)
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (over, unwrap, wrap)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Tuple (snd)
import Debug as Debug
import Effect (Effect)
import Language.Shape.Stlc.Event.KeyboardEvent (handleKeytype_Name)
import Language.Shape.Stlc.Hole (HoleEq)
import Language.Shape.Stlc.Metacontext (Metacontext(..), incrementIndentation, insertData, insertVar)
import Language.Shape.Stlc.Recursor.Index (Visit)
import Language.Shape.Stlc.Recursor.Metacontext as Rec
import Prim (Array, Record, Row)
import Prim as Prim
import Prim.Row (class Lacks)
import React (getState, modifyState)
import Record as R
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe (fromJust)

bindMaybeEffectUnit :: forall a. Maybe a -> (a -> Effect Unit) -> Effect Unit
bindMaybeEffectUnit = case _ of
  Just a -> (a # _)
  Nothing -> const (pure unit)

infixr 5 bindMaybeEffectUnit as >>|=

applyChange :: Change -> State -> Maybe State
applyChange change st = do
  Debug.traceM $ "===[ change ]==="
  Debug.traceM $ show change
  Debug.traceM $ "===[ history ]==="
  let
    -- history = (_ `Array.snoc` change) <$> st.history
    history = toHistoryItem st change : st.history
  Debug.traceM $ show history
  -- TODO: apply holeEq
  term' /\ ix' /\ typeChange /\ holeEq <- chAtTerm { term: st.term, gamma: default, alpha: st.type_ } change.toReplace change.ix
  pure
    st
      { term = term'
      , type_ = applyTC typeChange st.type_
      , mb_ix = Just ix'
      , history = history
      }

doChange :: This -> Change -> Effect Unit
doChange this change = modifyState this \st -> maybe st identity (applyChange change st)

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
            $ R.union { actions: common (ArrowType args.arrowType) args <> [] }
                args
    , dataType:
        \args ->
          rec.dataType
            $ R.union { actions: common (DataType args.dataType) args <> [] }
                args
    , holeType:
        \args ->
          rec.holeType
            $ R.union { actions: common (HoleType args.holeType) args <> [] }
                args
    }
  where
  common :: forall r. Type -> { | r } -> Array Action
  common type_ {} =
    [ Action
        { label: Just "enarrow"
        , effect:
            \{ this } -> do
              st <- getState this
              let
                holeType = freshHoleType unit
              doChange this { ix: fromJust st.mb_ix, toReplace: ReplaceType (ArrowType { dom: holeType, cod: type_, meta: default }) (InsertArg holeType) }
        , triggers: [ ActionTrigger_Keypress keys.lambda ]
        }
    , Action
        { label: Just "dig"
        , effect:
            \{ this } -> do
              st <- getState this
              let
                holeId = freshHoleId unit
              doChange this
                { ix: fromJust st.mb_ix
                , toReplace: ReplaceType (HoleType { holeId, weakening: Set.empty, meta: default }) (Dig holeId)
                }
        , triggers: [ ActionTrigger_Keypress keys.dig ]
        }
    ]

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
            $ R.union { actions: common (Lam args.lam) args <> [] }
                args
    , neu:
        \args ->
          rec.neu
            $ R.union { actions: common (Neu args.neu) args <> [] }
                args
    , let_:
        \args ->
          rec.let_
            $ R.union { actions: common (Let args.let_) args <> [] }
                args
    , buf:
        \args ->
          rec.buf
            $ R.union { actions: common (Buf args.buf) args <> [] }
                args
    , data_:
        \args ->
          rec.data_
            $ R.union { actions: common (Data args.data_) args <> [] }
                args
    , match:
        \args ->
          rec.match
            $ R.union { actions: common (Match args.match) args <> [] }
                args
    , hole:
        \args ->
          rec.hole
            $ R.union { actions: common (Hole args.hole) args <> [] }
                args
    }
  where
  common :: forall r. Term -> { visit :: Visit | r } -> Array Action
  common term args =
    [ Action
        { label: Just "enlambda"
        , effect:
            \{ this } ->
              args.visit.ix
                >>|= \ix ->
                    doChange this
                      { ix: toIxDown ix
                      , toReplace:
                          ReplaceTerm
                            (Lam { termBind: { termId: freshTermId unit, meta: default }, body: term, meta: default })
                            (InsertArg (freshHoleType unit))
                      }
        , triggers: [ ActionTrigger_Keypress keys.lambda ]
        }
    , Action
        { label: Just "dig"
        , effect:
            \{ this } ->
              args.visit.ix
                >>|= \ix ->
                    doChange this
                      { ix: toIxDown ix
                      , toReplace:
                          ReplaceTerm
                            (Hole { meta: default })
                            NoChange -- (Dig (freshHoleId unit))
                      }
        , triggers: [ ActionTrigger_Keypress keys.dig ]
        }
    , Action
        { label: Just "enlet"
        , effect:
            \{ this } ->
              args.visit.ix
                >>|= \ix ->
                    doChange this
                      { ix: toIxDown ix
                      , toReplace:
                          ReplaceTerm
                            (Let { termBind: freshTermBind unit, sign: freshHoleType unit, impl: freshHole unit, body: term, meta: default })
                            NoChange
                      }
        , triggers: [ ActionTrigger_Keypress keys.let_ ]
        }
    , Action
        { label: Just "enbuffer"
        , effect:
            \{ this } ->
              args.visit.ix
                >>|= \ix ->
                    doChange this
                      { ix: toIxDown ix
                      , toReplace:
                          ReplaceTerm
                            (Buf { sign: freshHoleType unit, impl: freshHole unit, body: term, meta: default })
                            NoChange
                      }
        , triggers: [ ActionTrigger_Keypress keys.buf ]
        }
    -- TODO: but actually, indentation is not necessarily a local action because it can step up the index via `stepUpToNearestIndentableParentIxUp` to perform an action
    , Action
        { label: Just "indent"
        , effect:
            \{ this } ->
              args.visit.ix
                >>|= \ix -> do
                    Debug.traceM "indent"
                    -- doChange this
                    --   { ix: toIxDown ix
                    --   , toReplace:
                    --       ReplaceTerm (indentTerm term) NoChange
                    --   }
                    let
                      mb_step /\ ixIndentableParent = stepUpToNearestIndentableParentIxUp ix
                    modifyState this \st ->
                      st { term = fromJust $ toTerm =<< indentSyntaxAt mb_step (toIxDown ixIndentableParent) (SyntaxTerm st.term) }
        , triggers: [ ActionTrigger_Keypress keys.indent ]
        }
    ]

-- | recArgItem
type ArgsArgItem r
  = Rec.ArgsArgItem ( | r )

type ArgsArgItem_ArgItem r rTerm
  = Rec.ArgsArgItem_ArgItem ( actions :: Array Action | r ) rTerm

recArgItem ::
  forall r a.
  Lacks "argItem" r =>
  Lacks "gamma" r =>
  Lacks "doms" r =>
  Lacks "cod" r =>
  { argItem :: Record (ArgsArgItem_ArgItem r (ArgsTerm r)) -> a } ->
  Record (ArgsArgItem r) -> a
recArgItem rec = Rec.recArgItem { argItem: \args -> rec.argItem $ R.union { actions: [] } args }

-- | recSumItem
type ArgsSumItem r
  = Rec.ArgsSumItem ( | r )

type ArgsSumItem_SumItem r rTermBind rParamItems
  = Rec.ArgsSumItem_SumItem ( actions :: Array Action | r ) rTermBind rParamItems

recSumItem ::
  forall r a.
  Lacks "sumItem" r =>
  { sumItem :: Record (ArgsSumItem_SumItem r (ArgsTermBind r) (ArgsParamItem r)) -> a } ->
  Record (ArgsSumItem r) -> a
recSumItem rec = Rec.recSumItem { sumItem: \args -> rec.sumItem $ R.union { actions: [] } args }

-- | recCaseItem
type ArgsCaseItem r
  = Rec.ArgsCaseItem ( | r )

type ArgsCaseItem_CaseItem r rTermBindItem rTerm
  = Rec.ArgsCaseItem_CaseItem ( actions :: Array Action | r ) rTermBindItem rTerm

recCaseItem ::
  forall r a.
  Lacks "caseItem" r =>
  Lacks "alpha" r =>
  Lacks "typeId" r =>
  { caseItem :: Record (ArgsCaseItem_CaseItem r (ArgsTermBindItem r) (ArgsTerm r)) -> a } ->
  Record (ArgsCaseItem r) -> a
recCaseItem rec = Rec.recCaseItem { caseItem: \args -> rec.caseItem $ R.union { actions: [] } args }

-- | recParamItems
type ArgsParamItem r
  = Rec.ArgsParamItem ( | r )

type ArgsParamItem_ParamItem r rType
  = Rec.ArgsParamItem_ParamItem ( actions :: Array Action | r ) rType

recParamItem ::
  forall r a.
  Lacks "paramItem" r =>
  { paramItem :: Record (ArgsParamItem_ParamItem r (ArgsType r)) -> a } ->
  Record (ArgsParamItem r) -> a
recParamItem rec = Rec.recParamItem { paramItem: \args -> rec.paramItem $ R.union { actions: [] } args }

-- | recTermBindItems
type ArgsTermBindItem r
  = Rec.ArgsTermBindItem ( | r )

type ArgsTermBindItem_TermBindItem r rTermBind
  = Rec.ArgsTermBindItem_TermBindItem ( actions :: Array Action | r ) rTermBind

recTermBindItem ::
  forall r a.
  Lacks "termBindItem" r =>
  { termBindItem :: Record (ArgsTermBindItem_TermBindItem r (ArgsTermBind r)) -> a } ->
  Record (ArgsTermBindItem r) -> a
recTermBindItem rec = Rec.recTermBindItem { termBindItem: \args -> rec.termBindItem $ R.union { actions: [] } args }

-- | recTypeBind
type ArgsTypeBind r
  = Rec.ArgsTypeBind ( | r )

type ArgsTypeBind_TypeBind r rTypeId
  = Rec.ArgsTypeBind_TypeBind ( actions :: Array Action | r ) rTypeId

recTypeBind ::
  forall r a.
  Lacks "typeBind" r =>
  { typeBind :: Record (ArgsTypeBind_TypeBind r (ArgsTypeId r)) -> a } ->
  Record (ArgsTypeBind r) -> a
recTypeBind rec =
  Rec.recTypeBind
    { typeBind:
        \args ->
          rec.typeBind
            $ R.union
                { actions:
                    [ Action
                        { label: Just "edit"
                        , triggers: [ ActionTrigger_Keytype ]
                        , effect:
                            case _ of
                              { this, mb_event: Just event } -> do
                                Debug.traceM "[event] ActionTrigger_Keytype"
                                args.visit.ix
                                  >>|= \ix ->
                                      handleKeytype_Name event (unwrap args.typeBind.meta).name
                                        >>|= \name' ->
                                            modifyState this \st ->
                                              st
                                                { term =
                                                  fromJust $ toTerm $ fromJust
                                                    $ replaceNameAt
                                                        args.typeBind
                                                        SyntaxTypeBind
                                                        TypeBindMetadata
                                                        name'
                                                        (toIxDown ix)
                                                        (SyntaxTerm st.term)
                                                }
                              _ -> pure unit
                        }
                    ]
                }
                args
    }

-- | recTermBind
type ArgsTermBind r
  = Rec.ArgsTermBind ( | r )

type ArgsTermBind_TermBind r rTermId
  = Rec.ArgsTermBind_TermBind ( actions :: Array Action | r ) rTermId

recTermBind ::
  forall r a.
  Lacks "termBind" r =>
  { termBind :: Record (ArgsTermBind_TermBind r (ArgsTermId r)) -> a } ->
  Record (ArgsTermBind r) -> a
recTermBind rec =
  Rec.recTermBind
    { termBind:
        \args ->
          rec.termBind
            $ R.union
                { actions:
                    [ Action
                        { label: Just "edit"
                        , triggers: [ ActionTrigger_Keytype ]
                        , effect:
                            case _ of
                              { this, mb_event: Just event } -> do
                                Debug.traceM "[event] ActionTrigger_Keytype"
                                args.visit.ix
                                  >>|= \ix ->
                                      handleKeytype_Name event (unwrap args.termBind.meta).name
                                        >>|= \name' ->
                                            modifyState this \st ->
                                              st
                                                { term =
                                                  fromJust $ toTerm $ fromJust
                                                    $ replaceNameAt
                                                        args.termBind
                                                        SyntaxTermBind
                                                        TermBindMetadata
                                                        name'
                                                        (toIxDown ix)
                                                        (SyntaxTerm st.term)
                                                }
                              _ -> pure unit
                        }
                    ]
                }
                args
    }

-- | recTypeId
type ArgsTypeId r
  = Rec.ArgsTypeId ( | r )

type ArgsTypeId_TypeId r
  = Rec.ArgsTypeId ( actions :: Array Action | r )

recTypeId ::
  forall r a.
  { typeId :: Record (ArgsTypeId_TypeId r) -> a } ->
  Record (ArgsTypeId r) -> a
recTypeId rec args = rec.typeId $ R.union { actions: [] } args

-- | recTermId
type ArgsTermId r
  = Rec.ArgsTermId ( | r )

type ArgsTermId_TermId r
  = Rec.ArgsTermId ( actions :: Array Action | r )

recTermId ::
  forall r a.
  { termId :: Record (ArgsTermId_TermId r) -> a } ->
  Record (ArgsTermId r) -> a
recTermId rec args = rec.termId $ R.union { actions: [] } args

-- | recHoleId 
type ArgsHoleId r
  = Rec.ArgsHoleId ( | r )

type ArgsHoleId_HoleId r
  = Rec.ArgsHoleId ( actions :: Array Action | r )

recHoleId ::
  forall r a.
  { holeId :: Record (ArgsHoleId_HoleId r) -> a } ->
  Record (ArgsHoleId r) -> a
recHoleId rec args = rec.holeId $ R.union { actions: [] } args
