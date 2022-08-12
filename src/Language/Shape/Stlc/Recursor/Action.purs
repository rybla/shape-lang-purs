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
import Control.Monad.State (runState)
import Data.Array ((:))
import Data.Array as Array
import Data.Default (default)
import Data.Foldable (foldM)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (over, unwrap, wrap)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Tuple (snd)
import Debug as Debug
import Effect (Effect)
import Language.Shape.Stlc.Event.KeyboardEvent (handleKeytype_Name, handleKeytype_String)
import Language.Shape.Stlc.Hole (HoleEq, HoleSub, restrictToFull, subTerm, unifyType)
import Language.Shape.Stlc.Metacontext (Metacontext(..), incrementIndentation, insertData, insertVar)
import Language.Shape.Stlc.Recursor.Index (Visit)
import Language.Shape.Stlc.Recursor.Metacontext as Rec
import Language.Shape.Stlc.Rendering.Utilities (maybeArray)
import Partial.Unsafe (unsafeCrashWith)
import Prim (Array, Record, Row, String)
import Prim as Prim
import Prim.Row (class Lacks)
import React (ReactElement, getState, modifyState)
import React.DOM as DOM
import React.DOM.Props as Props
import Record as R
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe (fromJust)

bindMaybeEffectUnit :: forall a. Maybe a -> (a -> Effect Unit) -> Effect Unit
bindMaybeEffectUnit = case _ of
  Just a -> (a # _)
  Nothing -> const (pure unit)

infixr 5 bindMaybeEffectUnit as >>|=

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
            $ R.union
                { actions:
                    common (ArrowType args.arrowType) args
                      <> [ Action
                            { label: Just "unarrow"
                            , tooltip: makeExampleTooltip "unwrap an arrow around a type" "A -> B" "B"
                            , triggers: [ ActionTrigger_Keypress keys.unlambda ]
                            , effect:
                                \{ this } -> do
                                  st <- getState this
                                  -- TODO: delete instances of bound term (ask jacob)
                                  doChange this { ix: fromJust st.mb_ix, toReplace: ReplaceType args.arrowType.cod RemoveArg }
                            }
                        ]
                      <> maybeArray
                          ( case args.arrowType.cod of
                              ArrowType arrowType -> Just arrowType
                              _ -> Nothing
                          )
                          ( \arrowType' ->
                              Action
                                { label: Just "swaparrow"
                                , tooltip: makeExampleTooltip "swap the order of nested arrows" "A -> B -> C" "B -> A -> C"
                                , triggers: [ ActionTrigger_Keypress keys.swap ]
                                , effect:
                                    \{ this } -> do
                                      st <- getState this
                                      doChange this
                                        { ix: fromJust st.mb_ix
                                        , toReplace:
                                            ReplaceType
                                              ( ArrowType
                                                  { dom: arrowType'.dom
                                                  , cod:
                                                      ArrowType
                                                        { dom: args.arrowType.dom
                                                        , cod: arrowType'.cod
                                                        , meta: arrowType'.meta
                                                        }
                                                  , meta: args.arrowType.meta
                                                  }
                                              )
                                              Swap
                                        }
                                }
                          )
                }
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
  common :: forall r. Type -> { visit :: Visit | r } -> Array Action
  common type_ args =
    [ Action
        { label: Just "enarrow"
        , tooltip: makeExampleTooltip "wrap a type in an arrow" "A" "? -> A"
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
        , tooltip: makeExampleTooltip "replace a type with a hole" "A" "?"
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
    , actionIndent args.visit.ix
    ]

-- | recTerm
type ArgsTerm r
  = Rec.ArgsTerm ( | r )

type ArgsLam r rTermBind rTerm
  = Rec.ArgsLam ( actions :: Array Action | r ) rTermBind rTerm

type ArgsNeu r rTermId rArgItem
  = Rec.ArgsNeu ( actions :: Array Action | r ) rTermId rArgItem

type ArgsLet r termBind rType rTerm
  = Rec.ArgsLet ( actions :: Array Action | r ) termBind rType rTerm

type ArgsBuf r rType rTerm
  = Rec.ArgsBuf ( actions :: Array Action | r ) rType rTerm

type ArgsData r rTypeBind rTerm rSumItem
  = Rec.ArgsData ( actions :: Array Action | r ) rTypeBind rTerm rSumItem

type ArgsMatch r rTypeId rTerm rCaseItem
  = Rec.ArgsMatch ( actions :: Array Action | r ) rTypeId rTerm rCaseItem

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
            $ R.union
                { actions:
                    common (Lam args.lam) args
                      <> [ Action
                            { label: Just "unlambda"
                            , tooltip: makeExampleTooltip "unwrap a lambda, digging the variable" "fun x => e" "e[x -> ?]"
                            , triggers: [ ActionTrigger_Keypress keys.unlambda ]
                            , effect:
                                \{ this } -> do
                                  -- BUG doesn't correctly delete bound var
                                  st <- getState this
                                  let
                                    state = chTerm args.gamma args.alpha (deleteVar emptyChanges args.lam.termBind.termId) NoChange args.lam.body

                                    body' /\ holeEq = runState state Map.empty
                                  -- TODO: is it possible that the holeEq could apply to more than just body'?
                                  doChange this { ix: fromJust st.mb_ix, toReplace: ReplaceTerm body' RemoveArg }
                            }
                        ]
                      <> maybeArray
                          ( case args.lam.body of
                              Lam lam -> Just lam -- the body is also a lambda
                              _ -> Nothing
                          )
                          ( \lam' ->
                              Action
                                { label: Just "swaplambdas"
                                , tooltip: makeExampleTooltip "swap the order of nested lambdas" "fun x => fun y => e" "fun y => fun x => e"
                                , triggers: [ ActionTrigger_Keypress keys.swap ]
                                , effect:
                                    \{ this } -> do
                                      st <- getState this
                                      doChange this
                                        { ix: fromJust st.mb_ix
                                        , toReplace:
                                            ReplaceTerm
                                              ( Lam
                                                  { termBind: lam'.termBind
                                                  , body:
                                                      Lam
                                                        { termBind: args.lam.termBind
                                                        , body: lam'.body
                                                        , meta: lam'.meta
                                                        }
                                                  , meta: args.lam.meta
                                                  }
                                              )
                                              Swap
                                        }
                                }
                          )
                }
                args
    , neu:
        \args ->
          rec.neu
            $ R.union
                { actions:
                    common (Neu args.neu) args
                      <> [ Action
                            { label: Just "app"
                            , tooltip: makeExampleTooltip "apply a neutral form to an additional argument" "f" "f ?"
                            , triggers: [ ActionTrigger_Keypress keys.app ]
                            , effect:
                                \{ this } ->
                                  modifyState this \st ->
                                    -- given a neu: `f a : B -> C` where `f : A -> B -> C`
                                    -- try to unify output of `f a`, which is `B -> C` with a function type `?0 -> ?1`
                                    -- if can unify, then apply resulting hole sub to program
                                    -- apply typechange `RemoveArg` to `f`
                                    -- apply resulting hole sub to program
                                    let
                                      -- type of neu's var
                                      phi :: Type
                                      phi = lookupVarType args.neu.termId args.gamma

                                      -- output type of neu
                                      out :: Type
                                      out = neuOutputType phi args.neu

                                      -- fresh arrow type
                                      arr :: ArrowType
                                      arr = { dom: freshHoleType unit, cod: freshHoleType unit, meta: default }
                                    in
                                      case unifyType out (ArrowType arr) of
                                        Just holeSub ->
                                          let
                                            term' = subTerm holeSub st.term

                                            res :: Maybe (Term /\ IxDown /\ TypeChange /\ HoleEq)
                                            res =
                                              chAtTerm { term: term', gamma: default, alpha: st.type_ }
                                                ( ReplaceTerm
                                                    (Neu args.neu { argItems = List.snoc args.neu.argItems { term: freshHole unit, meta: default } })
                                                    RemoveArg
                                                )
                                                =<< st.mb_ix
                                          in
                                            case res of
                                              Just (term'' /\ ix' /\ _tc /\ holeEq) ->
                                                let
                                                  term''' = subTerm (restrictToFull holeEq) term''
                                                in
                                                  st
                                                    { term = term'''
                                                    , mb_ix = Just ix'
                                                    }
                                              Nothing -> st
                                        Nothing -> st
                            }
                        ]
                }
                args
    , let_:
        \args ->
          rec.let_
            $ R.union
                { actions:
                    common (Let args.let_) args
                      <> [ Action
                            { label: Just "unlet"
                            , tooltip: makeExampleTooltip "unwrap a let, digging the variable" "let x : A = a in e" "e[x -> ?]"
                            , triggers: [ ActionTrigger_Keypress keys.unlet ]
                            , effect:
                                \{ this } -> do
                                  -- BUG doesn't correctly delete bound var
                                  st <- getState this
                                  let
                                    state = chTerm args.gamma args.alpha (deleteVar emptyChanges args.let_.termBind.termId) NoChange args.let_.body

                                    body' /\ holeEq = runState state Map.empty
                                  -- TODO: is it possible that the holeEq could apply to more than just body'?
                                  doChange this { ix: fromJust st.mb_ix, toReplace: ReplaceTerm body' NoChange }
                            }
                        ]
                }
                args
    , buf:
        \args ->
          rec.buf
            $ R.union
                { actions:
                    common (Buf args.buf) args
                      <> [ Action
                            { label: Just "unbuffer"
                            , tooltip: makeExampleTooltip "unwrap a buffer, discarding the term" "buf a : A in e" "e"
                            , triggers: [ ActionTrigger_Keypress keys.unbuf ]
                            , effect:
                                \{ this } -> do
                                  st <- getState this
                                  doChange this { ix: fromJust st.mb_ix, toReplace: ReplaceTerm args.buf.body NoChange }
                            }
                        ]
                }
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
            $ R.union
                { actions:
                    common (Hole args.hole) args
                      <> [ Action
                            { label: Just "inlambda"
                            , tooltip: makeExampleTooltip "fill a hole with a lambda" "?" "fun ~ => ?"
                            , triggers: [ ActionTrigger_Keypress keys.inlambda ]
                            , effect:
                                \{ this } -> do
                                  st <- getState this
                                  case args.alpha of
                                    ArrowType arrow -> doChange this { ix: fromJust st.mb_ix, toReplace: ReplaceTerm (Lam { termBind: freshTermBind unit, body: freshHole unit, meta: default }) NoChange }
                                    _ -> pure unit -- cannot inlambda if type is not an arrow
                            }
                        , Action
                            { label: Just "variableQueryMode"
                            , tooltip: makeSimpleTooltip "enter variable query mode at a hole"
                            , triggers: [ ActionTrigger_Keypress keys.variableQueryMode ]
                            , effect: \{ this } -> modifyState this _ { mode = QueryMode { query: "", i: 0 } }
                            }
                        -- TODO: don;t need this anymore, because is handled by handleKey_QueryMode
                        {- 
                        , Action
                            { label: Just "edit"
                            , triggers: [ ActionTrigger_Keytype ]
                            , effect:
                                case _ of
                                  { this, mb_event: Just event } -> do
                                    st <- getState this
                                    case st.mode of
                                      QueryMode query -> do
                                        case handleKeytype_String event query of
                                          Just query' -> modifyState this _ { mode = QueryMode query' }
                                          Nothing -> pure unit
                                      _ -> pure unit
                                  _ -> pure unit
                            }
                          -}
                        ]
                }
                args
    }
  where
  common :: forall r. Term -> { visit :: Visit | r } -> Array Action
  common term args =
    [ Action
        { label: Just "enlambda"
        , tooltip: makeExampleTooltip "wrap a term in a lambda" "e" "fun ~ => e"
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
        , tooltip: makeExampleTooltip "replace a term with a hole" "e" "?"
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
        , tooltip: makeExampleTooltip "wrap a term in a let" "e" "let ~ = ? in e"
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
        , tooltip: makeExampleTooltip "wrap a term in a buffer" "e" "buf ? : ? in e"
        , effect:
            \{ this } ->
              args.visit.ix
                >>|= \ix ->
                    doChange this
                      { ix: toIxDown ix
                      , toReplace:
                          ReplaceTerm
                            ( Buf
                                { sign: freshHoleType unit
                                , impl: freshHole unit
                                , body: term
                                , meta: default
                                }
                            )
                            NoChange
                      }
        , triggers: [ ActionTrigger_Keypress keys.buf ]
        }
    , Action
        { label: Just "endata"
        , tooltip: makeExampleTooltip "wrap a term in a datatype definition" "e" "type ? = ? in e"
        , effect:
            \{ this } ->
              args.visit.ix
                >>|= \ix ->
                    doChange this
                      { ix: toIxDown ix
                      , toReplace:
                          ReplaceTerm
                            ( Data
                                { typeBind: freshTypeBind unit
                                , sumItems: mempty
                                , body: term
                                , meta: default
                                }
                            )
                            NoChange
                      }
        , triggers: [ ActionTrigger_Keypress keys.data_ ]
        }
    , Action
        { label: Just "pop"
        , tooltip: makeExampleTooltip "pop a term into a buffer" "e" "buf e : ? in ?"
        , effect:
            \{ this } ->
              args.visit.ix
                >>|= \ix ->
                    doChange this
                      { ix: toIxDown ix
                      , toReplace:
                          ReplaceTerm
                            ( Buf
                                { sign: freshHoleType unit
                                , impl: term
                                , body: freshHole unit
                                , meta: default
                                }
                            )
                            NoChange
                      }
        , triggers: [ ActionTrigger_Keypress keys.pop ]
        }
    , actionIndent args.visit.ix
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
recArgItem rec =
  Rec.recArgItem
    { argItem:
        \args ->
          rec.argItem
            $ R.union
                { actions:
                    [ actionIndent args.visit.ix
                    ]
                }
                args
    }

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
  Lacks "termId" r =>
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
                        , tooltip: makeSimpleTooltip "modify the name of a variable"
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
                        , tooltip: makeSimpleTooltip "modify the name of a variable"
                        , triggers: [ ActionTrigger_Keytype ]
                        , effect:
                            case _ of
                              { this, mb_event: Just event } -> do
                                args.visit.ix
                                  >>|= \ix ->
                                      handleKeytype_Name event (unwrap args.termBind.meta).name
                                        >>|= \name' -> do
                                            modifyState this \st ->
                                              -- Debug.trace ("res = " <> show (replaceTermBindNameAt name' (toIxDown ix) (SyntaxTerm st.term))) \_ ->
                                              st { term = fromJust $ toTerm $ fromJust $ replaceNameAt args.termBind SyntaxTermBind TermBindMetadata name' (toIxDown ix) (SyntaxTerm st.term) }
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

-- misc actions
actionIndent :: Maybe IxUp -> Action
actionIndent mb_ix =
  Action
    { label: Just "indent"
    , tooltip: Nothing
    , effect:
        \{ this } ->
          mb_ix
            >>|= \ix -> do
                Debug.traceM "indent"
                let
                  mb_step /\ ixIndentableParent = stepUpToNearestIndentableParentIxUp ix
                modifyState this \st ->
                  st { term = fromJust $ toTerm =<< indentSyntaxAt mb_step (toIxDown ixIndentableParent) (SyntaxTerm st.term) }
    , triggers: [ ActionTrigger_Keypress keys.indent ]
    }

-- tooltip
makeSimpleTooltip :: String -> Maybe (Array ReactElement)
makeSimpleTooltip desc = Just [ DOM.text desc ]

makeExampleTooltip :: String -> String -> String -> Maybe (Array ReactElement)
makeExampleTooltip desc lhs rhs =
  Just
    [ DOM.text desc
    , DOM.br'
    , DOM.div [ Props.className "example" ]
        [ DOM.span [ Props.className "code" ] [ DOM.text lhs ]
        , DOM.span [ Props.className "metacode" ] [ DOM.text "~~~>" ]
        , DOM.span [ Props.className "code" ] [ DOM.text rhs ]
        ]
    ]
