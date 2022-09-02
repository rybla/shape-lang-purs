module Language.Shape.Stlc.Action where

import Data.Tuple.Nested
import KeyboardCursor
import Language.Shape.Stlc.ChAtIndex
import Language.Shape.Stlc.Changes
import Language.Shape.Stlc.Context
import Language.Shape.Stlc.Event.KeyboardEvent
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Key
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Syntax.Metadata
import Language.Shape.Stlc.Transition
import Language.Shape.Stlc.Types
import Prelude
import Prim hiding (Type)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State (runState)
import Data.Default (default)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set as Set
import Language.Shape.Stlc.Hole (restrictToFull, subTerm, unifyType)
import Unsafe (error)

action :: _
action =
  { stepCursorForwards:
      Action
        { tooltip: Just "move the cursor fowards in a tree walk"
        , triggers: [ ActionTrigger_Keypress keys.cursorForwards ]
        , transition:
            { label: "stepCursorForwards"
            , effect:
                \{ state } -> do
                  selMode <- requireSelectMode state
                  ix <-
                    maybeTransitionM "cannot step cursor forwards"
                      $ stepCursorForwards (SyntaxTerm state.program.term) selMode.ix
                  setSelectIndex ix state
            }
        }
  , stepCursorBackwards:
      Action
        { tooltip: Just "move the cursor backwards in a tree walk"
        , triggers: [ ActionTrigger_Keypress keys.cursorBackwards ]
        , transition:
            { label: "stepCursorBackwards"
            , effect:
                \{ state } -> do
                  selMode <- requireSelectMode state
                  ix <-
                    maybeTransitionM "cannot step cursor backwards"
                      $ stepCursorBackwards (SyntaxTerm state.program.term) selMode.ix
                  setSelectIndex ix state
            }
        }
  , undo:
      Action
        { tooltip: makeSimpleTooltip "undo"
        , triggers: [ ActionTrigger_Keypress keys.undo ]
        , transition:
            { label: "undo"
            , effect: \{ state } -> undo state
            }
        }
  , copy:
      \{ clipboard } ->
        Action
          { tooltip: makeSimpleTooltip "copy"
          , triggers: [ ActionTrigger_Keypress keys.copy ]
          , transition:
              { label: "copy"
              , effect: \{ state } -> setClipboard clipboard state
              }
          }
  , unarrow:
      \{ args } ->
        Action
          { tooltip: makeExampleTooltip "unwrap an arrow around a type" "A -> B" "B"
          , triggers: [ ActionTrigger_Keypress keys.unlambda ]
          , transition:
              { label: "unarrow"
              , effect:
                  \{ state, event } -> do
                    selMode <- requireSelectMode state
                    -- TODO: delete instances of bound term (ask jacob)
                    applyChange
                      { ix: selMode.ix
                      , toReplace: ReplaceType args.arrowType.cod RemoveArg
                      }
                      state
              }
          }
  , digtype:
      Action
        { tooltip: makeExampleTooltip "replace a type with a hole" "A" "?"
        , triggers: [ ActionTrigger_Keypress keys.dig ]
        , transition:
            { label: "dig"
            , effect:
                \{ state, event } -> do
                  selMode <- requireSelectMode state
                  let
                    holeId = freshHoleId unit
                  applyChange
                    { ix: selMode.ix
                    , toReplace: ReplaceType (HoleType { holeId, weakening: Set.empty, meta: default }) (Dig holeId)
                    }
                    state
            }
        }
  , swaparrow:
      \{ args, arrow } ->
        Action
          { tooltip: makeExampleTooltip "swap the order of nested arrows" "A -> B -> C" "B -> A -> C"
          , triggers: [ ActionTrigger_Keypress keys.swap ]
          , transition:
              { label: "swaparrow"
              , effect:
                  \{ state, event } -> do
                    selMode <- requireSelectMode state
                    applyChange
                      { ix: selMode.ix
                      , toReplace:
                          ReplaceType
                            ( ArrowType
                                { dom: arrow.dom
                                , cod:
                                    ArrowType
                                      { dom: args.arrowType.dom
                                      , cod: arrow.cod
                                      , meta: arrow.meta
                                      }
                                , meta: args.arrowType.meta
                                }
                            )
                            Swap
                      }
                      state
              }
          }
  , unlambda:
      \{ args } ->
        Action
          { tooltip: makeExampleTooltip "unwrap a lambda, digging the variable" "fun x => e" "e[x -> ?]"
          , triggers: [ ActionTrigger_Keypress keys.unlambda ]
          , transition:
              { label: "unlambda"
              , effect:
                  \{ state, event } -> do
                    selMode <- requireSelectMode state
                    let
                      body' /\ holeEq =
                        runState
                          (chTerm args.body.gamma args.body.alpha (deleteVar emptyChanges args.lam.termBind.termId) NoChange args.lam.body)
                          Map.empty
                    -- TODO: is it possible that the holeEq could apply to more than just body'?
                    applyChange
                      { ix: selMode.ix
                      , toReplace: ReplaceTerm body' RemoveArg
                      }
                      state
              }
          }
  , swaplambdas:
      \{ args, lam' } ->
        Action
          { tooltip: makeExampleTooltip "swap the order of nested lambdas" "fun x => fun y => e" "fun y => fun x => e"
          , triggers: [ ActionTrigger_Keypress keys.swap ]
          , transition:
              { label: "swaplambdas"
              , effect:
                  \{ state, event } -> do
                    selMode <- requireSelectMode state
                    applyChange
                      { ix: selMode.ix
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
                      state
              }
          }
  , app:
      \{ args } ->
        Action
          { tooltip: makeExampleTooltip "apply a neutral form to an additional argument" "f" "f ?"
          , triggers: [ ActionTrigger_Keypress keys.app ]
          , transition:
              { label: "app"
              , effect:
                  \{ state, event } -> do
                    -- given a neu: `f a : B -> C` where `f : A -> B -> C`
                    -- try to unify output of `f a`, which is `B -> C` with a function type `?0 -> ?1`
                    -- if can unify, then apply resulting hole sub to program
                    -- apply typechange `RemoveArg` to `f`
                    -- apply resulting hole sub to program
                    selMode <- requireSelectMode state
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
                    holeSub <-
                      maybeTransitionM "types did not unify"
                        $ unifyType out (ArrowType arr)
                    term <- pure $ subTerm holeSub state.program.term
                    term /\ ix /\ _tc /\ holeEq <-
                      maybeTransitionM "chAtTerm failed"
                        $ chAtTerm { term, gamma: default, alpha: state.program.type_ }
                            ( ReplaceTerm
                                (Neu args.neu { argItems = List.snoc args.neu.argItems { term: freshHole unit, meta: default } })
                                RemoveArg
                            )
                            selMode.ix
                    term <- pure $ subTerm (restrictToFull holeEq) term
                    pure
                      state
                        { mode = SelectMode { ix }
                        , program { term = term }
                        }
              }
          }
  , unapp:
      \{ args } ->
        Action
          { tooltip: makeExampleTooltip "apply a neutral form to one fewer arguments" "f a" "f"
          , triggers: [ ActionTrigger_Keypress keys.unapp ]
          , transition:
              { label: "unapp"
              , effect:
                  \{ state, event } -> do
                    selMode <- requireSelectMode state
                    argItems' <- case List.unsnoc args.neu.argItems of
                      Just { init } -> pure init
                      Nothing -> throwError "can only try to unapp a neutral form with at least one argument"
                    -- given a neu `f a : B` where `f : A -> B`
                    -- try to unify this term's expected type, `B` with `A -> B`
                    -- if can unify, then apply resulting hole sub to program
                    -- apply typechange `AddArg` to `f`
                    -- apply resulting hole sub to program
                    let
                      -- type of neu's var
                      phi :: Type
                      phi = lookupVarType args.neu.termId args.gamma

                      -- output type of neu with one less arg
                      out :: Type
                      out = neuOutputType phi args.neu { argItems = argItems' }
                    arr <- case out of
                      ArrowType arr -> pure arr
                      _ -> throwError "can only try to unapp a neutral form that has an applicant of an arrow type"
                    term /\ ix /\ _tc /\ holeEq <-
                      maybeTransitionM "chAtTerm failed"
                        $ chAtTerm { term: state.program.term, gamma: default, alpha: state.program.type_ }
                            ( ReplaceTerm
                                (Neu args.neu { argItems = argItems' })
                                (InsertArg arr.dom)
                            )
                            selMode.ix
                    term <- pure $ subTerm (restrictToFull holeEq) term
                    pure
                      state
                        { mode = SelectMode { ix }
                        , program { term = term }
                        }
              }
          }
  , unlet:
      \{ args } ->
        Action
          { tooltip: makeExampleTooltip "unwrap a let, digging the variable" "let x : A = a in e" "e[x -> ?]"
          , triggers: [ ActionTrigger_Keypress keys.unlet ]
          , transition:
              { label: "unlet"
              , effect:
                  \{ state } -> do
                    selMode <- requireSelectMode state
                    let
                      body' /\ holeEq =
                        runState
                          (chTerm args.body.gamma args.body.alpha (deleteVar emptyChanges args.let_.termBind.termId) NoChange args.let_.body)
                          Map.empty
                    -- TODO: is it possible that the holeEq could apply to more than just body'?
                    applyChange
                      { ix: selMode.ix
                      , toReplace: ReplaceTerm body' NoChange
                      }
                      state
              }
          }
  , unbuffer:
      \{ args } ->
        Action
          { tooltip: makeExampleTooltip "unwrap a buffer, discarding the term" "buf a : A in e" "e"
          , triggers: [ ActionTrigger_Keypress keys.unbuf ]
          , transition:
              { label: "unbuffer"
              , effect:
                  \{ state } -> do
                    selMode <- requireSelectMode state
                    applyChange
                      { ix: selMode.ix
                      , toReplace: ReplaceTerm args.buf.body NoChange
                      }
                      state
              }
          }
  , undata:
      \{ args } ->
        Action
          { tooltip: makeExampleTooltip "unwrap a data" "data A = ... in e" "e[A -> ?]"
          , triggers: [ ActionTrigger_Keypress keys.undata ]
          , transition:
              { label: "undata"
              , effect:
                  \{ state } -> do
                    selMode <- requireSelectMode state
                    error "TODO: undata"
              }
          }
  , inlambda:
      \{ args } ->
        Action
          { tooltip: makeExampleTooltip "fill a hole with a lambda" "?" "fun ~ => ?"
          , triggers: [ ActionTrigger_Keypress keys.inlambda ]
          , transition:
              { label: "inlambda"
              , effect:
                  \{ state } -> case args.alpha of
                    ArrowType arrow -> do
                      selMode <- requireSelectMode state
                      applyChange
                        { ix: selMode.ix
                        , toReplace:
                            ReplaceTerm
                              (Lam { termBind: freshTermBind unit, body: freshHole unit, meta: default })
                              NoChange
                        }
                        state
                    _ -> throwError "cannot inlambda if type is not an arrow"
              }
          }
  , enlambda:
      \{ args, term } ->
        Action
          { tooltip: makeExampleTooltip "wrap a term in a lambda" "e" "fun ~ => e"
          , triggers: [ ActionTrigger_Keypress keys.lambda ]
          , transition:
              { label: "enlambda"
              , effect:
                  \{ state } -> do
                    selMode <- requireSelectMode state
                    applyChange
                      { ix: selMode.ix
                      , toReplace:
                          ReplaceTerm
                            (Lam { termBind: { termId: freshTermId unit, meta: default }, body: term, meta: default })
                            (InsertArg (freshHoleType unit))
                      }
                      state
              }
          }
  , digterm:
      \{ args } ->
        Action
          { tooltip: makeExampleTooltip "replace a term with a hole" "e" "?"
          , triggers: [ ActionTrigger_Keypress keys.dig ]
          , transition:
              { label: "dig"
              , effect:
                  \{ state } -> do
                    selMode <- requireSelectMode state
                    applyChange
                      { ix: selMode.ix
                      , toReplace:
                          ReplaceTerm
                            (Hole { meta: default })
                            NoChange
                      }
                      state
              }
          }
  , enlet:
      \{ args, term } ->
        Action
          { tooltip: makeExampleTooltip "wrap a term in a let" "e" "let ~ = ? in e"
          , triggers: [ ActionTrigger_Keypress keys.let_ ]
          , transition:
              { label: "enlet"
              , effect:
                  \{ state } -> do
                    selMode <- requireSelectMode state
                    applyChange
                      { ix: selMode.ix
                      , toReplace:
                          ReplaceTerm
                            (Let { termBind: freshTermBind unit, sign: freshHoleType unit, impl: freshHole unit, body: term, meta: default })
                            NoChange
                      }
                      state
              }
          }
  , enbuffer:
      \{ args, term } ->
        Action
          { tooltip: makeExampleTooltip "wrap a term in a buffer" "e" "buf ? : ? in e"
          , triggers: [ ActionTrigger_Keypress keys.buf ]
          , transition:
              { label: "enbuffer"
              , effect:
                  \{ state } -> do
                    selMode <- requireSelectMode state
                    applyChange
                      { ix: selMode.ix
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
                      state
              }
          }
  , endata:
      \{ args, term } ->
        Action
          { tooltip: makeExampleTooltip "wrap a term in a datatype definition" "e" "type ? = ? in e"
          , triggers: [ ActionTrigger_Keypress keys.data_ ]
          , transition:
              { label: "endata"
              , effect:
                  \{ state } -> do
                    selMode <- requireSelectMode state
                    applyChange
                      { ix: selMode.ix
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
                      state
              }
          }
  , pop:
      \{ args, term } ->
        Action
          { tooltip: makeExampleTooltip "pop a term into a buffer" "e" "buf e : ? in ?"
          , triggers: [ ActionTrigger_Keypress keys.pop ]
          , transition:
              { label: "pop"
              , effect:
                  \{ state } -> do
                    selMode <- requireSelectMode state
                    applyChange
                      { ix: selMode.ix
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
                      state
              }
          }
  , editTypeBind:
      \{ args, name } ->
        Action
          { tooltip: makeSimpleTooltip "modify the name of a data"
          , triggers: [ ActionTrigger_Keytype ]
          , transition:
              { label: "edit typeBind"
              , effect:
                  \{ state, event } -> do
                    selMode <- requireSelectMode state
                    e <- case event of
                      WebTransitionEvent e -> pure e
                      _ -> throwError "wrong kind of event"
                    name <-
                      maybeTransitionM "invalid name modification"
                        $ handleKeytype_Name e name
                    syn <-
                      maybeTransitionM "replaceNameAt failed"
                        $ replaceNameAt
                            args.typeBind
                            SyntaxTypeBind
                            TypeBindMetadata
                            name
                            selMode.ix
                            (SyntaxTerm state.program.term)
                    term <-
                      maybeTransitionM "replaceNameAt resulted in a non-Term"
                        $ toTerm syn
                    setProgram (state.program { term = term }) state
              }
          }
  , editTermBind:
      \{ args, name } ->
        Action
          { tooltip: makeSimpleTooltip "modify the name of a term variable"
          , triggers: [ ActionTrigger_Keytype ]
          , transition:
              { label: "edit termBind"
              , effect:
                  \{ state, event } -> do
                    selMode <- requireSelectMode state
                    e <- case event of
                      WebTransitionEvent e -> pure e
                      _ -> throwError "edit termBind must be spawned by an Event"
                    name <-
                      maybeTransitionM "TODO"
                        $ handleKeytype_Name e name
                    term <-
                      maybeTransitionM "TODO"
                        $ toTerm
                        =<< replaceNameAt
                            args.termBind
                            SyntaxTermBind
                            TermBindMetadata
                            name
                            selMode.ix
                            (SyntaxTerm state.program.term)
                    setProgram (state.program { term = term }) state
              }
          }
  , select:
      \{ ix } ->
        Action
          { tooltip: makeSimpleTooltip "click to select"
          , triggers: [ ActionTrigger_Click ]
          , transition:
              { label: "select"
              , effect: \{ state } -> select (toIxDown ix) state
              }
          }
  , startDrag: error "TODO"
  , submitDrag: error "TODO"
  , cancelDrag: error "TODO"
  }

-- tooltip
makeSimpleTooltip :: String -> Maybe String
makeSimpleTooltip = Just

makeExampleTooltip :: String -> String -> String -> Maybe String
makeExampleTooltip desc lhs rhs = Just $ desc <> ";  " <> lhs <> "  ~~>  " <> rhs
