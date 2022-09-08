module Language.Shape.Stlc.Action where

import Data.Tuple.Nested
import Language.Shape.Stlc.ChAtIndex
import Language.Shape.Stlc.Changes
import Language.Shape.Stlc.Context
import Language.Shape.Stlc.Event.KeyboardEvent
import Language.Shape.Stlc.Hole
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Syntax.Metadata
import Language.Shape.Stlc.Types
import Prelude
import Prim hiding (Type)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.State (get, modify, runState, runStateT)
import Data.Default (default)
import Data.Either (Either(..))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect (Effect)
import Effect.Class.Console as Console
import KeyboardCursor (getLastIndex)
import KeyboardCursor as KeyboardCursor
import Language.Shape.Stlc.Key (keys)
import React (getState, modifyState)
import React.SyntheticEvent as React
import Unsafe (error)
import Web.Event.Event as Web
import Language.Shape.Stlc.ActionM as ActionM
import Language.Shape.Stlc.ActionM

-- | The only way an `Action` should be performed.
doAction :: { this :: This, event :: ActionTrigger } = Action -> Effect Unit
doAction { this, event } (Action action) = do
  -- doTransition { this, event } (unwrap act).transition
  Console.log "+---------------------------------------------------------------"
  Console.log $ "action: " <> action.label
  state <- getState this
  case event of
    KeyboardActionTrigger e -> do
      React.stopPropagation e
      React.preventDefault e
    MouseActionTrigger e -> do
      React.stopPropagation e
      React.preventDefault e
    WebActionTrigger e -> do
      Web.stopPropagation e
      Web.preventDefault e
    QuerySubmitActionTrigger -> pure unit
  void case runExcept (flip runStateT state (flip runReaderT event action.effect)) of
    Left err -> Console.log $ "[!] transition failure: " <> err
    Right (_ /\ state') -> do
      modifyState this \_ -> state'
  Console.log "+---------------------------------------------------------------"

-- | Proper `Action`s
gotoCursorTop =
  Action
    { label: "go to top"
    , tooltip: Just "move the cursor to the top of the program"
    , shortcuts: [ ActionShortcut_Keypress keys.cursorForwards ]
    , effect: setSelectIndex nilIxDown
    }

gotoCursorBottom =
  Action
    { label: "go to bottom"
    , tooltip: Just "move the cursor to the bottom of the program"
    , shortcuts: [ ActionShortcut_Keypress keys.cursorBackwards ]
    , effect:
        do
          state <- get
          setSelectIndex
            $ IxDown (getLastIndex (SyntaxTerm state.program.term))
    }

-- TODO: all of these actions should be implemented as `ActionM`s instead,
-- and then maybe they can be packaged up as actions here also, or just packaged
-- up at where they're given as arguments...
-- 
stepCursorForwards =
  Action
    { label: "step forwards"
    , tooltip: Just "move the cursor fowards in a tree walk"
    , shortcuts: [ ActionShortcut_Keypress keys.cursorForwards ]
    , effect:
        do
          state <- get
          selMode <- requireSelectMode
          ix <-
            maybeActionM "cannot step cursor forwards"
              $ KeyboardCursor.stepCursorForwards (SyntaxTerm state.program.term) selMode.ix
          setSelectIndex ix
    }

stepCursorBackwards =
  Action
    { label: "step backwards"
    , tooltip: Just "move the cursor backwards in a tree walk"
    , shortcuts: [ ActionShortcut_Keypress keys.cursorBackwards ]
    , effect:
        do
          state <- get
          selMode <- requireSelectMode
          ix <-
            maybeActionM "cannot step cursor backwards"
              $ KeyboardCursor.stepCursorBackwards (SyntaxTerm state.program.term) selMode.ix
          setSelectIndex ix
    }

undo =
  Action
    { label: "undo"
    , tooltip: makeSimpleTooltip "undo"
    , shortcuts: [ ActionShortcut_Keypress keys.undo ]
    , effect: ActionM.undo
    }

copy { clipboard } =
  Action
    { label: "copy"
    , tooltip: makeSimpleTooltip "copy"
    , shortcuts: [ ActionShortcut_Keypress keys.copy ]
    , effect: setClipboard clipboard
    }

unarrow { args } =
  Action
    { label: "unarrow"
    , tooltip: makeExampleTooltip "unwrap an arrow around a type" "A -> B" "B"
    , shortcuts: [ ActionShortcut_Keypress keys.unlambda ]
    , effect:
        do
          selMode <- requireSelectMode
          -- TODO: delete instances of bound term (ask jacob)
          applyChange
            { ix: selMode.ix
            , toReplace: ReplaceType args.arrowType.cod RemoveArg
            }
    }

digtype =
  Action
    { label: "dig type"
    , tooltip: makeExampleTooltip "replace a type with a hole" "A" "?"
    , shortcuts: [ ActionShortcut_Keypress keys.dig ]
    , effect:
        do
          selMode <- requireSelectMode
          let
            holeId = freshHoleId unit
          applyChange
            { ix: selMode.ix
            , toReplace: ReplaceType (HoleType { holeId, weakening: Set.empty, meta: default }) (Dig holeId)
            }
    }

swaparrow { args, arrow } =
  Action
    { label: "swap arrows"
    , tooltip: makeExampleTooltip "swap the order of nested arrows" "A -> B -> C" "B -> A -> C"
    , shortcuts: [ ActionShortcut_Keypress keys.swap ]
    , effect:
        do
          selMode <- requireSelectMode
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
    }

unlambda { args } =
  Action
    { label: "unlambda"
    , tooltip: makeExampleTooltip "unwrap a lambda, digging the variable" "fun x => e" "e[x -> ?]"
    , shortcuts: [ ActionShortcut_Keypress keys.unlambda ]
    , effect:
        do
          selMode <- requireSelectMode
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
    }

swaplambdas { args, lam' } =
  Action
    { label: "swap lambdas"
    , tooltip: makeExampleTooltip "swap the order of nested lambdas" "fun x => fun y => e" "fun y => fun x => e"
    , shortcuts: [ ActionShortcut_Keypress keys.swap ]
    , effect:
        do
          selMode <- requireSelectMode
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
    }

app { args } =
  Action
    { label: "app"
    , tooltip: makeExampleTooltip "apply a neutral form to an additional argument" "f" "f ?"
    , shortcuts: [ ActionShortcut_Keypress keys.app ]
    , effect:
        do
          state <- get
          -- given a neu: `f a : B -> C` where `f : A -> B -> C`
          -- try to unify output of `f a`, which is `B -> C` with a function type `?0 -> ?1`
          -- if can unify, then apply resulting hole sub to program
          -- apply typechange `RemoveArg` to `f`
          -- apply resulting hole sub to program
          selMode <- requireSelectMode
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
            maybeActionM "types did not unify"
              $ unifyType out (ArrowType arr)
          term <- pure $ subTerm holeSub state.program.term
          term /\ ix /\ _tc /\ holeEq <-
            maybeActionM "chAtTerm failed"
              $ chAtTerm { term, gamma: default, alpha: state.program.type_ }
                  ( ReplaceTerm
                      (Neu args.neu { argItems = List.snoc args.neu.argItems { term: freshHole unit, meta: default } })
                      RemoveArg
                  )
                  selMode.ix
          term <- pure $ subTerm (restrictToFull holeEq) term
          void
            $ modify
                _
                  { mode = SelectMode { ix }
                  , program { term = term }
                  }
    }

unapp { args } =
  Action
    { label: "unapp"
    , tooltip: makeExampleTooltip "apply a neutral form to one fewer arguments" "f a" "f"
    , shortcuts: [ ActionShortcut_Keypress keys.unapp ]
    , effect:
        do
          state <- get
          selMode <- requireSelectMode
          argItems' <- case List.unsnoc args.neu.argItems of
            Just { init } = pure init
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
            maybeActionM "chAtTerm failed"
              $ chAtTerm { term: state.program.term, gamma: default, alpha: state.program.type_ }
                  ( ReplaceTerm
                      (Neu args.neu { argItems = argItems' })
                      (InsertArg arr.dom)
                  )
                  selMode.ix
          term <- pure $ subTerm (restrictToFull holeEq) term
          void
            $ modify
                _
                  { mode = SelectMode { ix }
                  , program { term = term }
                  }
    }

unlet { args } =
  Action
    { label: "unlet"
    , tooltip: makeExampleTooltip "unwrap a let, digging the variable" "let x : A = a in e" "e[x -> ?]"
    , shortcuts: [ ActionShortcut_Keypress keys.unlet ]
    , effect:
        do
          selMode <- requireSelectMode
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
    }

unbuffer { args } =
  Action
    { label: "unbuffer"
    , tooltip: makeExampleTooltip "unwrap a buffer, discarding the term" "buf a : A in e" "e"
    , shortcuts: [ ActionShortcut_Keypress keys.unbuf ]
    , effect:
        do
          selMode <- requireSelectMode
          applyChange
            { ix: selMode.ix
            , toReplace: ReplaceTerm args.buf.body NoChange
            }
    }

undata { args } =
  Action
    { label: "undata"
    , tooltip: makeExampleTooltip "unwrap a data" "data A = ... in e" "e[A -> ?]"
    , shortcuts: [ ActionShortcut_Keypress keys.undata ]
    , effect:
        do
          selMode <- requireSelectMode
          error "TODO: undata"
    }

inlambda { args } =
  Action
    { label: "inlambda"
    , tooltip: makeExampleTooltip "fill a hole with a lambda" "?" "fun ~ => ?"
    , shortcuts: [ ActionShortcut_Keypress keys.inlambda ]
    , effect:
        case args.alpha of
          ArrowType arrow -> do
            selMode <- requireSelectMode
            applyChange
              { ix: selMode.ix
              , toReplace:
                  ReplaceTerm
                    (Lam { termBind: freshTermBind unit, body: freshHole unit, meta: default })
                    NoChange
              }
          _ -> throwError "cannot inlambda if type is not an arrow"
    }

enlambda { args, term } =
  Action
    { label: "enlambda"
    , tooltip: makeExampleTooltip "wrap a term in a lambda" "e" "fun ~ => e"
    , shortcuts: [ ActionShortcut_Keypress keys.lambda ]
    , effect:
        do
          selMode <- requireSelectMode
          applyChange
            { ix: selMode.ix
            , toReplace:
                ReplaceTerm
                  (Lam { termBind: { termId: freshTermId unit, meta: default }, body: term, meta: default })
                  (InsertArg (freshHoleType unit))
            }
    }

digterm { args } =
  Action
    { label: "dig term"
    , tooltip: makeExampleTooltip "replace a term with a hole" "e" "?"
    , shortcuts: [ ActionShortcut_Keypress keys.dig ]
    , effect:
        do
          selMode <- requireSelectMode
          applyChange
            { ix: selMode.ix
            , toReplace:
                ReplaceTerm
                  (Hole { meta: default })
                  NoChange
            }
    }

enlet { args, term } =
  Action
    { label: "enlet"
    , tooltip: makeExampleTooltip "wrap a term in a let" "e" "let ~ = ? in e"
    , shortcuts: [ ActionShortcut_Keypress keys.let_ ]
    , effect:
        do
          selMode <- requireSelectMode
          applyChange
            { ix: selMode.ix
            , toReplace:
                ReplaceTerm
                  (Let { termBind: freshTermBind unit, sign: freshHoleType unit, impl: freshHole unit, body: term, meta: default })
                  NoChange
            }
    }

enbuffer { args, term } =
  Action
    { label: "enbuffer"
    , tooltip: makeExampleTooltip "wrap a term in a buffer" "e" "buf ? : ? in e"
    , shortcuts: [ ActionShortcut_Keypress keys.buf ]
    , effect:
        do
          selMode <- requireSelectMode
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
    }

endata { args, term } =
  Action
    { label: "endata"
    , tooltip: makeExampleTooltip "wrap a term in a datatype definition" "e" "type ? = ? in e"
    , shortcuts: [ ActionShortcut_Keypress keys.data_ ]
    , effect:
        do
          selMode <- requireSelectMode
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
    }

pop { args, term } =
  Action
    { label: "pop"
    , tooltip: makeExampleTooltip "pop a term into a buffer" "e" "buf e : ? in ?"
    , shortcuts: [ ActionShortcut_Keypress keys.pop ]
    , effect:
        do
          selMode <- requireSelectMode
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
    }

editTypeBind { args, name } =
  Action
    { label: "edit type bind"
    , tooltip: makeSimpleTooltip "modify the name of a data"
    , shortcuts: [ ActionShortcut_Keytype ]
    , effect:
        do
          event <- ask
          state <- get
          selMode <- requireSelectMode
          e <- case event of
            WebActionTrigger e -> pure e
            _ -> throwError "wrong kind of event"
          name <-
            maybeActionM "invalid name modification"
              $ handleKeytype_Name e name
          syn <-
            maybeActionM "replaceNameAt failed"
              $ replaceNameAt
                  args.typeBind
                  SyntaxTypeBind
                  TypeBindMetadata
                  name
                  selMode.ix
                  (SyntaxTerm state.program.term)
          term <-
            maybeActionM "replaceNameAt resulted in a non-Term"
              $ toTerm syn
          setTermInPlace term
    }

editTermBind { args, name } =
  Action
    { label: "edit term bind"
    , tooltip: makeSimpleTooltip "modify the name of a term variable"
    , shortcuts: [ ActionShortcut_Keytype ]
    , effect:
        do
          event <- ask
          state <- get
          selMode <- requireSelectMode
          e <- case event of
            WebActionTrigger e -> pure e
            _ -> throwError "edit termBind must be spawned by an Event"
          name <-
            maybeActionM "TODO"
              $ handleKeytype_Name e name
          term <-
            maybeActionM "TODO"
              $ toTerm
              =<< replaceNameAt
                  args.termBind
                  SyntaxTermBind
                  TermBindMetadata
                  name
                  selMode.ix
                  (SyntaxTerm state.program.term)
          setTermInPlace term
    }

indent :: Action
indent =
  Action
    { label: "indent"
    , tooltip: Nothing
    , shortcuts: [ ActionShortcut_Keypress keys.indent ]
    , effect:
        do
          state <- get
          selMode <- requireSelectMode
          let
            mb_step /\ ixIndentableParent = stepUpToNearestIndentableParentIxUp (toIxUp selMode.ix)
          term <-
            maybeActionM "indexSyntaxAt failed"
              $ toTerm
              =<< indentSyntaxAt mb_step (toIxDown ixIndentableParent) (SyntaxTerm state.program.term)
          setTermInPlace term
    }

select ix =
  Action
    { label: "select"
    , tooltip: makeSimpleTooltip "select a node"
    , shortcuts: []
    , effect: ActionM.select ix
    }

deselect =
  Action
    { label: "deselect"
    , tooltip: Nothing
    , shortcuts: []
    , effect: ActionM.deselect
    }

startDrag { dragMode } =
  Action
    { label: "start drag"
    , tooltip: Nothing
    , shortcuts: []
    , effect: ActionM.startDrag dragMode
    }

submitDrag { ix, gamma, alpha, term } =
  Action
    { label: "submit drag"
    , tooltip: Nothing
    , shortcuts: []
    , effect: ActionM.submitDrag ix gamma alpha term
    }

loadProgram { program } =
  Action
    { label: "load program"
    , tooltip: Nothing
    , shortcuts: []
    , effect: ActionM.loadProgram program
    }

pasteDatatype { holeType, typeId } =
  Action
    { label: "paste datatype"
    , tooltip: Nothing
    , shortcuts: []
    , effect: ActionM.pasteDatatype holeType typeId
    }

pasteMatch { data_, typeId } =
  Action
    { label: "paste match"
    , tooltip: Nothing
    , shortcuts: []
    , effect: ActionM.pasteMatch data_ typeId
    }

pasteVar { env, type_, termId } =
  Action
    { label: "paste var"
    , tooltip: Nothing
    , shortcuts: []
    , effect: ActionM.pasteVar env type_ termId
    }
