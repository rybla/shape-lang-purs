module Language.Shape.Stlc.Transition where

import Data.Tuple.Nested
import Language.Shape.Stlc.Types
import Prelude
import Data.Array ((:))
import Data.Default (default)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Debug as Debug
import Effect (Effect)
import Effect.Class.Console as Console
import Language.Shape.Stlc.ChAtIndex (Change, chAtTerm)
import Language.Shape.Stlc.Changes (applyTC)
import Language.Shape.Stlc.Rendering.Token (SyntaxTheme)
import React (getState, modifyState)
import Undefined (undefined)
import Unsafe (error)

doTransition :: This -> Transition -> Effect Unit
doTransition this trans = do
  Console.log "+---------------------------------------------------------------"
  Console.log $ "transition: " <> trans.label
  state <- getState this
  case trans.effect { state, mb_event: Nothing } of
    Right st' -> modifyState this \_ -> st'
    Left err -> Console.log $ "[!] transition failure: " <> err
  Console.log "+---------------------------------------------------------------"

requireTopMode :: State -> TransitionM TopMode
requireTopMode st = case st.mode of
  TopMode topMode -> pure topMode
  _ -> Left "requires TopMode"

requireSelectMode :: State -> TransitionM SelectMode
requireSelectMode st = case st.mode of
  SelectMode selectMode -> pure selectMode
  _ -> Left "requires SelectMode"

requireQueryMode :: State -> TransitionM QueryMode
requireQueryMode st = case st.mode of
  QueryMode queryMode -> pure queryMode
  _ -> Left "requires QueryMode"

requireDragMode :: State -> TransitionM DragMode
requireDragMode st = case st.mode of
  DragMode dragMode -> pure dragMode
  _ -> Left "requires DragMode"

applyChange :: Change -> State -> TransitionM State
applyChange ch state = do
  Debug.traceM $ "change: " <> show ch
  selectMode <- requireSelectMode state
  history <- pure $ { program: state.program, change: ch } : state.history
  (term /\ ix /\ tych /\ holeEq) <- case chAtTerm
      { term: state.program.term, gamma: default, alpha: state.program.type_ }
      ch.toReplace
      ch.ix of
    Just res -> Right res
    Nothing -> Left "chAtTerm failure"
  pure
    state
      { mode = SelectMode { ix }
      , program =
        state.program
          { term = term
          , type_ = applyTC tych state.program.type_
          }
      , history = history
      }

applyAction :: Action -> State -> TransitionM State
applyAction = undefined

setSyntaxTheme :: SyntaxTheme -> State -> TransitionM State
setSyntaxTheme synthm state = do
  pure state { syntaxtheme = synthm }

setProgram :: Program -> State -> TransitionM State
setProgram program state =
  pure
    state
      { mode = TopMode {}
      , program = program
      }

clearHighlights :: State -> TransitionM Unit
clearHighlights st = error "TODO"
