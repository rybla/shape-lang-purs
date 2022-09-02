module Language.Shape.Stlc.Transition where

import Data.Tuple.Nested
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Rendering.Token
import Language.Shape.Stlc.Types
import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Array ((:))
import Data.Array as Array
import Data.Default (default)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse_)
import Debug as Debug
import Effect (Effect)
import Effect.Class.Console as Console
import Language.Shape.Stlc.ChAtIndex (Change, chAtTerm)
import Language.Shape.Stlc.Changes (applyTC)
import Language.Shape.Stlc.Rendering.Highlight (setHighlight)
import React (getState, modifyState)
import React.SyntheticEvent (SyntheticEvent)
import Undefined (undefined)
import Unsafe (error)

doTransition :: { this :: This, event :: TransitionEvent } -> Transition -> Effect Unit
doTransition { this, event } trans = do
  Console.log "+---------------------------------------------------------------"
  Console.log $ "transition: " <> trans.label
  state <- getState this
  case trans.effect { state, event } of
    Right st' -> modifyState this \_ -> st'
    Left err -> Console.log $ "[!] transition failure: " <> err
  Console.log "+---------------------------------------------------------------"

doAction :: { this :: This, event :: TransitionEvent } -> Action -> Effect Unit
doAction { this, event } act = doTransition { this, event } (unwrap act).transition

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

setMode :: Mode -> State -> TransitionM State
setMode mode state = do
  -- TODO: do any special operations to transition between modes
  pure state { mode = mode }

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
applyAction = error "TODO"

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

setSelectIndex :: IxDown -> State -> TransitionM State
setSelectIndex ix state = do
  selMode <- requireSelectMode state
  pure state { mode = SelectMode selMode { ix = ix } }

clearAllHighlights :: State -> Effect State
clearAllHighlights state = do
  traverse_ (setHighlight false) state.highlights
  pure state { highlights = [] }

undo :: State -> TransitionM State
undo state = case Array.uncons state.history of
  Just { head: { program, change }, tail: history } ->
    pure
      state
        { mode = SelectMode { ix: change.ix }
        , program = program
        , history = history
        , clipboard = Nothing
        }
  Nothing -> throwError "cannot undo at beginning of history"

deselect :: State -> TransitionM State
deselect state = do
  pure state { mode = TopMode {} }

setClipboard :: Clipboard -> State -> TransitionM State
setClipboard clipboard state = do
  pure state { clipboard = Just clipboard }

select :: IxDown -> State -> TransitionM State
select ix state = do
  setMode (SelectMode { ix }) state

startDrag :: DragMode -> State -> TransitionM State
startDrag dragMode state = do
  setMode (DragMode dragMode) state
