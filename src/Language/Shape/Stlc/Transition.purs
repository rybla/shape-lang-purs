module Language.Shape.Stlc.Transition where

import Data.Tuple.Nested
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Rendering.Token
import Language.Shape.Stlc.Types
import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (get, modify, put, runStateT)
import Data.Array ((:))
import Data.Array as Array
import Data.Default (default)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse_)
import Debug as Debug
import Effect (Effect)
import Effect.Class.Console as Console
import Language.Shape.Stlc.ChAtIndex (Change, chAtTerm)
import Language.Shape.Stlc.Changes (applyTC)
import Language.Shape.Stlc.Rendering.Highlight (setHighlight)
import React (getState, modifyState)
import Unsafe (error)

doTransition :: { this :: This, event :: TransitionEvent } -> Transition -> Effect Unit
doTransition { this, event } trans = do
  Console.log "+---------------------------------------------------------------"
  Console.log $ "transition: " <> trans.label
  state <- getState this
  _ <- case runExcept (flip runStateT state (flip runReaderT event trans.effect)) of
    Left err -> Console.log $ "[!] transition failure: " <> err
    Right (_ /\ state') -> modifyState this \_ -> state'
  Console.log "+---------------------------------------------------------------"

doAction :: { this :: This, event :: TransitionEvent } -> Action -> Effect Unit
doAction { this, event } act = doTransition { this, event } (unwrap act).transition

requireTopMode :: TransitionM TopMode
requireTopMode = do
  st <- get
  case st.mode of
    TopMode topMode -> pure topMode
    _ -> throwError "requires TopMode"

requireSelectMode :: TransitionM SelectMode
requireSelectMode = do
  st <- get
  case st.mode of
    SelectMode selectMode -> pure selectMode
    _ -> throwError "requires SelectMode"

requireQueryMode :: TransitionM QueryMode
requireQueryMode = do
  st <- get
  case st.mode of
    QueryMode queryMode -> pure queryMode
    _ -> throwError "requires QueryMode"

requireDragMode :: TransitionM DragMode
requireDragMode = do
  st <- get
  case st.mode of
    DragMode dragMode -> pure dragMode
    _ -> throwError "requires DragMode"

setMode :: Mode -> TransitionM Unit
setMode mode = do
  -- TODO: do any special operations to transition between modes
  void $ modify \state -> state { mode = mode }

applyChange :: Change -> TransitionM Unit
applyChange ch = do
  state <- get
  Debug.traceM $ "change: " <> show ch
  selectMode <- requireSelectMode
  let
    history = { program: state.program, change: ch } : state.history
  (term /\ ix /\ tych /\ _holeEq) <-
    fromMaybe
      (throwError "chAtTerm failure")
      $ pure
      <$> chAtTerm
          { term: state.program.term, gamma: default, alpha: state.program.type_ }
          ch.toReplace
          ch.ix
  void
    $ modify
        _
          { mode = SelectMode { ix }
          , program =
            state.program
              { term = term
              , type_ = applyTC tych state.program.type_
              }
          , history = history
          }

setSyntaxTheme :: SyntaxTheme -> TransitionM Unit
setSyntaxTheme synthm = do
  void $ modify _ { syntaxtheme = synthm }

setProgram :: Program -> TransitionM Unit
setProgram program = do
  void $ modify _ { mode = TopMode {}, program = program }

setSelectIndex :: IxDown -> TransitionM Unit
setSelectIndex ix = do
  selMode <- requireSelectMode
  void $ modify _ { mode = SelectMode selMode { ix = ix } }

clearAllHighlights :: State -> Effect State
clearAllHighlights state = do
  traverse_ (setHighlight false) state.highlights
  pure state { highlights = [] }

undo :: TransitionM Unit
undo = do
  state <- get
  case Array.uncons state.history of
    Just { head: { program, change }, tail: history } ->
      void
        $ modify
            _
              { mode = SelectMode { ix: change.ix }
              , program = program
              , history = history
              , clipboard = Nothing
              }
    Nothing -> throwError "cannot undo at beginning of history"

deselect :: TransitionM Unit
deselect = do
  setMode (TopMode {})

setClipboard :: Clipboard -> TransitionM Unit
setClipboard clipboard = do
  void $ modify _ { clipboard = Just clipboard }

select :: IxDown -> TransitionM Unit
select ix = do
  setMode (SelectMode { ix })

startDrag :: DragMode -> TransitionM Unit
startDrag dragMode = do
  state <- get
  case state.mode of
    TopMode _ -> pure unit
    SelectMode _ -> pure unit
    _ -> throwError "requires top mode or select mode"
  setMode (DragMode dragMode)

submitDrag :: IxDown -> TransitionM Unit
submitDrag ixTarget = do
  state <- get
  dragModeSource <- requireDragMode
  -- setMode (TopMode {})
  error "TODO"
