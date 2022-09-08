module Language.Shape.Stlc.ActionM where

import Data.Tuple.Nested
import Language.Shape.Stlc.ChAtIndex
import Language.Shape.Stlc.Changes
import Language.Shape.Stlc.Context
import Language.Shape.Stlc.Hole
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Rendering.Token
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Types
import Prelude
import Prim hiding (Type)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, modify)
import Data.Array ((:))
import Data.Array as Array
import Data.Default (default)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse_)
import Debug as Debug
import Effect (Effect)
import Language.Shape.Stlc.CopyPasteBackend (createNeu, fitsInHole)
import Language.Shape.Stlc.Event.KeyboardEvent (handleKeytype_String)
import Language.Shape.Stlc.Rendering.Highlight (setHighlight)
import Language.Shape.Stlc.Syntax.Modify (modifySyntaxAt)
import Undefined (undefined)
import Unsafe (fromJust)

requireTopMode :: ActionM TopMode
requireTopMode = do
  st <- get
  case st.mode of
    TopMode topMode -> pure topMode
    _ -> throwError "requires TopMode"

requireSelectMode :: ActionM SelectMode
requireSelectMode = do
  st <- get
  case st.mode of
    SelectMode selectMode -> pure selectMode
    _ -> throwError "requires SelectMode"

requireDragMode :: ActionM DragMode
requireDragMode = do
  st <- get
  case st.mode of
    DragMode dragMode -> pure dragMode
    _ -> throwError "requires DragMode"

setMode :: Mode -> ActionM Unit
setMode mode = do
  -- TODO: do any special operations to transition between modes
  void $ modify \state -> state { mode = mode }

applyChange :: Change -> ActionM Unit
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
          { mode = SelectMode { ix, mb_query: Nothing }
          , program =
            state.program
              { term = term
              , type_ = applyTC tych state.program.type_
              }
          , history = history
          }

setSyntaxTheme :: SyntaxTheme -> ActionM Unit
setSyntaxTheme synthm = do
  void $ modify _ { syntaxtheme = synthm }

loadProgram :: Program -> ActionM Unit
loadProgram program = do
  void $ modify _ { mode = TopMode {}, program = program }

-- | Requires that the current mode is already valid for the new program (e.g.
-- | if in SelectMode, that the cursor index is still valid).
setProgramInPlace :: Program -> ActionM Unit
setProgramInPlace program = do
  void $ modify _ { program = program }

-- | Requires that the current mode is already valid for the new term (e.g. if
-- | in SelectMode, that the cursor index is still valid).
setTermInPlace :: Term -> ActionM Unit
setTermInPlace term = do
  void $ modify \state -> state { program = state.program { term = term } }

setSelectIndex :: IxDown -> ActionM Unit
setSelectIndex ix = do
  setMode (SelectMode { ix, mb_query: Nothing })

clearAllHighlights :: State -> Effect State
clearAllHighlights state = do
  traverse_ (setHighlight false) state.highlights
  pure state { highlights = [] }

undo :: ActionM Unit
undo = do
  state <- get
  case Array.uncons state.history of
    Just { head: { program, change }, tail: history } ->
      void
        $ modify
            _
              { mode = SelectMode { ix: change.ix, mb_query: Nothing }
              , program = program
              , history = history
              , clipboard = Nothing
              }
    Nothing -> throwError "cannot undo at beginning of history"

deselect :: ActionM Unit
deselect = do
  setMode (TopMode {})

setClipboard :: Clipboard -> ActionM Unit
setClipboard clipboard = do
  void $ modify _ { clipboard = Just clipboard }

select :: IxDown -> ActionM Unit
select ix = do
  setMode (SelectMode { ix, mb_query: Nothing })

startDrag :: DragMode -> ActionM Unit
startDrag dragMode = do
  state <- get
  case state.mode of
    TopMode _ -> pure unit
    SelectMode _ -> pure unit
    _ -> throwError "requires top mode or select mode"
  setMode (DragMode dragMode)

submitDrag :: IxDown -> Context -> Type -> Term -> ActionM Unit
submitDrag ix _gamma alpha term = do
  dragMode <- requireDragMode
  ----
  unless (isSuperIxDown dragMode.ix ix) do
    case term of
      Hole _ -> case fitsInHole dragMode.alpha alpha of
        Nothing -> do
          -- doesn't fit into hole
          pure unit
        Just (nArgs /\ _holeSub) -> do
          -- TODO: use holeSub
          case unsnocIxDown ix of
            -- at impl of a buf, so replace buf with its bod
            Just { ix: ix', step }
              | step == ixStepBuf.impl ->
                void
                  $ modify \state ->
                      state
                        { program
                          { term =
                            fromJust
                              $ toTerm
                              =<< modifySyntaxAt
                                  ( case _ of
                                      SyntaxTerm (Buf buf) -> Just (SyntaxTerm buf.body)
                                      _ -> Nothing
                                  )
                                  ix'
                                  (SyntaxTerm state.program.term)
                          }
                        }
            -- otherwise, just dig
            _ ->
              applyChange
                { ix: dragMode.ix, toReplace: ReplaceTerm (Hole { meta: default }) NoChange }
          -- paste term into hole at index
          if nArgs == 0 then
            applyChange
              { ix, toReplace: ReplaceTerm dragMode.term NoChange }
          else
            throwError "unimplemented: dropping a term into a hole that requires more arguments"
          setMode (TopMode {})
      _term -> do
        -- TODO: mapM_ (doChange this) $ changesBetweenContexts props.gamma gamma' 
        -- dig dragged term from its original index
        applyChange
          { ix: dragMode.ix
          , toReplace: ReplaceTerm (Hole { meta: default }) NoChange
          }
        -- wrap the drop location in a buffer, and put the dropped term in the buffer
        -- BUG: need to dig the dragged term from `term`
        applyChange
          { ix
          , toReplace:
              ReplaceTerm
                ( Buf
                    { sign: dragMode.alpha
                    , impl: dragMode.term
                    , body: term
                    , meta: default
                    }
                )
                NoChange
          }
        ----
        setMode (TopMode {})

pasteDatatype holeType typeId = do
  state <- get
  void $ requireSelectMode
  holeSub <- pure $ Map.singleton holeType.holeId (DataType { typeId, meta: default })
  term <- pure $ subTerm holeSub state.program.term
  type_ <- pure $ subType holeSub state.program.type_
  setProgramInPlace { term, type_ }

pasteMatch data_ typeId = do
  selMode <- requireSelectMode
  applyChange
    { ix: selMode.ix
    , toReplace:
        ReplaceTerm
          ( Match
              { typeId: typeId
              , term: Hole { meta: default }
              , caseItems:
                  ( \sumItem ->
                      { termBindItems: (\_ -> { termBind: freshTermBind unit, meta: default }) <$> sumItem.paramItems
                      , body: Hole { meta: default }
                      , meta: default
                      }
                  )
                    <$> data_.sumItems
              , meta: default
              }
          )
          NoChange
    }

pasteVar env type_ termId = do
  selMode <- requireSelectMode
  alpha <- maybeActionM "rendering environment doesn't have type" env.alpha
  nArgs /\ holeSub <-
    maybeActionM "variable doesn't fit in hole"
      $ fitsInHole type_ alpha
  term <- pure $ createNeu termId nArgs
  -- TODO: do I actually need to do the holeSub? or does that happen automatically via applyChange?
  applyChange
    { ix: selMode.ix
    , toReplace: ReplaceTerm term NoChange
    }

-- | You can only edit queries at a hole.
editQuery :: ActionM Unit
editQuery = do
  selMode <- requireSelectMode
  string /\ i <- case selMode.mb_query of
    Just query -> pure (query.string /\ query.i)
    -- if we don't have a query in progress, treat it as an empty string
    Nothing -> pure ("" /\ 0)
  e <-
    ask
      >>= case _ of
          WebActionTrigger e -> pure e
          _ -> throwError "editQuery expects a WebActionTrigger"
  case handleKeytype_String e string of
    Nothing -> pure unit
    Just string -> do
      Debug.traceM $ "new query string = " <> string
      setMode (SelectMode selMode { mb_query = Just { string, i } })
  updateQuery

-- | Makes sure that the query index and stuff stays valid.
updateQuery :: ActionM Unit
updateQuery = pure unit -- TODO

escapeQuery :: ActionM Unit
escapeQuery = do
  selMode <- requireSelectMode
  setMode (SelectMode selMode { mb_query = Nothing })
