module Language.Shape.Stlc.Rendering.ClickDragDrop where

import Data.Tuple
import Data.Tuple.Nested
import Language.Shape.Stlc.ChAtIndex
import Language.Shape.Stlc.Changes
import Language.Shape.Stlc.Hole
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Metacontext
import Language.Shape.Stlc.Recursor.Action
import Language.Shape.Stlc.Recursor.Index
import Language.Shape.Stlc.Rendering.Token
import Language.Shape.Stlc.Rendering.Types
import Language.Shape.Stlc.Rendering.Utilities
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Control.Monad.State (State)
import Control.Monad.State as State
import Data.Array (concat)
import Data.Array as Array
import Data.Default (default)
import Data.Foldable (foldM)
import Data.List.Unsafe (List(..), reverse)
import Data.List.Unsafe as List
import Data.Map.Unsafe as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.OrderedSet (OrderedSet)
import Data.OrderedSet as OrderedSet
import Data.Set as Set
import Data.String (joinWith)
import Data.Traversable (sequence)
import Debug as Debub
import Debug as Debug
import Effect (Effect)
import Effect.Console as Console
import Language.Shape.Stlc.Context (Context(..))
import Language.Shape.Stlc.CopyPasteBackend (changesBetweenContexts, fitsInHole)
import Language.Shape.Stlc.Metadata (Name(..))
import Language.Shape.Stlc.Recursor.Action as Rec
import Language.Shape.Stlc.Recursor.Context as RecCtx
import Language.Shape.Stlc.Recursor.Index as RecIx
import Language.Shape.Stlc.Recursor.Metacontext as RecMeta
import Language.Shape.Stlc.Syntax.Modify (modifySyntaxAt)
import Language.Shape.Stlc.Types (Action(..), This, applyChange)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Union)
import React (ReactElement, getState, modifyState)
import React.DOM as DOM
import React.DOM.Props as Props
import React.SyntheticEvent (shiftKey, stopPropagation)
import Record as Record
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe (fromJust)

-- import Language.Shape.Rendering.Utilities
propsClickDragDrop :: This -> NodeProps -> Array Props.Props
propsClickDragDrop this props =
  concat
    -- selection
    [ maybeArray (props.visit.ix) \ix ->
        Props.onClick \event -> do
          Debug.traceM $ "clicked on a node: " <> show props.label -- DEBUG
          stopPropagation event
          -- select this node
          modifyState this (_ { mb_ix = Just (toIxDown ix) })
    -- dragging and dropping
    , case props.syntax of
        Just (SyntaxTerm term) ->
          concat
            $ maybeArray (props.visit.ix) \ix ->
                -- start drag
                [ Props.onMouseDown \event -> do
                    -- put into dragboard
                    -- Debug.traceM event
                    stopPropagation event
                    -- Debug.traceM $ "mouse-down on a term; start dragging: " <> show term
                    -- put this node in dragboard
                    modifyState this
                      ( _
                          { dragboard =
                            Just
                              { ix: toIxDown ix
                              , gamma: props.gamma
                              , alpha: fromJust props.alpha
                              , term
                              }
                          }
                      )
                -- stop drag (drop)
                , Props.onMouseUp \event -> do
                    st <- getState this
                    case st.dragboard of
                      -- Just (ixDown' /\ gamma' /\ alpha' /\ term') -> do
                      Just { ix: ixDown', gamma: _gamma', alpha: alpha', term: term' } -> do
                        stopPropagation event
                        let
                          ixDown = toIxDown ix
                        -- assert that the drop index is not a superindex of the drag index
                        unless (isSuperIxDown ixDown ixDown') do
                          case term of
                            Hole _ -> do
                              case fitsInHole alpha' (fromJust props.alpha) of
                                Just (nArgs /\ holeSub)
                                  | nArgs == 0 -> do
                                    modifyState this \st ->
                                      maybe st identity do
                                        -- -- TODO enable this because in theory it works right?
                                        -- -- mapM_ (doChange this) $ changesBetweenContexts props.gamma gamma' 
                                        st <- pure $ st { dragboard = Nothing }
                                        -- drop dragged term into its new index (here)
                                        st <-
                                          applyChange
                                            { ix: ixDown
                                            , toReplace: ReplaceTerm term' NoChange
                                            }
                                            st
                                        -- Dig dragged term from its original
                                        -- index. Except, if dragging from the
                                        -- implementation of a buffer, then
                                        -- unbuffer. Note that since this may
                                        -- unwrap a buffer, the indices of
                                        -- things might change, so that's why
                                        -- the drop must happen first, as it
                                        -- does above.
                                        st <- case unsnocIxDown ixDown' of
                                          Just { ix: ixDown'', step }
                                            | step == ixStepBuf.impl -> do
                                              Debub.traceM "when dropping a term, found that it came from the implementation of a buffer"
                                              term' <-
                                                -- use `modifySyntaxAt` instead
                                                -- of `applyChange` because
                                                -- `applyChange` requires
                                                -- already knowing the
                                                -- relacement term 
                                                toTerm
                                                  =<< modifySyntaxAt
                                                      ( case _ of
                                                          -- already know from matching on `unsnocIxDown ixDown'` that `ixDown''` is at a buffer
                                                          SyntaxTerm (Buf buf) -> Just (SyntaxTerm buf.body)
                                                          _ -> Nothing
                                                      )
                                                      ixDown''
                                                      (SyntaxTerm st.term)
                                              pure st { term = term' }
                                          -- ?a
                                          _ ->
                                            applyChange
                                              { ix: ixDown'
                                              , toReplace: ReplaceTerm (Hole { meta: default }) NoChange
                                              }
                                              st
                                        -- apply holeSub
                                        st <-
                                          pure
                                            $ st
                                                { term = subTerm holeSub st.term
                                                , type_ = subType holeSub st.type_
                                                }
                                        pure st
                                  -- TODO: handle case where dragged thing is a neutral form, so can apply more arguments to it i.e. when nArgs > 0
                                  | otherwise -> do
                                    -- extract TermId from pasted term
                                    unsafeCrashWith "[unimplemented] use createNeu to add arguments to pasted term if needed"
                                _ -> do
                                  -- doesn't fit into hole
                                  pure unit
                            term -> do
                              -- if the drop location is at a non-hole, then wrap it in a buffer with the dropped term
                              modifyState this \st ->
                                maybe st identity do
                                  -- -- TODO enable this because in theory it works right?
                                  -- -- mapM_ (doChange this) $ changesBetweenContexts props.gamma gamma' 
                                  st <- pure $ st { dragboard = Nothing }
                                  -- dig dragged term from its original index
                                  st <-
                                    applyChange
                                      { ix: ixDown'
                                      , toReplace: ReplaceTerm (Hole { meta: default }) NoChange
                                      }
                                      st
                                  -- wrap the drop location in a buffer, and put the dropped term in the buffer
                                  -- BUG need to dig the dragged term if it appears in `term` also. how best to do this? could do an additional dig on `term`, or could extract the new `term` from the state after digging
                                  st <-
                                    applyChange
                                      { ix: ixDown
                                      , toReplace:
                                          ReplaceTerm
                                            ( Buf
                                                { sign: alpha'
                                                , impl: term'
                                                , body: term
                                                , meta: default
                                                }
                                            )
                                            NoChange
                                      }
                                      st
                                  pure st
                      Nothing -> do
                        -- empty dragboard
                        pure unit
                ]
        -- Debug.trace ("mouse-downed on a " <> show props.syntax) \_ -> []
        _ -> []
    ]
