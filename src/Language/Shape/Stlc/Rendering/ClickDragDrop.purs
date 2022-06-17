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
import Language.Shape.Stlc.Types (Action(..), This)
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
          -- Debug.traceM "clicked on a node"
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
                    modifyState this (_ { dragboard = Just (toIxDown ix /\ props.gamma /\ fromJust props.alpha /\ term) })
                -- stop drag (drop)
                , Props.onMouseUp \event -> do
                    st <- getState this
                    case st.dragboard of
                      Just (ixDown_drag /\ gamma' /\ alpha' /\ term') -> do
                        stopPropagation event
                        case fitsInHole alpha' (fromJust props.alpha) of
                          Just (nArgs /\ holeSub)
                            | nArgs == 0 -> do
                              let
                                ixDown = toIxDown ix
                              -- assert that the drop index is not a superindex of the drag index
                              unless (isSuperIxDown ixDown_drag ixDown) do
                                modifyState this \st ->
                                  maybe st identity do
                                    -- -- TODO enable this because in theory it works right?
                                    -- -- mapM_ (doChange this) $ changesBetweenContexts props.gamma gamma' 
                                    st <- pure $ st { dragboard = Nothing }
                                    -- dig dragged term from its original index
                                    st <-
                                      applyChange
                                        { ix: ixDown_drag
                                        , toReplace: ReplaceTerm (Hole { meta: default }) NoChange
                                        }
                                        st
                                    -- drop dragged term into its new index (here)
                                    st <-
                                      applyChange
                                        { ix: ixDown_drag
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
                          -- -- TODO enable this because in theory it works right?
                          -- -- mapM_ (doChange this) $ changesBetweenContexts props.gamma gamma' 
                          -- -- Debug.traceM $ "mouse-up on a term; stop dragging and dropped: " <> show term'
                          -- -- remove from dragboard
                          -- modifyState this (_ { dragboard = Nothing })
                          -- doChanges this
                          --   -- dig dragged term from its original index
                          --   [ { ix: ixDown_drag
                          --     , toReplace: ReplaceTerm (Hole { meta: default }) NoChange
                          --     }
                          --   -- drop dragged term into its new index (here)
                          --   , { ix: ixDown
                          --     , toReplace: ReplaceTerm term' NoChange
                          --     }
                          --   ]
                          -- -- apply holeSub
                          -- modifyState this \st ->
                          --   st
                          --     { term = subTerm holeSub st.term
                          --     , type_ = subType holeSub st.type_
                          --     }
                          -- TODO: handle case where dragged thing is a neutral form, so can apply more arguments to it i.e. when nArgs > 0
                          _ -> do
                            -- doesn't fit into hole
                            pure unit
                      Nothing -> do
                        -- empty dragboard
                        pure unit
                ]
        -- Debug.trace ("mouse-downed on a " <> show props.syntax) \_ -> []
        _ -> []
    ]
