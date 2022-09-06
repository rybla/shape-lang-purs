module Language.Shape.Stlc.Rendering.ClickDragDrop where

import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Rendering.Types
import Language.Shape.Stlc.Rendering.Utilities
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Transition
import Prelude
import Prim hiding (Type)
import Data.Array (concat)
import Data.Maybe (Maybe(..))
import Language.Shape.Stlc.Types (This, TransitionEvent(..))
import React.DOM.Props as Props
import React.SyntheticEvent (stopPropagation)
import Unsafe (fromJust)

propsClickDragDrop :: This -> NodeProps -> Array Props.Props
propsClickDragDrop this props =
  concat
    -- selecting 
    [ maybeArray' do
        ix <- props.visit.ix
        pure
          $ Props.onClick \event ->
              doTransition
                { event: MouseTransitionEvent event, this }
                ( { label: "select"
                  , effect: select (toIxDown ix)
                  }
                )
    -- dragging and dropping
    , case props.syntax of
        Just (SyntaxTerm term) ->
          concat
            $ maybeArray (props.visit.ix) \ix ->
                -- start drag
                [ Props.onMouseDown \event -> do
                    stopPropagation event
                    doTransition
                      { event: MouseTransitionEvent event, this }
                      ( { label: "start drag"
                        , effect:
                            startDrag
                              { alpha: fromJust props.alpha
                              , gamma: props.gamma
                              , ix: toIxDown ix
                              , term
                              }
                        }
                      )
                -- stop drag (drop)
                , Props.onMouseUp \event -> do
                    doTransition
                      { event: MouseTransitionEvent event, this }
                      ( { label: "submit drag"
                        , effect: submitDrag (toIxDown ix)
                        }
                      )
                ----
                {-
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
                              Debug.traceM "[drop] breakpoint 1"
                              case fitsInHole alpha' (fromJust props.alpha) of
                                Just (nArgs /\ holeSub)
                                  | nArgs == 0 -> do
                                    Debug.traceM "[drop] breakpoint 2"
                                    modifyState this \st ->
                                      maybe st identity do
                                        Debug.traceM "[drop] breakpoint 3"
                                        -- -- TODO enable this because in theory it works right?
                                        -- -- mapM_ (doChange this) $ changesBetweenContexts props.gamma gamma' 
                                        st <- pure st { dragboard = Nothing }
                                        -- drop dragged term into its new index (here)
                                        st <-
                                          applyChange
                                            { ix: ixDown
                                            , toReplace: ReplaceTerm term' NoChange
                                            }
                                            st
                                        Debug.traceM "[drop] breakpoint 4"
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
                                                -- 
                                                -- BUG: however, this won't add
                                                -- the change to the History,
                                                -- since that's done by
                                                -- `applyChange`
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
                                        Debug.traceM "[drop] breakpoint 5"
                                        -- set index to be target of drop
                                        -- st <- pure st { mb_ix = Just ixDown }
                                        -- TODO: this doesn't actually set the index at all!!
                                        st <- pure st { mb_ix = Nothing }
                                        -- apply holeSub
                                        st <-
                                          pure
                                            $ st
                                                { term = subTerm holeSub st.term
                                                , type_ = subType holeSub st.type_
                                                }
                                        Debug.traceM "[drop] breakpoint 6"
                                        Debug.traceM $ "[drop] st.mb_ix = " <> show st.mb_ix
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
                  -}
                ]
        _ -> []
    ]
