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
                        , effect:
                            submitDrag
                              (toIxDown ix)
                              props.gamma
                              (fromJust props.alpha)
                              term
                        }
                      )
                ]
        _ -> []
    ]
