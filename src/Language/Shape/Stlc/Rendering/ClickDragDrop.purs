module Language.Shape.Stlc.Rendering.ClickDragDrop where

import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Rendering.Types
import Language.Shape.Stlc.Rendering.Utilities
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Data.Array (concat)
import Data.Maybe (Maybe(..))
import Language.Shape.Stlc.Action
import Language.Shape.Stlc.Types (This, ActionTrigger(..))
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
              doAction
                { event: MouseActionTrigger event, this }
                (select (toIxDown ix))
    -- dragging and dropping
    , case props.syntax of
        Just (SyntaxTerm term) ->
          concat
            $ maybeArray (props.visit.ix) \ix ->
                -- start drag
                [ Props.onMouseDown \event -> do
                    stopPropagation event
                    doAction
                      { event: MouseActionTrigger event, this }
                      ( startDrag
                          { dragMode:
                              { alpha: fromJust props.alpha
                              , gamma: props.gamma
                              , ix: toIxDown ix
                              , term
                              }
                          }
                      )
                -- stop drag (drop)
                , Props.onMouseUp \event -> do
                    doAction
                      { event: MouseActionTrigger event, this }
                      ( submitDrag
                          { ix: toIxDown ix
                          , gamma: props.gamma
                          , alpha: fromJust props.alpha
                          , term
                          }
                      )
                ]
        _ -> []
    ]
