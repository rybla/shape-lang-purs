module Language.Shape.Stlc.Rendering where

import Data.Tuple.Nested
import Language.Shape.Stlc.Types
import Prelude hiding (div)
import React
import React.DOM
import Debug as Debug
import Effect (Effect)
import Effect.Console as Console
import Language.Shape.Stlc.Initial (init1)
import Language.Shape.Stlc.Rendering.Editor (renderEditor)
import Undefined (undefined)
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Window (toEventTarget)

foreign import eventKey :: Event -> String

type ReactElements
  = Array ReactElement

programClass :: ReactClass Props
programClass = component "Program" programComponent

programComponent :: ReactThis Props State -> Effect Given
programComponent this =
  pure
    { state
    , render
    , componentDidMount
    }
  where
  state =
    let
      term /\ type_ = init1
    in
      { ix: mempty
      , term
      , type_
      }

  componentDidMount = do
    Console.log "componentDidMount"
    win <- window
    listener <- eventListener keyboardEventHandler
    addEventListener (EventType "keydown") listener false (toEventTarget win)

  keyboardEventHandler event = do
    let
      key = eventKey event
    Console.log $ "===[ keydown: " <> key <> " ]==============================="
    pure unit

  render = do
    st <- getState this
    -- Console.log $ "st = " <> show st
    renderEditor this
