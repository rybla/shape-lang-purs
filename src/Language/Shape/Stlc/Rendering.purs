module Language.Shape.Stlc.Rendering where

import Data.Tuple.Nested
import Language.Shape.Stlc.Rendering.Syntax
import Language.Shape.Stlc.Rendering.Types
import Language.Shape.Stlc.Types
import Prelude hiding (div)
import React
import Data.Array (intercalate)
import Data.Maybe (Maybe(..))
import Debug as Debug
import Effect (Effect)
import Effect.Console as Console
import Effect.Ref as Ref
import Language.Shape.Stlc.Event.KeyboardEvent (eventKey, handleKey)
import Language.Shape.Stlc.Example.Basic as Basic
import Language.Shape.Stlc.Initial (init1)
import Language.Shape.Stlc.Rendering.Editor (renderEditor)
import Language.Shape.Stlc.Rendering.Menu (renderMenubar)
import React.DOM as DOM
import React.Ref as Ref
import Unsafe (error)
import Web.Event.Event (Event, EventType(..), preventDefault)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Window (toEventTarget)

type ReactElements
  = Array ReactElement

programClass :: ReactClass Props
programClass = component "Program" programComponent

programComponent :: ReactThis Props State -> Effect Given
programComponent this = do
  let
    state = initState Basic.program
  renderEnvironmentRef <- Ref.new (emptyRenderEnvironment state)
  let
    componentDidMount = do
      Console.log "componentDidMount"
      win <- window
      listener <- eventListener keyboardEventHandler
      addEventListener (EventType "keydown") listener false (toEventTarget win)

    keyboardEventHandler event = do
      renEnv <- Ref.read renderEnvironmentRef
      st <- getState this
      case handleKey st renEnv event of
        Just (trigger /\ Action action) -> do
          preventDefault event
          -- action.effect { this, mb_event: Just event, trigger }
          error "TODO"
        Nothing -> pure unit

    render = do
      st <- getState this
      resMenubar <- renderMenubar this
      renEnv /\ resEditor <- renderEditor this
      Ref.write renEnv renderEnvironmentRef
      pure $ DOM.div' (resMenubar <> resEditor)
  pure
    { state
    , render
    , componentDidMount
    }
