module Language.Shape.Stlc.Rendering where

import Data.Tuple.Nested
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
import Language.Shape.Stlc.Initial (init1)
import Language.Shape.Stlc.Rendering.Editor (renderEditor)
import Language.Shape.Stlc.Rendering.Syntax (RenderEnvironment, emptyRenderEnvironment)
import React.DOM as DOM
import React.Ref as Ref
import Undefined (undefined)
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
  renderEnvironmentRef <- Ref.new emptyRenderEnvironment
  let
    state =
      { ix: mempty
      , term
      , type_
      , history: (term /\ type_) /\ []
      }
      where
      term /\ type_ = init1

    componentDidMount = do
      Console.log "componentDidMount"
      win <- window
      listener <- eventListener keyboardEventHandler
      addEventListener (EventType "keydown") listener false (toEventTarget win)

    keyboardEventHandler event = do
      Debug.traceM event
      let
        key = eventKey event
      Console.log $ "===[ keydown: " <> key <> " ]==============================="
      renEnv <- Ref.read renderEnvironmentRef
      Console.log $ "===[ actions ]==============================="
      Console.log $ intercalate "\n" <<< map ("- " <> _) <<< map show $ renEnv.actions
      case handleKey renEnv event of
        Just (Action action) -> do
          preventDefault event
          action.effect this
        Nothing -> pure unit

    render = do
      st <- getState this
      -- Console.log (show st)
      renEnv /\ elems <- renderEditor this
      Ref.write renEnv renderEnvironmentRef
      pure elems
  pure
    { state
    , render
    , componentDidMount
    }
