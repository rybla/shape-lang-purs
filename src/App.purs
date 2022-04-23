module App where

import Prelude
import Effect (Effect)
import Language.Shape.Stlc.Rendering (programClass)
import React
import React.DOM as DOM
import React.DOM.Props as Props

type AppProps
  = {}

type AppState
  = {}

type AppGiven
  = { state :: AppState, render :: Effect ReactElement }

appClass :: ReactClass AppProps
appClass = component "app" appComponent

appComponent :: ReactThis AppProps AppState -> Effect AppGiven
appComponent this =
  pure
    { state, render: render <$> getState this
    }
  where
  state :: AppState
  state = {}

  render :: AppState -> ReactElement
  render st =
    DOM.div
      [ Props.className "app" ]
      [ createLeafElement programClass {}
      ]
