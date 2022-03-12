module App where

import Prelude
import Effect (Effect)
import Language.Shape.Stlc.Rendering (programClass)
import React as React
import React.DOM as DOM
import React.DOM.Props as Props

type AppProps
  = {}

type AppState
  = {}

type AppGiven
  = { state :: AppState, render :: Effect React.ReactElement }

appClass :: React.ReactClass AppProps
appClass = React.component "app" appComponent

appComponent :: React.ReactThis AppProps AppState -> Effect AppGiven
appComponent this =
  pure
    { state, render: render <$> React.getState this
    }
  where
  state :: AppState
  state = {}

  render :: AppState -> React.ReactElement
  render st =
    DOM.div
      [ Props.className "app" ]
      [ React.createLeafElement programClass {}
      ]
