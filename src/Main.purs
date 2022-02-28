module Main where

import Language.Shape.Stlc.Syntax
import Prelude
import App.Action (Action(..), handleAction)
import App.Rendering (render)
import App.State (State, initialState)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Partial.Unsafe (unsafePartial)
import Undefined (undefined)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

component :: forall query input output. H.Component query input output Aff
component =
  H.mkComponent
    { initialState: \_ -> initialState
    , render: render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Init }
    }
