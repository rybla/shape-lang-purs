module App.Rendering where

import Prelude
import App.Action (Action(..))
import App.State (Console, State)
import Data.List as List
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Language.Shape.Stlc.Renderer (renderModule)
import Undefined (undefined)
import Web.UIEvent.KeyboardEvent

render :: forall cs m. State -> HH.HTML (H.ComponentSlot cs m Action) Action
render st =
  HH.div
    [ HP.class_ (HH.ClassName "app") ]
    [ renderModule st.module_
    , renderConsole st.console
    ]

renderConsole :: forall w i. Console -> HH.HTML w i
renderConsole console =
  HH.div
    [ HP.class_ (HH.ClassName "console") ]
    ( List.toUnfoldable
        <<< map
            ( \msg ->
                HH.div
                  [ HP.class_ (HH.ClassName "consoleMessage") ]
                  [ HH.text msg ]
            )
        $ console
    )
