module Main where

import App.Action
import App.State
import Language.Shape.Stlc.Syntax
import Prelude
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Language.Shape.Stlc.Renderer (renderModule)
import Partial.Unsafe (unsafePartial)
import Undefined (undefined)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

component :: forall query input output m. H.Component query input output m
component =
  H.mkComponent
    { initialState:
        \_ -> { module_: initialModule }
    , render: render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. State -> HH.HTML (H.ComponentSlot cs m Action) Action
render st =
  HH.div
    [ HP.class_ (HH.ClassName "app") ]
    [ renderModule st.module_ ]

handleAction :: forall cs output m. Action -> H.HalogenM State Action cs output m Unit
handleAction = case _ of
  SetModule mb_module -> case mb_module of
    Just module_ -> H.modify_ (_ { module_ = module_ })
    Nothing -> H.modify_ (\st -> st)
  SetPalette _ -> H.modify_ (\st -> st) -- TODO
