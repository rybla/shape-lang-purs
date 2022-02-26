module Main where

import Language.Shape.Stlc.Syntax
import Prelude
import Data.List as List
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Language.Shape.Stlc.Renderer (renderModule)
import Partial.Unsafe (unsafePartial)
import Undefined (undefined)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

type State
  = { module_ :: Module }

data Action
  = Pass

initialModule :: Module
initialModule =
  Module
    $ List.singleton
    $ DataDefinition (TypeName "Nat") (TypeId 0)
    $ List.fromFoldable
        [ Constructor (TermName "Zero") (TermId 1) List.Nil
        , Constructor (TermName "Suc") (TermId 2)
            $ List.singleton (Tuple (TermName "n") (BaseType (DataType (TypeId 0))))
        ]

component :: forall query input output m. H.Component query input output m
component =
  H.mkComponent
    { initialState:
        \_ -> { module_: initialModule }
    , render: unsafePartial render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. Partial => State -> HH.HTML (H.ComponentSlot cs m Action) Action
render st = HH.div_ [ renderModule st.module_ ]

handleAction :: forall cs output m. Action -> H.HalogenM State Action cs output m Unit
handleAction = case _ of
  Pass -> H.modify_ identity
