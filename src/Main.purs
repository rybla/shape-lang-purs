module Main where

import Data.Array
import Data.Maybe
import Effect
import Prelude
import Prelude
import App (appClass)
import Data.Traversable (sequence)
import Effect.Console (error)
import Partial.Unsafe (unsafePartial)
import React (ReactElement)
import React as React
import React.DOM as DOM
import React.DOM.Props as Props
import ReactDOM as ReactDOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.HTML.Window (document) as DOM

main :: Effect Unit
main =
  void do
    window <- DOM.window
    document <- DOM.document window
    let
      node = DOM.toNonElementParentNode document
    element <- do
      element <- DOM.getElementById "main" node
      pure $ unsafePartial $ fromJust element
    void $ ReactDOM.render (React.createLeafElement appClass {}) element
