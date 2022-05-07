module Language.Shape.Stlc.Rendering.Editor where

import Data.Tuple.Nested
import Prelude hiding (div)

import Effect (Effect)
import Language.Shape.Stlc.Rendering.Syntax (renderProgram)
import Language.Shape.Stlc.Types (Action(..), This)
import React (ReactElement)
import React.DOM (div)
import React.DOM.Props (className)
import Undefined (undefined)

-- | renderEditor
renderEditor :: This -> Effect ReactElement
renderEditor this = do
  elemsProgram /\ actions <- renderProgram this
  pure $ div [ className "editor" ]
    $ renderPanel this actions
    <> elemsProgram

-- | renderPanel
renderPanel :: This -> Array Action -> Array ReactElement
renderPanel this actions =
  [ div [ className "panel" ]
      []
  ]

renderEnvironment :: This -> Array ReactElement
renderEnvironment this =
  [ div [ className "environment" ]
      []
  ]

renderPalette :: This -> Array Action -> Array ReactElement
renderPalette this actions =
  [ div [ className "palette" ]
      []
  ]
