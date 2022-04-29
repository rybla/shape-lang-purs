module Language.Shape.Stlc.Rendering.Editor where

import Prelude hiding (div)
import Language.Shape.Stlc.Rendering.Syntax (renderProgram)
import Language.Shape.Stlc.Types (This)
import React (ReactElement)
import React.DOM (div)
import React.DOM.Props (className)
import Undefined (undefined)

-- | renderEditor
renderEditor :: This -> ReactElement
renderEditor this =
  div [ className "editor" ]
    $ renderPanel this
    <> renderProgram this

-- | renderPanel
renderPanel :: This -> Array ReactElement
renderPanel this =
  [ div [ className "panel" ]
      []
  ]

renderEnvironment :: This -> Array ReactElement
renderEnvironment this =
  [ div [ className "environment" ]
      []
  ]

renderPalette :: This -> Array ReactElement
renderPalette this =
  [ div [ className "palette" ]
      []
  ]
