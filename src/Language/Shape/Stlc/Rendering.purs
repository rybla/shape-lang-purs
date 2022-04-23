module Language.Shape.Stlc.Rendering where

import Language.Shape.Stlc.Types
import Prelude
import React

import Effect (Effect)
import Undefined (undefined)

type ReactElements
  = Array ReactElement

programClass :: ReactClass Props
programClass = component "Program" programComponent

programComponent :: ReactThis Props State -> Effect Given
programComponent this = undefined