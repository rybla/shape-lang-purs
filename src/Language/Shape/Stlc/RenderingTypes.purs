module Language.Shape.Stlc.RenderingTypes where

import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Syntax
import Prelude
import Data.List (List)
import Data.Maybe (Maybe)
import Effect (Effect)
import React (ReactElement)
import React as React
import Web.HTML (HTMLElement)

type Props
  = {}

-- transformations only operate on the prestate
type Prestate r
  = { module_ :: Module
    , ix_cursor :: DownwardIndex
    -- , environment :: Environment -- TODO
    | r
    }

type State
  = Prestate
      ( syntax_dragging :: Maybe Syntax
      , outline_parents :: List HTMLElement
      )

type Given
  = { state :: State
    , render :: Effect ReactElement
    , componentDidMount :: Effect Unit
    }

type Environment
  = {} -- TODO

type This
  = React.ReactThis Props State
