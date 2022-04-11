module Language.Shape.Stlc.RenderingTypes where

import Data.Tuple.Nested
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)

import Data.List (List)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Effect (Effect)
import Language.Shape.Stlc.ChangeAtIndex (Change)
import Language.Shape.Stlc.Recursion.MetaContext (MetaContext)
import Language.Shape.Stlc.Typing (Context)
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
    , changeHistory :: ChangeHistory
    | r
    }

type ChangeHistory = List (Syntax /\ Change /\ DownwardIndex)

type Poststate
  = ( syntax_dragging :: Maybe Syntax
    , outline_parents :: List HTMLElement
    , actions :: Array Action 
    , environment :: 
      { metaGamma :: Maybe MetaContext
      , gamma :: Maybe Context
      , goal :: Maybe Type
      }
    )

type State
  = Prestate Poststate

type Given
  = { state :: State
    , render :: Effect ReactElement
    , componentDidMount :: Effect Unit
    }

type Environment
  = {} -- TODO

type This
  = React.ReactThis Props State

type Action
  = { label :: Maybe String
    , trigger :: Trigger
    , effect :: Effect Unit
    }

data Trigger
  = Trigger_Drop
  | Trigger_Keypress { key :: String }
  | Trigger_Paste
  | Trigger_Hover
  | Trigger_Button

derive instance eqTrigger :: Eq Trigger