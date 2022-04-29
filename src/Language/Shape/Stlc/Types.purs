module Language.Shape.Stlc.Types where

import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Language.Shape.Stlc.Context (Context(..))
import Language.Shape.Stlc.Index (IxDown(..))
import Language.Shape.Stlc.Key (Key(..))
import Language.Shape.Stlc.Metacontext (Metacontext(..))
import React (ReactElement, ReactThis, getState)

type Props
  = {}

type State
  = { term :: Term
    , ix :: IxDown
    , actions :: Array Action
    , environment :: Environment
    }

type Environment
  = { meta :: Maybe Metacontext
    , ctx :: Maybe Context
    , goal :: Maybe Type
    }

type Given
  = { state :: State
    , render :: Effect ReactElement
    , componentDidMount :: Effect Unit
    }

type This
  = ReactThis Props State

getState' :: This -> State
getState' this = unsafePerformEffect $ getState this

newtype Action
  = Action
  { label :: Maybe String
  , triggers :: Array ActionTrigger
  , effect :: ActionEffect
  }

type ActionEffect
  = This -> Effect Unit

data ActionTrigger
  = ActionTrigger_Drop
  | ActionTrigger_Drag
  | ActionTrigger_Hover
  | ActionTrigger_Paste
  | ActionTrigger_Keypress { keys :: Array Key }

derive instance eqActionTrigger :: Eq ActionTrigger
