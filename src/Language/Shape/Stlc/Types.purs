module Language.Shape.Stlc.Types where

import Data.Tuple.Nested
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)

import Data.Array (intercalate)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Language.Shape.Stlc.ChAtIndex (Change)
import Language.Shape.Stlc.Context (Context(..))
import Language.Shape.Stlc.Index (IxDown(..))
import Language.Shape.Stlc.Key (Key(..))
import Language.Shape.Stlc.Metacontext (Metacontext(..))
import React (ReactElement, ReactThis, getState)

type Props
  = {}

type State
  = { term :: Term
    , type_ :: Type
    , mb_ix :: Maybe IxDown
    , history :: History
    , clipboard :: Maybe (IxDown /\ Context /\ Type /\ Term)
    , dragboard :: Maybe (IxDown /\ Context /\ Type /\ Term)
    }

type History = (Term /\ Type) /\ Array Change

type Given
  = { state :: State
    , render :: Effect ReactElement
    , componentDidMount :: Effect Unit
    }

type This
  = ReactThis Props State

newtype Action
  = Action
  { label :: Maybe String
  , triggers :: Array ActionTrigger
  , effect :: ActionEffect
  }

derive instance newtypeAction :: Newtype Action _ 

type ActionEffect
  = This -> Effect Unit

data ActionTrigger
  = ActionTrigger_Drop
  | ActionTrigger_Drag
  | ActionTrigger_Hover
  | ActionTrigger_Paste
  | ActionTrigger_Keypress (Array Key)

derive instance eqActionTrigger :: Eq ActionTrigger

instance Show ActionTrigger where 
  show ActionTrigger_Drop = "drop"
  show ActionTrigger_Drag = "drag"
  show ActionTrigger_Hover = "hover"
  show ActionTrigger_Paste = "paste"
  show (ActionTrigger_Keypress keys) = "keys[" <> intercalate ", " (show <$> keys) <> "]"

instance Show Action where 
  show (Action action ) = 
    (case action.label of 
      Just str -> str <> ": "
      Nothing -> ""
    ) <>  intercalate ", " (show <$> action.triggers )