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
import Prim.Row (class Union)
import React (ReactElement)
import React as React
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.EventTarget (EventListener)
import Web.HTML (HTMLElement)
import Record as Record

type Props
  = {}

type ChangeHistory
  = List (Syntax /\ Change /\ DownwardIndex)

type State
  = { module_ :: Module
    , ix_cursor :: DownwardIndex
    , changeHistory :: ChangeHistory
    , syntax_dragging :: Maybe Syntax
    , elements_highlighted :: List HTMLElement
    , actions :: Array Action
    , actions_keymap :: Map String Action
    , environment ::
        { metaGamma :: Maybe MetaContext
        , gamma :: Maybe Context
        , goal :: Maybe Type
        }
    }

type Given
  = { state :: State
    , render :: Effect ReactElement
    , componentDidMount :: Effect Unit
    }

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

type Actions
  = Array Action

-- unsafeSubrecord :: forall row1 row2 row3. Union row1 row2 row3 => Record row3 -> Record row1
-- unsafeSubrecord = unsafeCoerce
-- liftPrestate :: (Prestate -> Prestate) -> (State -> State)
-- liftPrestate f st =
--     f (unsafeSubrecord st)
--         `Record.union`
--           { syntax_dragging: st.syntax_dragging
--           , outline_parents: st.outline_parents
--           , actions: st.actions
--           , environment: st.environment
--           }
-- liftPrestateM :: forall m. Monad m => (Prestate -> m Prestate) -> (State -> m State)
-- liftPrestateM f st = do 
--   pst <- f (unsafeSubrecord st)
--   pure $ pst `Record.union`
--     { syntax_dragging: st.syntax_dragging
--     , outline_parents: st.outline_parents
--     , actions: st.actions
--     , environment: st.environment
--     }
