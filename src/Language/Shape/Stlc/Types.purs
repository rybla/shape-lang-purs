module Language.Shape.Stlc.Types where

import Data.Tuple.Nested
import Language.Shape.Stlc.ChAtIndex
import Language.Shape.Stlc.Context
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Key
import Language.Shape.Stlc.Metacontext
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Data.Array ((:))
import Data.Array as Array
import Data.Default (default)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Debug as Debug
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Language.Shape.Stlc.Changes (applyTC)
import Language.Shape.Stlc.Rendering.Token (SyntaxTheme(..), defaultSyntaxTheme, expandedSyntaxTheme)
import React (ReactElement, ReactThis, getState, modifyState)
import React.SyntheticEvent (SyntheticEvent, SyntheticKeyboardEvent, SyntheticMouseEvent)
import Undefined (undefined)
import Web.Event.Event (Event)
import Web.HTML (HTMLElement)

type Props
  = {}

type State
  = { mode :: Mode
    , program :: Program
    , history :: History
    , clipboard :: Maybe Clipboard
    , highlights :: Array HTMLElement
    , syntaxtheme :: SyntaxTheme
    , colortheme :: String
    }

type Program
  = { term :: Term, type_ :: Type }

type Clipboard
  = { ix :: IxDown, gamma :: Context, alpha :: Type, term :: Term }

initSyntaxtheme :: SyntaxTheme
initSyntaxtheme = defaultSyntaxTheme

initColortheme :: String
initColortheme = "default-light"

initState :: Program -> State
initState program =
  { mode: TopMode {}
  , program
  , history: emptyHistory
  , clipboard: Nothing
  , highlights: []
  , syntaxtheme: initSyntaxtheme
  , colortheme: initColortheme
  }

data Mode
  = TopMode TopMode
  | SelectMode SelectMode
  | QueryMode QueryMode
  | DragMode DragMode

type TopMode
  = {}

type SelectMode
  = { ix :: IxDown }

type QueryMode
  = { ix :: IxDown, query :: String, i :: Int }

type DragMode
  = { ix :: IxDown, gamma :: Context, alpha :: Type, term :: Term }

derive instance genericMode :: Generic Mode _

instance showMode :: Show Mode where
  show x = genericShow x

getStateIndex :: State -> Maybe IxDown
getStateIndex st = case st.mode of
  TopMode _ -> Nothing
  SelectMode { ix } -> Just ix
  QueryMode { ix } -> Just ix
  DragMode _ -> Nothing

type History
  = Array HistoryItem

type HistoryItem
  = { program :: Program, change :: Change }

emptyHistory :: History
emptyHistory = []

type Given
  = { state :: State
    , render :: Effect ReactElement
    , componentDidMount :: Effect Unit
    }

type This
  = ReactThis Props State

newtype Action
  = Action
  { tooltip :: Maybe String
  , triggers :: Array ActionTrigger
  , transition :: Transition
  }

derive instance newtypeAction :: Newtype Action _

data ActionTrigger
  = ActionTrigger_Drop
  | ActionTrigger_Drag
  | ActionTrigger_Hover
  | ActionTrigger_Paste
  | ActionTrigger_Click
  | ActionTrigger_Keypress (Array Key)
  | ActionTrigger_Keytype

derive instance eqActionTrigger :: Eq ActionTrigger

instance showActionTrigger :: Show ActionTrigger where
  show ActionTrigger_Drop = "drop"
  show ActionTrigger_Drag = "drag"
  show ActionTrigger_Hover = "hover"
  show ActionTrigger_Paste = "paste"
  show ActionTrigger_Click = "click"
  show (ActionTrigger_Keypress keys) = Array.intercalate ", " (show <$> keys)
  show ActionTrigger_Keytype = "keytype"

instance showAction :: Show Action where
  show (Action action) =
    action.transition.label <> ": "
      <> Array.intercalate ", " (show <$> action.triggers)

-- | The type of transitions over the State. The `Transition`s defined in this
-- | file should be the _only_ ways that you modify the state; do not modify the
-- | state directly by updating its fields.
type Transition
  = { label :: String
    , effect :: TransitionM Unit
    }

data TransitionEvent
  = KeyboardTransitionEvent SyntheticKeyboardEvent
  | MouseTransitionEvent SyntheticMouseEvent
  | WebTransitionEvent Event
  | QuerySubmitTransitionEvent

type TransitionM a
  = ReaderT TransitionEvent (StateT State (ExceptT String Identity)) a

maybeTransitionM :: forall a. String -> Maybe a -> TransitionM a
maybeTransitionM err = case _ of
  Nothing -> throwError err
  Just a -> pure a
