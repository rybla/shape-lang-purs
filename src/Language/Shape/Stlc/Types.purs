module Language.Shape.Stlc.Types where

import Language.Shape.Stlc.ChAtIndex
import Language.Shape.Stlc.Context
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Key
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Language.Shape.Stlc.Rendering.Token (SyntaxTheme, defaultSyntaxTheme)
import React (ReactElement, ReactThis)
import React.SyntheticEvent (SyntheticKeyboardEvent, SyntheticMouseEvent)
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
  | DragMode DragMode

type TopMode
  = {}

type SelectMode
  = { ix :: IxDown, mb_query :: Maybe Query }

type Query
  = { string :: String, i :: Int }

type DragMode
  = { ix :: IxDown, gamma :: Context, alpha :: Type, term :: Term }

derive instance genericMode :: Generic Mode _

instance showMode :: Show Mode where
  show x = genericShow x

getStateIndex :: State -> Maybe IxDown
getStateIndex st = case st.mode of
  TopMode _ -> Nothing
  SelectMode { ix } -> Just ix
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
  { label :: String
  , tooltip :: Maybe String
  , queryable :: Boolean
  , shortcuts :: Array ActionShortcut
  , effect :: ActionM Unit
  }

type QueryResult
  = { action :: Action, n :: Int }

type Tooltip
  = Maybe String

derive instance newtypeAction :: Newtype Action _

data ActionShortcut
  = ActionShortcut_Keypress (Array Key)
  | ActionShortcut_Keytype

derive instance eqActionShortcut :: Eq ActionShortcut

instance showActionShortcut :: Show ActionShortcut where
  show (ActionShortcut_Keypress keys) = Array.intercalate ", " (show <$> keys)
  show ActionShortcut_Keytype = "keytype"

instance showAction :: Show Action where
  show (Action action) =
    action.label <> ": "
      <> Array.intercalate ", " (show <$> action.shortcuts)

data ActionTrigger
  = KeyboardActionTrigger SyntheticKeyboardEvent
  | MouseActionTrigger SyntheticMouseEvent
  | WebActionTrigger Event

type ActionM a
  = ReaderT
      { actionTrigger :: ActionTrigger
      , mb_queryResult :: Maybe { action :: Action, n :: Int }
      }
      ( StateT State
          ( ExceptT String
              Identity
          )
      )
      a

maybeActionM :: forall a. String -> Maybe a -> ActionM a
maybeActionM err = case _ of
  Nothing -> throwError err
  Just a -> pure a
