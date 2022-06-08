module Language.Shape.Stlc.Event.KeyboardEvent where

import Data.Array
import Language.Shape.Stlc.Key
import Prelude
import Control.Alternative (guard)
import Data.Foldable (any)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), split)
import Language.Shape.Stlc.Rendering.Syntax (RenderEnvironment)
import Language.Shape.Stlc.Types (Action(..), ActionTrigger(..))
import Partial.Unsafe (unsafeCrashWith)
import Undefined (undefined)
import Web.Event.Event (Event, EventType(..))

foreign import eventKey :: Event -> String

foreign import shiftKey :: Event -> Boolean

foreign import metaKey :: Event -> Boolean

foreign import ctrlKey :: Event -> Boolean

foreign import altKey :: Event -> Boolean

matchOneOfKeys :: Event -> Array Key -> Boolean
matchOneOfKeys event keys = any (matchKey event) keys

matchKey :: Event -> Key -> Boolean
matchKey event (Key str) = case uncons (reverse $ split (Pattern " ") str) of
  Nothing -> false
  Just { head: k, tail: mods } -> k == key && all checkMod mods
  where
  checkMod :: String -> Boolean
  checkMod = case _ of
    "Shift" -> shift
    "Meta" -> meta
    "Ctrl" -> ctrl
    "Alt" -> alt
    mod -> unsafeCrashWith $ "Unrecognized key modifier: " <> show mod

  key = eventKey event

  shift = shiftKey event

  meta = metaKey event

  ctrl = ctrlKey event

  alt = altKey event

handleKey :: RenderEnvironment -> Event -> Maybe Action
handleKey renEnv event =
  foldr
    ( \action ->
        maybe
          ( foldr
              ( case _ of
                  ActionTrigger_Keypress keys ->
                    maybe
                      ( do
                          guard (matchOneOfKeys event keys)
                          pure action
                      )
                      pure
                  _ -> identity
              )
              Nothing
              (unwrap action).triggers
          )
          pure
    )
    Nothing
    renEnv.actions
