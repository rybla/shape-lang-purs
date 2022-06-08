module Language.Shape.Stlc.Event.KeyboardEvent where

import Data.Array
import Language.Shape.Stlc.Key
import Prelude
import Data.Foldable (any)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Partial.Unsafe (unsafeCrashWith)
import Web.Event.Event (Event, EventType(..))

foreign import eventKey :: Event -> String

foreign import shiftKey :: Event -> Boolean

foreign import metaKey :: Event -> Boolean

foreign import ctrlKey :: Event -> Boolean

foreign import altKey :: Event -> Boolean

matchKeys :: Event -> Array Key -> Boolean
matchKeys event keys = any (matchKey event) keys

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
