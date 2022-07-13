module Language.Shape.Stlc.Event.KeyboardEvent where

import Data.Array
import Data.Tuple.Nested
import Language.Shape.Stlc.Key
import Language.Shape.Stlc.Rendering.Types
import Prelude
import Prim hiding (Type)
import Control.Alternative (guard)
import Data.Char as Char
import Data.Foldable (any)
import Data.Foldable as Array
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), split)
import Data.String as String
import Data.String as String
import Data.String.CodePoints as CodePoints
import Debug as Debug
import Language.Shape.Stlc.Metadata (Name(..))
import Language.Shape.Stlc.Types (Action(..), ActionTrigger(..))
import Partial.Unsafe (unsafeCrashWith)
import Undefined (undefined)
import Unsafe (fromJust)
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
  Just { head: k, tail: mods } -> String.toLower k == String.toLower key && checkMods [ "Shift", "Meta", "Ctrl", "Alt" ] mods
  where
  checkMods :: Array String -> Array String -> Boolean
  checkMods mods opts =
    foldr
      ( \mod b ->
          ( if Array.elem mod opts then
              modVal mod -- if in the options, then mod must be enabled
            else
              not (modVal mod) -- if not in the options, then mod must be disabled
          )
            && b
      )
      true
      mods

  modVal :: String -> Boolean
  modVal = case _ of
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

handleKey :: RenderEnvironment -> Event -> Maybe (ActionTrigger /\ Action)
handleKey renEnv event =
  Debug.trace "handleKey.event:" \_ ->
    Debug.trace event \_ ->
      foldr
        ( \action ->
            maybe
              ( foldr
                  ( \trigger -> case trigger of
                      ActionTrigger_Keytype -> const $ Just (ActionTrigger_Keytype /\ action)
                      ActionTrigger_Keypress keys ->
                        maybe
                          ( do
                              guard (matchOneOfKeys event keys)
                              pure (trigger /\ action)
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

handleKeytype_Name :: Event -> Name -> Maybe Name
handleKeytype_Name event (Name mb_str) = (\str1 -> if str1 == "" then Name Nothing else Name (Just str1)) <$> mb_str1
  where
  str0 = maybe "" identity mb_str

  mb_str1 = handleKeytype_String event str0

handleKeytype_String :: Event -> String -> Maybe String
handleKeytype_String event str = go (eventKey event)
  where
  go key
    -- TODO: handle other special keys
    | key == "Backspace" && altKey event = Just ""
    | key == "Backspace" = Just $ String.take (String.length str - 1) str
    | key == "Space" = Just $ str <> " "
    | key `Array.elem` [ "Shift", "Meta", "Control", "Alt", "Tab", "ArrowLeft", "ArrowRight" ] = Nothing
    | otherwise = Just $ str <> key
