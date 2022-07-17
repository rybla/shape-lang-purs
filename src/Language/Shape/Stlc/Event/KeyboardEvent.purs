module Language.Shape.Stlc.Event.KeyboardEvent where

import Control.Alternative
import Data.Foldable
import Data.Maybe
import Data.Newtype
import Data.Tuple.Nested
import Language.Shape.Stlc.Key
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Rendering.Types
import Language.Shape.Stlc.Types
import Prelude
import Prim hiding (Type)
import Data.Array as Array
import Data.Char as Char
import Data.String (Pattern(..), split)
import Data.String as String
import Data.String.CodePoints as CodePoints
import Debug as Debug
import Effect (Effect)
import Partial.Unsafe (unsafeCrashWith)
import React (modifyState)
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
matchKey event (Key str) = case Array.uncons (Array.reverse $ split (Pattern " ") str) of
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

handleKey :: State -> RenderEnvironment -> Event -> Maybe (ActionTrigger /\ Action)
handleKey st renEnv event = case st.mode of
  NormalMode -> handleKey_NormalMode renEnv event
  QueryMode q -> handleKey_QueryMode renEnv event q

handleKey_QueryMode :: RenderEnvironment -> Event -> { query :: String, i :: Int } -> Maybe (ActionTrigger /\ Action)
handleKey_QueryMode renEnv event q
  | matchOneOfKeys event keys.normalMode =
    let
      trigger = ActionTrigger_Keypress keys.normalMode
    in
      Just
        ( trigger
            /\ Action
                { label: Just "normalMode"
                , triggers: [ trigger ]
                , effect: \{ this } -> modifyState this _ { mode = NormalMode }
                }
        )
  | matchOneOfKeys event keys.enter =
    let
      trigger = ActionTrigger_Keypress keys.submitQueryvariableMode
    in
      Just
        ( trigger
            /\ Action
                { label: Just "submitQueryvariableMode"
                , triggers: [ trigger ]
                , effect: \{ this } -> modifyState this _ { mode = NormalMode } -- TODO: puts the queried variable into hole; maybe QueryVariableMode needs to hold the topmost variable that's been queried?
                }
        )
  | otherwise =
    let
      trigger = ActionTrigger_Keytype
    in
      Just
        ( trigger
            /\ Action
                { label: Just "edit"
                , triggers: [ trigger ]
                , effect:
                    \{ this } -> case handleKeytype_String event q.query of
                      Just query -> modifyState this _ { mode = QueryMode { query, i: 0 } }
                      Nothing -> pure unit
                }
        )

handleKey_NormalMode :: RenderEnvironment -> Event -> Maybe (ActionTrigger /\ Action)
handleKey_NormalMode renEnv event =
  Debug.trace event \_ ->
    foldr
      ( \action ->
          maybe
            ( foldr
                ( \trigger -> case trigger of
                    -- overrides any previously captured keypress. (or
                    -- keytype, but there should only be one keytype trigger
                    -- available at a time)
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
    | key `Array.elem` keysIgnore = Nothing
    | otherwise = Just $ str <> key

  keysIgnore =
    [ "Shift"
    , "Meta"
    , "Control"
    , "Alt"
    , "Tab"
    , "ArrowLeft"
    , "ArrowRight"
    , "ArrowUp"
    , "ArrowDown"
    , "Escape"
    , "Enter"
    ]
