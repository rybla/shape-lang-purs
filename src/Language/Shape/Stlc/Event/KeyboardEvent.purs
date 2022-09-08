module Language.Shape.Stlc.Event.KeyboardEvent where

import Data.Foldable
import Data.Maybe
import Language.Shape.Stlc.Key
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Rendering.Types
import Language.Shape.Stlc.Types
import Prelude
import Prim hiding (Type)

import Data.Array as Array
import Data.Newtype (unwrap)
import Data.String (Pattern(..), split)
import Data.String as String
import Debug as Debug
import Partial.Unsafe (unsafeCrashWith)
import Web.Event.Event (Event)

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

shouldPreventDefault :: Event -> Boolean
shouldPreventDefault event =
  -- blacklist of which keys should be prevented default
  not
    $ matchOneOfKeys event
        [ Key "Meta Tab"
        , Key "Meta q"
        , Key "Meta l"
        , Key "Meta r"
        , Key "Ctrl l"
        , Key "Ctrl r"
        , Key ("Meta Shift {")
        , Key ("Meta Shift }")
        ]

-- find an action in environment that is triggered by event
handleKey :: RenderEnvironment -> Event -> Maybe Action
handleKey env event = do
  Debug.traceM $ "actions: " <> intercalate ", " ((_.label <<< unwrap) <$> env.actions)
  Array.find
    ( \(Action action) ->
        any
          ( case _ of
              ActionShortcut_Keypress keys -> matchOneOfKeys event keys
              ActionShortcut_Keytype -> true
              _ -> false
          )
          action.triggers
    )
    env.actions

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
