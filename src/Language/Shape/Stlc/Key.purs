module Language.Shape.Stlc.Key where

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Unsafe (error)

newtype Key
  = Key String

instance showKey :: Show Key where
  show (Key str) = Array.intercalate "" (showModKey <$> mods) <> showBaseKey base
    where
    { init: mods, last: base } = case Array.unsnoc $ String.split (Pattern " ") str of
      Just res -> res
      Nothing -> error $ "the following string is not a valid encoding for a Key: " <> str

    showBaseKey = case _ of
      "Escape" -> "ESC"
      "Enter" -> "↩"
      "ArrowRight" -> "◀"
      "ArrowLeft" -> "▶"
      "ArrowUp" -> "▲"
      "ArrowDown" -> "▼"
      "Tab" -> "⇥"
      "Backspace" -> "⌫"
      base -> base

    showModKey = case _ of
      "Shift" -> "⇧"
      "Meta" -> "⌘"
      "Ctrl" -> "⌃"
      mod -> mod

derive newtype instance eqKey :: Eq Key

derive newtype instance ordKey :: Ord Key

keys :: _
keys =
  { dig: [ Key "Ctrl d", Key "Ctrl Backspace" ]
  -- lambda: l
  , lambda: [ Key "Ctrl l" ]
  , unlambda: [ Key "Ctrl Shift l", Key "Backspace" ]
  , inlambda: [ Key "Ctrl r" ] -- "r" for "refine", like in Agda
  -- eta: n
  , eta: [ Key "Ctrl n" ]
  , uneta: [ Key "Ctrl Shift n" ]
  -- match: m
  , inmatch: [ Key "Ctrl Meta m" ]
  -- let: f
  , let_: [ Key "Ctrl f" ]
  , unlet: [ Key "Ctrl Shift f", Key "Backspace" ]
  -- data: t
  , data_: [ Key "Ctrl t" ]
  , undata: [ Key "Ctrl Shift t", Key "Backspace" ]
  -- buf: b
  , buf: [ Key "Ctrl b" ]
  , unbuf: [ Key "Ctrl Shift b", Key "Backspace" ]
  , pop: [ Key "Ctrl p" ]
  -- swap: s
  , swap: [ Key "Ctrl s" ]
  -- app
  , app: [ Key "Ctrl a" ]
  , unapp: [ Key "Ctrl Shift a", Key "Backspace" ]
  -- NormalMode
  , normalMode: [ Key "Escape" ]
  -- QueryMode
  , submitQuery: [ Key "Enter" ]
  , nextQueryOption: [ Key "ArrowUp" ]
  , prevQueryOption: [ Key "ArrowDown" ]
  -- cursor
  , cursorForwards: [ Key "ArrowRight" ]
  , cursorBackwards: [ Key "ArrowLeft" ]
  -- misc
  , delete: [ Key "Backspace" ]
  , indent: [ Key "Tab" ]
  , copy: [ Key "Ctrl c" ]
  , paste: [ Key "Ctrl v" ]
  , undo: [ Key "Ctrl z" ]
  , escape: [ Key "Escape" ]
  , enter: [ Key "Enter" ]
  , arrowUp: [ Key "ArrowUp" ]
  , arrowDown: [ Key "ArrowDown" ]
  , arrowLeft: [ Key "ArrowLeft" ]
  , arrowRight: [ Key "ArrowRight" ]
  }
