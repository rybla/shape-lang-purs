module Language.Shape.Stlc.Key where

import Prelude

newtype Key
  = Key String

derive newtype instance showKey :: Show Key

derive newtype instance eqKey :: Eq Key

derive newtype instance ordKey :: Ord Key

keys :: _
keys =
  { dig: [ Key "d" ]
  -- lambda: l
  , lambda: [ Key "l" ]
  , unlambda: [ Key "Shift l" ]
  , inlambda: [ Key "Meta l" ]
  -- eta: n
  , eta: [ Key "n" ]
  , uneta: [ Key "Shift n" ]
  -- match: m
  , inmatch: [ Key "Meta m" ]
  -- let: a
  , let_: [ Key "a" ]
  , unlet: [ Key "Shift a" ]
  -- data: t
  , data_: [ Key "t" ]
  , undata: [ Key "Shift t" ]
  -- buf: b
  , buf: [ Key "b" ]
  , unbuf: [ Key "Shift b" ]
  -- swap: s
  , swap: [ Key "s" ]
  -- cursor
  , cursorForwards: [ Key "ArrowRight" ]
  , cursorBackwards: [ Key "ArrowLeft" ]
  -- misc
  , delete: [ Key "Backspace" ]
  , indent: [ Key "Tab" ]
  , copy: [ Key "Ctrl c" ]
  , paste: [ Key "Ctrl v" ]
  , undo: [ Key "Ctrl z" ]
  }
