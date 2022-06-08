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
  , lambda: [ Key "l" ]
  , unlambda: [ Key "Shift l" ]
  , eta: [ Key "n" ]
  , uneta: [ Key "Shift n" ]
  , let_: [ Key "=" ]
  , unlet: [ Key "Shift =" ]
  , data_: [ Key ":" ]
  , undata: [ Key "Shift :" ]
  , buf: [ Key "/" ]
  , unbuf: [ Key "Shift /" ]
  , delete: [ Key "Backspace" ]
  , indent: [ Key "Tab" ]
  , copy: [ Key "Ctrl c" ]
  , paste: [ Key "Ctrl v" ]
  }
