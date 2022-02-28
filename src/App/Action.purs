module App.Action where

import Data.List (List)
import Data.Maybe (Maybe)
import Language.Shape.Stlc.Syntax (Module)

data Action
  = SetModule (Maybe Module)
  | SetPalette (List Action)

-- TODO: others?
-- change binder name
