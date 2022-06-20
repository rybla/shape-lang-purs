module Unsafe where

import Prelude
import Data.List as List
import Data.Map as Map
import Data.Maybe as Maybe
import Debug as Debug
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))

error :: forall a. String -> a
error msg = unsafePartial $ crashWith msg

fromJust :: forall a. Maybe.Maybe a -> a
fromJust = case _ of
  Maybe.Just a -> a
  Maybe.Nothing -> error "fromJust: Nothing"
