module Data.Map.Unsafe (module Data.Map, lookup') where

import Prelude
import Data.Tuple.Nested
import Debug as Debug
import Unsafe
import Data.Map

lookup' :: forall k v. Ord k => k -> Map k v -> v
lookup' k m = -- let
  --   _ = Debug.trace "lookup'" identity /\ Debug.trace k identity /\ Debug.trace m identity
  -- in
  fromJust $ lookup k m
