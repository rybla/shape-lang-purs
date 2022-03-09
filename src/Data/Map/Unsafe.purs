module Data.Map.Unsafe (module Data.Map, lookup') where

import Prelude
import Unsafe
import Data.Map

lookup' :: forall k v. Ord k => k -> Map k v -> v
lookup' k m = fromJust $ lookup k m
