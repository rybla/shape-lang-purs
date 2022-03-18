module Data.Map.Unsafe (module Data.Map, lookup') where

import Data.Map
import Data.Tuple.Nested
import Prelude
import Unsafe
import Data.Maybe (Maybe(..))
import Debug as Debug
import Undefined (undefined)
import Unsafe as Unsafe
import Unsafe.Coerce (unsafeCoerce)

lookup' :: forall k v. Ord k => k -> Map k v -> v
lookup' k m = case lookup k m of
  Just v -> v
  Nothing -> Unsafe.error $ Debug.trace (k /\ m) \_ -> "fail lookup"
