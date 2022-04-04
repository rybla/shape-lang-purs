module Data.Map.Unsafe (module Data.Map, lookup') where

import Data.Map
import Data.Tuple.Nested
import Prelude
import Unsafe
import Data.Maybe (Maybe(..))
import Debug as Debug
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)
import Undefined (undefined)
import Unsafe as Unsafe
import Unsafe.Coerce (unsafeCoerce)

lookup' :: forall k v. Show k => Show v => Ord k => k -> Map k v -> v
lookup' k m = case lookup k m of
  Just v -> v
  Nothing -> unsafeCrashWith $ "fail lookup" <> "\nkey: " <> show k <> "\nmap: " <> show m
