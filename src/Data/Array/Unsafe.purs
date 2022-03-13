module Data.Array.Unsafe (module Data.Array, index') where

import Prelude
import Data.Array
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)

index' :: forall a. Array a -> Int -> a
index' arr i = unsafePartial $ fromJust $ index arr i
