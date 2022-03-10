module Data.List.Unsafe (module Data.List, updateAt') where

import Data.List
import Prelude
import Unsafe
import Debug as Debug
import Data.Tuple.Nested

updateAt' :: forall a. Int -> a -> List a -> List a
updateAt' i a ls = -- let
  --   _ = Debug.trace "updateAt'" identity /\ Debug.trace i identity /\ Debug.trace ls identity
  -- in
  fromJust $ updateAt i a ls
