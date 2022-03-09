module Data.List.Unsafe (module Data.List, updateAt') where

import Prelude
import Data.List
import Unsafe

updateAt' :: forall a. Int -> a -> List a -> List a
updateAt' i a ls = fromJust $ updateAt i a ls
