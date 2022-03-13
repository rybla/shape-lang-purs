module Data.List.Unsafe (module Data.List, updateAt', index', insertAt', deleteAt') where

import Data.List
import Prelude
import Unsafe
import Debug as Debug
import Data.Tuple.Nested

index' :: forall a. List a -> Int -> a
index' ls i = fromJust $ index ls i

updateAt' :: forall a. Int -> a -> List a -> List a
updateAt' i a ls = fromJust $ updateAt i a ls

-- let --   _ = Debug.trace "updateAt'" identity /\ Debug.trace i identity /\ Debug.trace ls identity -- in

insertAt' :: ∀ a. Int → a → List a → List a
insertAt' i a l = fromJust $ insertAt i a l

deleteAt' :: ∀ a. Int → List a → List a
deleteAt' i l = fromJust $ deleteAt i l