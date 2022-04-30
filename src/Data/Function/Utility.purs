module Data.Function.Utility where

import Prelude

import Data.Foldable (class Foldable, foldr)

flipfoldr :: forall f a b. Foldable f ⇒ f a → (a → b → b) → b → b
flipfoldr xs f a = foldr f a xs
