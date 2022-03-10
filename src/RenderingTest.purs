module RenderingTest where

import Prelude

data A
  = A1 A B

data B
  = B1 A
  | B3

type Wrap a
  = a -> A

recA :: forall a. { a1 :: A -> B -> Wrap A -> Wrap B -> a } -> A -> Wrap A -> a
recA rec a wrap_a = case a of
  A1 a b -> rec.a1 a b (\a' -> wrap_a $ A1 a' b) (\b' -> wrap_a $ A1 a b')

recB :: forall a. { b1::A -> Wrap A -> a , b3 :: a } -> B -> Wrap B -> a 
recB rec b wrap_b = case b of 
  B1 a -> rec.b1 a (\a' -> wrap_b $ B1 a') 
  B3 