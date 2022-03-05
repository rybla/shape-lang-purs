module Language.Shape.Stlc.Recursion.Test where

import Prelude
import Undefined (undefined)

data A
  = A1 B B
  | A2

data B
  = B1 Int
  | B2 A

type Wrap a
  = a -> A

recA ::
  forall t.
  { a1 :: B -> B -> Wrap B -> Wrap B -> t
  , a2 :: t
  } ->
  A -> Wrap A -> t
recA rec a wrapA = case a of
  A1 b b' -> rec.a1 b b' (\b -> wrapA $ A1 b b') (\b' -> wrapA $ A1 b b')
  A2 -> rec.a2

recB ::
  forall t.
  { b1 :: Int -> t
  , b2 :: A -> Wrap A -> t
  } ->
  B -> Wrap B -> t
recB rec b wrapB = case b of
  B1 i -> rec.b1 i
  B2 a -> rec.b2 a (\a -> wrapB $ B2 a)
