module Language.Shape.Stlc.RecursionTest where

import Prelude
import Data.List (List(..))

data A
  = A1 String B B
  | A2

data B
  = B1 B
  | B2

type Wrap a
  = a -> A

type Context
  = List String

{-
level 1: handles context
-}
recA_l1 :: forall t. { recA1 :: Context -> String -> B -> B -> t, recA2 :: Context -> t } -> Context -> A -> t
recA_l1 recs gamma = case _ of
  A1 str b b' -> recs.recA1 (Cons str gamma) str b b'
  A2 -> recs.recA2 gamma

recB_l1 :: forall t. { recB1 :: Context -> B -> t, recB2 :: Context -> t } -> Context -> B -> t
recB_l1 recs gamma = case _ of
  B1 b -> recs.recB1 gamma b
  B2 -> recs.recB2 gamma

{-
level 2: handles wrapping
-}
recA_l2 :: forall t. { recA1 :: Context -> String -> B -> B -> Wrap B -> Wrap B -> t, recA2 :: Context -> t } -> Context -> A -> Wrap A -> t
recA_l2 recs =
  recA_l1
    { recA1: \gamma' str b b' wrapA -> recs.recA1 gamma' str b b' (wrapA <<< \b -> A1 str b b') (wrapA <<< \b' -> A1 str b b')
    , recA2: \gamma' _ -> recs.recA2 gamma'
    }

recB_l2 :: forall t. { recB1 :: Context -> B -> Wrap B -> t, recB2 :: Context -> t } -> Context -> B -> Wrap B -> t
recB_l2 recs =
  recB_l1
    { recB1: \gamma b wrapB -> recs.recB1 gamma b (wrapB <<< B1)
    , recB2: \gamma _ -> recs.recB2 gamma
    }
