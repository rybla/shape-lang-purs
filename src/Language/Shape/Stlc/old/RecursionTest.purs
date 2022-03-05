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
level 0: simple traversal
-}
recA_l0 :: forall t. { recA1 :: String -> B -> B -> t, recA2 :: t } -> A -> t
recA_l0 recs = case _ of
  A1 str b b' -> recs.recA1 str b b'
  A2 -> recs.recA2

recB_l0 :: forall t. { recB1 :: B -> t, recB2 :: t } -> B -> t
recB_l0 recs = case _ of
  B1 b -> recs.recB1 b
  B2 -> recs.recB2

{-
level 1: handles context
-}
recA_l1 :: forall t. { recA1 :: String -> B -> B -> Context -> t, recA2 :: Context -> t } -> A -> Context -> t
recA_l1 recs = case _ of
  A1 str b b' -> \gamma -> recs.recA1 str b b' (Cons str gamma)
  A2 -> \gamma -> recs.recA2 gamma

recB_l1 :: forall t. { recB1 :: Context -> B -> t, recB2 :: Context -> t } -> B -> Context -> t
recB_l1 recs = case _ of
  B1 b -> \gamma -> recs.recB1 gamma b
  B2 -> \gamma -> recs.recB2 gamma

{-
level 2: handles wrapping
-}
recA_l2 :: forall t. { recA1 :: String -> B -> B -> Context -> Wrap B -> Wrap B -> t, recA2 :: Context -> t } -> A -> Context -> Wrap A -> t
recA_l2 recs =
  recA_l1
    { recA1: \str b b' gamma wrapA -> recs.recA1 str b b' gamma (wrapA <<< \b -> A1 str b b') (wrapA <<< \b' -> A1 str b b')
    , recA2: \gamma _ -> recs.recA2 gamma
    }

recB_l2 :: forall t. { recB1 :: Context -> B -> Wrap B -> t, recB2 :: Context -> t } -> B -> Context -> Wrap B -> t
recB_l2 recs =
  recB_l1
    { recB1: \b gamma wrapB -> recs.recB1 b gamma (wrapB <<< B1)
    , recB2: \gamma wrapB -> recs.recB2 gamma
    }

{-
usage
-}
printA :: A -> Context -> String
printA =
  recA_l1
    { recA1: \str b b' gamma -> "A{" <> printB b gamma <> ", " <> printB b' gamma <> "}"
    , recA2: \gamma -> show gamma
    }

printB :: B -> Context -> String
printB =
  recB_l1
    { recB1: \gamma b -> "B" <> printB b gamma
    , recB2: \gamma -> ""
    }
