module Unsafe where

import Prelude
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)
import Data.List as List
import Data.Map as Map
import Data.Maybe as Maybe

error :: forall a. String -> a
error msg = unsafePartial $ crashWith msg

fromJust :: forall a. Maybe.Maybe a -> a
fromJust = case _ of
  Maybe.Just a -> a
  Maybe.Nothing -> error "fromJust: Nothing"

index :: forall a. List.List a -> Int -> a
index ls i = case List.index ls i of
  Maybe.Just a -> a
  Maybe.Nothing -> error $ "index: index of bounds: " <> show i

lookup :: forall k v. Ord k => Show k => k -> Map.Map k v -> v
lookup k m = case Map.lookup k m of
  Maybe.Just v -> v
  Maybe.Nothing -> error $ "lookup: key not found: " <> show k
