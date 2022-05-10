module Data.OrderedMap where

import Data.Tuple
import Data.Tuple.Nested
import Prelude

import Data.Foldable as Foldable
import Data.List (List(..))
import Data.Maybe (Maybe(..), maybe')
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Partial.Unsafe (unsafeCrashWith)

-- Keeps track of the order of items in map.
-- Is VERY inefficient.
newtype OrderedMap k v
  = OrderedMap (List (k /\ v))

derive instance newTypeOrderedMap :: Newtype (OrderedMap k v) _

derive newtype instance showOrderedMap :: (Show k, Show v) => Show (OrderedMap k v)

keys :: forall k v. OrderedMap k v -> List k
keys = map fst <<< unwrap

vals :: forall k v. OrderedMap k v -> List v
vals = map snd <<< unwrap

lookup :: forall k v. Eq k => k -> OrderedMap k v -> Maybe v
lookup k = Foldable.lookup k <<< unwrap

lookup' :: forall k v. Show k => Show v => Eq k => k -> OrderedMap k v -> v
lookup' k m = maybe' (\_ -> unsafeCrashWith $ "could not find key " <> show k <> " in ordered map " <> show m) identity $ Foldable.lookup k $ unwrap m

lookup'' :: forall k v. Ord k => String -> k -> OrderedMap k v -> v
lookup'' msg k m = case lookup k m of
  Just v -> v
  Nothing -> unsafeCrashWith $ msg

insert :: forall k v. Eq k => k -> v -> OrderedMap k v -> OrderedMap k v
insert k v = over wrap (Cons (k /\ v))

empty :: forall k v. OrderedMap k v
empty = wrap Nil

toList :: forall k v. OrderedMap k v -> List (k /\ v)
toList = unwrap
