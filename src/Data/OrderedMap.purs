module Data.OrderedMap where

import Data.Tuple
import Data.Tuple.Nested
import Prelude
import Data.Foldable as Foldable
import Data.List (List(..))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over, unwrap, wrap)

-- Keeps track of the order of items in map.
-- Is VERY inefficient.
newtype OrderedMap k v
  = OrderedMap (List (k /\ v))

derive instance newTypeOrderedMap :: Newtype (OrderedMap k v) _

derive newtype instance showOrderedMap :: (Show k, Show v) => Show (OrderedMap k v)

lookup :: forall k v. Eq k => k -> OrderedMap k v -> Maybe v
lookup k = Foldable.lookup k <<< unwrap

insert :: forall k v. Eq k => k -> v -> OrderedMap k v -> OrderedMap k v
insert k v = over wrap (Cons (k /\ v))

empty :: forall k v. OrderedMap k v
empty = wrap Nil
