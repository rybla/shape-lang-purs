module Data.OrderedSet where

import Prelude
import Data.Foldable (class Foldable)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over, unwrap, wrap)

newtype OrderedSet a
  = OrderedSet (List a)

derive instance newTypeOrderedSet :: Newtype (OrderedSet a) _

derive newtype instance monoidOrderedSet :: Monoid (OrderedSet a)

derive newtype instance foldableOrderedSet :: Foldable OrderedSet

derive newtype instance eqOrderedSet :: Eq a => Eq (OrderedSet a)

insert :: forall a. Eq a => a -> OrderedSet a -> OrderedSet a
insert a s = if List.elem a s then s else over wrap (Cons a) s

findIndex :: forall a. Eq a => (a -> Boolean) -> OrderedSet a -> Maybe Int
findIndex cond s = List.findIndex cond (unwrap s)
