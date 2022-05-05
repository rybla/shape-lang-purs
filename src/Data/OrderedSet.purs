module Data.OrderedSet where

import Prelude

import Data.Foldable (class Foldable)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over, wrap, unwrap)

newtype OrderedSet a
  = OrderedSet (List a)

derive instance newTypeOrderedSet :: Newtype (OrderedSet a) _

-- prioritizes elements on the left
instance semigroupOrderedSet :: Eq a => Semigroup (OrderedSet a) where 
  append xs (OrderedSet ys) = List.foldl (\l y -> insert y l) xs ys
  -- wrap $ xs <> List.filter (not <<< (_ `List.elem` xs)) ys

instance monoidOrderedSet :: Eq a => Monoid (OrderedSet a) where
  mempty = wrap mempty

derive newtype instance foldableOrderedSet :: Foldable OrderedSet

derive newtype instance eqOrderedSet :: Eq a => Eq (OrderedSet a)

insert :: forall a. Eq a => a -> OrderedSet a -> OrderedSet a
insert a s = if List.elem a s then s else over wrap (Cons a) s

findIndex :: forall a. Eq a => (a -> Boolean) -> OrderedSet a -> Maybe Int
findIndex cond s = List.findIndex cond (unwrap s)

singleton :: forall a. a -> OrderedSet a
singleton = wrap <<< List.singleton

