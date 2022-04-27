module Data.Default where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

class Default a where
  default :: a

instance defaultMaybe :: Default (Maybe a) where
  default = Nothing

class DefaultF :: Type -> Type -> Constraint
class DefaultF a b | b -> a where
  defaultF :: a -> b

instance defaultEither :: DefaultF a (Either a b) where
  defaultF = Left
