module Data.Default where

class Default a where
  default :: a

class DefaultF :: Type -> Type -> Constraint
class DefaultF a b | b -> a where
  defaultF :: a -> b
