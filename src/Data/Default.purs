module Data.Default where

class Default :: Type -> Type -> Constraint
class Default a b | b -> a where
  default :: a -> b
