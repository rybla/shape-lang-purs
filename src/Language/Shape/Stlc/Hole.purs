module Language.Shape.Stlc.Hole where

import Control.Monad.State
import Data.Map
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Control.Monad.Except
import Control.Monad.Except as Except
import Undefined (undefined)
import Data.Tuple
import Data.Tuple.Nested

type HoleSub
  = Map HoleId Type

unify :: forall m. Monad m => Type -> Type -> ExceptT (Type /\ Type) (StateT HoleSub m) Unit
unify type1 type2 = case type1 /\ type2 of
  ArrowType arrow1 /\ ArrowType arrow2 -> do
    unify arrow1.dom arrow2.dom
    unify arrow1.cod arrow2.cod
  DataType data1 /\ DataType data2 -> unless (data1.id == data2.id) $ throwError (type1 /\ type2)
  HoleType hole1 /\ HoleType hole2 -> unifyHoles hole1 hole2
  HoleType hole1 /\ _ -> unifyHoleWith hole1 type2
  _ /\ HoleType hole2 -> unifyHoleWith hole2 type1
  _ -> throwError (type1 /\ type2)

unifyHoles :: forall m. Monad m => HoleType -> HoleType -> ExceptT (Type /\ Type) (StateT HoleSub m) Unit
unifyHoles = undefined -- TODO

unifyHoleWith :: forall m. Monad m => HoleType -> Type -> ExceptT (Type /\ Type) (StateT HoleSub m) Unit
unifyHoleWith = undefined -- TODO
