module Language.Holes where

import Prelude
import Prim

import Data.Map (Map, singleton)
import Data.Maybe (Maybe(..))
import Data.Set (member)
import Halogen.VDom.Thunk (thunk2)
import Language.Shape.Stlc.Syntax (BaseType(..), HoleId(..), Type(..), TypeId(..), TypeWeakening)
import Undefined (undefined)

{-
This file will deal with unification of types, and applying hole substitutions
to Blocks, Terms, ...
-}

type HoleSub = Map HoleId Type

unifyType :: Type -> Type -> Maybe HoleSub
unifyType (ArrowType ins out md) (ArrowType ins' out' md') = undefined
unifyType (BaseType t1) (BaseType t2) = unifyBaseType t1 t2
unifyType _ _ = Nothing

unifyBaseType :: BaseType -> BaseType -> Maybe HoleSub
unifyBaseType (HoleType x weak md) t2 = undefined
    -- = if wasn't weakened:
    -- = Just $ singleton x t2
unifyBaseType t1 t2@(HoleType x weak md) = unifyBaseType t2 t1
unifyBaseType (DataType x1 md1) (DataType x2 md2) = undefined
-- unifyBaseType _ _ = Nothing

wasWeakened :: TypeWeakening -> Type -> Boolean
wasWeakened w (ArrowType ins out md) = undefined
wasWeakened w (BaseType (HoleType id w2 md)) = undefined
wasWeakened w (BaseType (DataType id md)) = member id w