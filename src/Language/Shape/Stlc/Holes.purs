module Language.Holes where

import Prelude
import Prim

import Data.Map (Map, empty, lookup, singleton, union)
import Data.Maybe (Maybe(..))
import Data.Set (member)
import Language.Shape.Stlc.Syntax (Block(..), Case(..), HoleID(..), NeutralTerm(..), Parameter(..), Term(..), Type(..), TypeID(..), TypeWeakening)
import Undefined (undefined)

{-
This file will deal with unification of types, and applying hole substitutions
to Blocks, Terms, ...
-}

type HoleSub = Map HoleID Type

subType :: HoleSub -> Type -> Type
subType sub (ArrowType (Parameter a md1) b md2)
    = ArrowType (Parameter (subType sub a) md1) (subType sub b) md2
subType sub (DataType i md) = DataType i md
subType sub (HoleType id wea md) = case lookup id sub of
    Nothing -> HoleType id wea md
    Just t -> t

unifyType :: Type -> Type -> Maybe HoleSub
unifyType (HoleType id wea md) t2 = Just $ singleton id t2
unifyType t1 (HoleType id wea md) = unifyType (HoleType id wea md) t1
unifyType (ArrowType (Parameter a1 mda1) b1 mdb1) (ArrowType (Parameter a2 mda2) b2 mdb2)
    = do a <- unifyType a1 a2
         b <- unifyType (subType a b1) (subType a b2)
         pure $ union a b
unifyType (DataType i1 md1) (DataType i2 md2) = if i1 == i2 then Just empty else Nothing
unifyType _ _ = Nothing

subTerm :: HoleSub -> Term -> Term
subTerm sub (LambdaTerm bind block md) = undefined
subTerm sub (HoleTerm md) = HoleTerm md
subTerm sub (MatchTerm id t cases md)
    = MatchTerm id (subTerm sub t) (map (subCase sub) cases) md
subTerm sub (NeutralTerm t md) = NeutralTerm (subTerm sub t) md

subBlock :: HoleSub -> Block -> Block

subCase :: HoleSub -> Case -> Case
subCase sub (Case binds t md) = undefined

subNeutral :: HoleSub -> NeutralTerm -> NeutralTerm
subNeutral sub (VariableTerm i md) = undefined
subNeutral sub (ApplicationTerm t1 t2 md) = undefined