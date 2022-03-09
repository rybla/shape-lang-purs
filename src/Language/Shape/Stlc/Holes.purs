module Language.Holes where

import Prelude
import Prim

import Data.Map (Map, empty, lookup, singleton, union)
import Data.Maybe (Maybe(..))
import Data.Set (member)
import Language.Shape.Stlc.Syntax (Args(..), Block(..), Case(..), Constructor(..), Definition(..), HoleID(..), Parameter(..), Term(..), Type(..), TypeID(..), TypeWeakening)
import Undefined (undefined)
import Unsafe (error)

{-
This file will deal with unification of types, and applying hole substitutions
to Blocks, Terms, ...
-}
type HoleSub
  = Map HoleID Type

subType :: HoleSub -> Type -> Type
subType sub (ArrowType param b md2) = ArrowType (subParameter sub param) (subType sub b) md2
subType sub (DataType i md) = DataType i md
subType sub (ProxyHoleType i) = error "This probably shouldn't happen"
subType sub (HoleType id wea md) = case lookup id sub of
  Nothing -> HoleType id wea md
  Just t -> t

subParameter :: HoleSub -> Parameter -> Parameter
subParameter sub (Parameter t md) = Parameter (subType sub t) md

unifyType :: Type -> Type -> Maybe HoleSub
unifyType (HoleType id wea md) t2 = Just $ singleton id t2
unifyType t1 (HoleType id wea md) = unifyType (HoleType id wea md) t1
unifyType (ProxyHoleType i1) (ProxyHoleType i2) = if i1 == i2 then Just empty else Nothing
unifyType (ArrowType (Parameter a1 mda1) b1 mdb1) (ArrowType (Parameter a2 mda2) b2 mdb2) = do
  a <- unifyType a1 a2
  b <- unifyType (subType a b1) (subType a b2)
  pure $ union a b

unifyType (DataType i1 md1) (DataType i2 md2) = if i1 == i2 then Just empty else Nothing

unifyType _ _ = Nothing

subTerm :: HoleSub -> Term -> Term
subTerm sub (LambdaTerm bind block md) = LambdaTerm bind (subBlock sub block) md
subTerm sub (HoleTerm md) = HoleTerm md
subTerm sub (MatchTerm id t cases md) = MatchTerm id (subTerm sub t) (map (subCase sub) cases) md
subTerm sub (NeutralTerm x args md) = NeutralTerm x (subArgs sub args) md

subArgs :: HoleSub -> Args -> Args
subArgs sub None = None
subArgs sub (Cons t args md) = Cons (subTerm sub t) (subArgs sub args) md

subBlock :: HoleSub -> Block -> Block
subBlock sub (Block defs t md) = Block (map (subDefinition sub) defs) (subTerm sub t) md

subDefinition :: HoleSub -> Definition -> Definition
subDefinition sub (TermDefinition binds ty t md) = TermDefinition binds (subType sub ty) (subTerm sub t) md

subDefinition sub (DataDefinition bind ctrs md) = DataDefinition bind (map (subConstructor sub) ctrs) md

subConstructor :: HoleSub -> Constructor -> Constructor
subConstructor sub (Constructor bind args md) = Constructor bind (map (subParameter sub) args) md

subCase :: HoleSub -> Case -> Case
subCase sub (Case binds t md) = Case binds (subTerm sub t) md