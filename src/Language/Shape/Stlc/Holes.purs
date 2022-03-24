module Language.Shape.Stlc.Holes where

import Prelude
import Prim hiding (Type)
import Data.Map (Map, empty, lookup, singleton, union)
import Data.Maybe (Maybe(..))
import Data.Set (member)
import Language.Shape.Stlc.Syntax
import Undefined (undefined)
import Unsafe (error)
import Data.List (List(..))
import Data.Tuple (Tuple(..))

{-
This file will deal with unification of types, and applying hole substitutions
to Blocks, Terms, ...
-}
type HoleSub
  = Map HoleId Type

emptyHoleSub :: HoleSub
emptyHoleSub = empty

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

subModule :: HoleSub -> Module -> Module
subModule = undefined

subTerm :: HoleSub -> Term -> Term
subTerm sub (LambdaTerm bind block md) = LambdaTerm bind (subBlock sub block) md
subTerm sub (HoleTerm md) = HoleTerm md
subTerm sub (MatchTerm id t cases md)
  = MatchTerm id (subTerm sub t) (map (\(Tuple cas md) -> Tuple (subCase sub cas) md) cases) md
subTerm sub (NeutralTerm x args md) = NeutralTerm x (subArgs sub args) md

subArgs :: HoleSub -> List ArgItem -> List ArgItem
subArgs sub Nil = Nil
subArgs sub (Cons (Tuple t md) rst) = Cons (Tuple (subTerm sub t) md) (subArgs sub rst)

subBlock :: HoleSub -> Block -> Block
subBlock sub (Block defs t md)
  = Block (map (\(Tuple def md) -> Tuple (subDefinition sub def) md) defs) (subTerm sub t) md

subDefinition :: HoleSub -> Definition -> Definition
subDefinition sub (TermDefinition binds ty t md) = TermDefinition binds (subType sub ty) (subTerm sub t) md

subDefinition sub (DataDefinition bind ctrs md)
  = DataDefinition bind (map (\(Tuple ctr md) -> Tuple (subConstructor sub ctr) md) ctrs) md

subConstructor :: HoleSub -> Constructor -> Constructor
subConstructor sub (Constructor bind paramItems md)
  = Constructor bind (map (\(Tuple param md) -> Tuple (subParameter sub param) md) paramItems) md

subCase :: HoleSub -> Case -> Case
subCase sub (Case binds t md) = Case binds (subBlock sub t) md