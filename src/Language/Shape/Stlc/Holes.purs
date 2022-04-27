module Language.Shape.Stlc.Hole where

import Control.Monad.Except
import Control.Monad.State
import Data.Map
import Data.Tuple
import Data.Tuple.Nested
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)

import Control.Monad.Except as Except
import Data.List (List)
import Data.Maybe (Maybe(..))
import Undefined (undefined)


{-
This deals with unification of types, and applying hole substitutions
to Blocks, Terms, ...
-}

type HoleSub
  = Map HoleId Type

type HoleEq = Map HoleId HoleType

emptyHoleSub :: HoleSub
emptyHoleSub = empty

subType :: HoleSub -> Type -> Type
subType sub (ArrowType {dom, cod, meta})
  = ArrowType {dom: subType sub dom, cod: subType sub cod, meta}
subType sub (DataType data_) = DataType data_
subType sub (HoleType {holeId, weakening, meta}) = case lookup holeId sub of
  Nothing -> HoleType {holeId, weakening, meta}
  Just t -> t

restrictToFull :: HoleEq -> HoleSub
restrictToFull sub = map HoleType sub

unifyType :: Type -> Type -> Maybe HoleSub
unifyType (HoleType {holeId}) t2 = Just $ singleton holeId t2
unifyType t1 (HoleType hole) = unifyType (HoleType hole) t1
unifyType (ArrowType {dom:dom1, cod: cod1, meta: meta1})
          (ArrowType {dom:dom2, cod: cod2, meta: meta2}) = do
  a <- unifyType dom1 dom2
  b <- unifyType (subType a cod1) (subType a cod2)
  pure $ union a b
unifyType (DataType {typeId:id1}) (DataType {typeId:id2}) = if id1 == id2 then Just empty else Nothing
unifyType _ _ = Nothing

unifyTypeRestricted :: Type -> Type -> Maybe HoleEq
unifyTypeRestricted (HoleType {holeId:holeId1})
  (HoleType hole2@{holeId:holeId2}) =
    if holeId1 == holeId2 then Just $ singleton holeId1 hole2 else Nothing
unifyTypeRestricted t1 (HoleType hole) = unifyTypeRestricted (HoleType hole) t1
unifyTypeRestricted (ArrowType {dom:dom1, cod: cod1, meta: meta1})
                    (ArrowType {dom:dom2, cod: cod2, meta: meta2}) = do
  a <- unifyTypeRestricted dom1 dom2
  b <- unifyTypeRestricted (subType (restrictToFull a) cod1) (subType (restrictToFull a) cod2)
  pure $ union a b
unifyTypeRestricted (DataType {typeId:id1}) (DataType {typeId:id2}) = if id1 == id2 then Just empty else Nothing
unifyTypeRestricted _ _ = Nothing

subTerm :: HoleSub -> Term -> Term
subTerm sub (Lam {termBind, body, meta}) = Lam {termBind, body:(subTerm sub body), meta}
subTerm sub (Neu {termId, args, meta}) = Neu {termId, args: subArgs sub args, meta}
subTerm sub (Let {termBind, type_, term}) = ?h
subTerm sub (Buf {type_, term, body, meta}) = ?h
subTerm sub (Data {typeBind, sum, body, meta}) = ?h
subTerm sub (Match {type_, term, case_, meta})
  = Match {type_, term: subTerm sub term, case_: map (subCase sub) case_, meta}
subTerm sub (Hole {meta}) = Hole {meta}

subArgs :: HoleSub -> List ArgItem -> List ArgItem
subArgs = undefined

subCase :: HoleSub -> Case -> Case
subCase = undefined

{-
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

-}