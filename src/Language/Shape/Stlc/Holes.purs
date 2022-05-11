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

emptyHoleEq :: HoleEq
emptyHoleEq = empty

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
subTerm sub (Neu {termId, argItems, meta}) = Neu {termId, argItems: map (subArgItem sub) argItems, meta}
subTerm sub (Let {termBind, sign, impl, body, meta})
  = Let {termBind, sign: subType sub sign, impl: subTerm sub impl,
          body: subTerm sub body, meta}
subTerm sub (Buf {sign, impl, body, meta}) =
  Buf {sign: subType sub sign, impl: subTerm sub impl, body: subTerm sub body, meta}
subTerm sub (Data {typeBind, sumItems, body, meta}) =
  Data {typeBind, sumItems: map (subSumItem sub) sumItems, body: subTerm sub body, meta}
subTerm sub (Match {typeId, term, caseItems, meta})
  = Match {typeId, term: subTerm sub term, caseItems: map (subCase sub) caseItems, meta}
subTerm sub (Hole {meta}) = Hole {meta}

subArgItem :: HoleSub -> ArgItem -> ArgItem
subArgItem sub {term, meta} = {term: subTerm sub term, meta}

subCase :: HoleSub -> CaseItem -> CaseItem
subCase sub {termBindItems, body, meta} = {termBindItems, body: subTerm sub body, meta}

subSumItem :: HoleSub -> SumItem -> SumItem
subSumItem sub {termBind, paramItems, meta} = {termBind, paramItems: map (subParamItem sub) paramItems, meta}

subParamItem :: HoleSub -> ParamItem -> ParamItem
subParamItem sub {type_, meta} = {type_: subType sub type_, meta}