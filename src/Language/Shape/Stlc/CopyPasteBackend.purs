module Language.Shape.Stlc.CopyPasteBackend where

import Data.Tuple.Nested
import Prelude
import Prim hiding (Type)

import Data.Default (default)
import Data.Either (Either)
import Data.List (List(..), range, (:))
import Data.List as List
import Data.Map (empty)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.OrderedMap as OMap
import Data.Set as Set
import Language.Shape.Stlc.Changes (Changes, VarChange(..))
import Language.Shape.Stlc.Context (Context(..))
import Language.Shape.Stlc.Hole (HoleSub, unifyType)
import Language.Shape.Stlc.Index (IxDown(..), IxStep(..))
import Language.Shape.Stlc.Syntax (Neu, Term(..), TermId(..), Type(..), ArgItem)
import Undefined (undefined)

-- First type is type of variable, second type is type of hole, output is how many args the
-- variable is applied to to fit in the hole
fitsInHole :: Type -> Type -> Maybe (Int /\ HoleSub)
fitsInHole a b = case unifyType a b of
    Just holeSub -> Just (0 /\ holeSub)
    Nothing -> case a of
         (ArrowType {dom, cod}) -> map (\(n /\ s) -> ((n + 1) /\ s)) (fitsInHole cod b)
         _ -> Nothing

createNeu :: TermId -> Int -> Term
createNeu x n = Neu {termId : x,
    argItems : map (\_ -> {term: Hole {meta: default}, meta: default}) (range 1 n),
    meta: default}

-- for use in copy-pasting a term.
-- For now, makes the assumption that nothing about the program was changed between
-- the copy and paste, can I can update later to remove that assumption if we determine that
-- to be the correct approach.
-- TODO: which context is the copied term's context, and which is the pasted hole's context? I assumed the first is copied term's and second is pasted hole's
changesBetweenContexts :: Context -> Context -> Changes
changesBetweenContexts gamma1 gamma2 = 
    let g1 = unwrap gamma1 in
    let g2 = unwrap gamma2 in
    {
        termChanges : Map.fromFoldable (map (\ v -> (v /\ VariableDeletion))
            (List.difference (OMap.keys g1.varTypes) (OMap.keys g2.varTypes)))
        , matchChanges : empty
        , dataTypeDeletions : Set.fromFoldable (List.difference (OMap.keys g1.datas) (OMap.keys g2.datas))
    }

-- inputs "start" and "end" indicies
-- if moving a let from "start" to "end", every move in the AST can be classified as going up to an index,
-- and then going back down to "end".
-- This function returns that maximum point.

-- returns least common ancestor of two indices
indexMoveClassify :: IxDown -> IxDown -> IxDown
indexMoveClassify (IxDown ix1) (IxDown ix2) =
    let indexMoveClassifyAux :: List IxStep -> List IxStep -> List IxStep
        indexMoveClassifyAux (step1 : ix1) (step2 : ix2) =
            if step1 == step2
            then step1 : indexMoveClassifyAux ix1 ix2
            else Nil
        indexMoveClassifyAux _ _ = Nil
    in IxDown(indexMoveClassifyAux ix1 ix2)

{-
Next, its time to actually write the code that moves a definition.
There are three relevant indices: start, ancestor, and end.
It needs to:

call chAtIndex on subterm at "ancestor", and use ToReplace to insert term at "start"
This will require taking difference of "start" and "ancestor". Also, will need the context at "ancestor"
-}