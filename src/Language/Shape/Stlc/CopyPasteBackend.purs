module Language.Shape.Stlc.CopyPasteBackend where

import Data.Tuple.Nested
import Prelude
import Prim hiding (Type)

import Data.Default (default)
import Data.Either (Either)
import Data.List (List(..), range, (:))
import Data.Maybe (Maybe(..))
import Language.Shape.Stlc.Index (IxDown(..), IxStep(..))
import Language.Shape.Stlc.Syntax (Neu, Term(..), TermId(..), Type(..), ArgItem)
import Undefined (undefined)

-- First type is type of variable, second type is type of hole, output is how many args the
-- variable is applied to to fit in the hole
fitsInHole :: Type -> Type -> Maybe Int
fitsInHole a b = if a == b
    then Just 0
    else case a of
         (ArrowType {dom, cod}) -> map (\n -> n + 1) (fitsInHole cod b)
         _ -> Nothing

createNeu :: TermId -> Int -> Term
createNeu x n = Neu {termId : x,
    argItems : map (\_ -> {term: Hole {meta: default}, meta: default}) (range 1 n),
    meta: default}

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