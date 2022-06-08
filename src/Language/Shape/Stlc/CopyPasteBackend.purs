module Language.Shape.Stlc.CopyPasteBackend where

import Prelude
import Prim hiding (Type)

import Data.Default (default)
import Data.List (range)
import Data.Maybe (Maybe(..))
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