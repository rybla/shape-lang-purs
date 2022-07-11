module Language.Shape.Stlc.Syntax.TreeView where

import Prelude
import Prim hiding (Type)

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Language.Shape.Stlc.Index (IxStepLabel(..))
import Language.Shape.Stlc.Syntax (Syntax(..), Term(..), Type(..))
import Undefined (undefined)
import Unsafe (error)

{-
The concept of this file is to give functions for looking at Syntax as a tree.
Often, when you are working with syntax, you don't care about the distinction between
e.g. an ArrowType and a Buffer, and you just want to view it as a tree.
-}

-- numChildren :: Syntax -> Int
-- numChildren = undefined

-- getNthChild :: Syntax -> Int -> Syntax
-- getNthChild = undefined

getChildren :: Syntax -> List Syntax
getChildren (SyntaxType (ArrowType {dom, cod})) = SyntaxType dom : SyntaxType cod : Nil
getChildren (SyntaxType (DataType _)) = Nil
getChildren (SyntaxType (HoleType _)) = Nil
getChildren (SyntaxTerm (Lam {termBind, body})) = SyntaxTermBind termBind : SyntaxTerm body : Nil
getChildren (SyntaxTerm (Let {termBind, sign, impl, body})) = SyntaxTermBind termBind : SyntaxType sign : SyntaxTerm impl : SyntaxTerm body : Nil
getChildren (SyntaxTerm (Neu {termId, argItems})) = SyntaxTermId termId : undefined : Nil
getChildren (SyntaxTerm (Buf _)) = undefined
getChildren (SyntaxTerm (Data _)) = undefined
getChildren (SyntaxTerm (Match _)) = undefined
getChildren (SyntaxTerm (Hole _)) = undefined
getChildren (SyntaxTermBind _) = undefined
getChildren (SyntaxTermId _) = undefined
getChildren (SyntaxTypeBind _) = undefined
getChildren (SyntaxArgItem _) = undefined
getChildren (SyntaxSumItem _) = undefined
getChildren (SyntaxCaseItem _) = undefined
getChildren (SyntaxParamItem _) = undefined
getChildren (SyntaxTermBindItem _) = undefined
getChildren (SyntaxList _) = undefined