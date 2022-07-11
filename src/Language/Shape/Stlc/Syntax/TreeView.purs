module Language.Shape.Stlc.Syntax.TreeView where

import Data.Tuple.Nested
import Prelude
import Prim hiding (Type)

import Data.List (List(..), mapWithIndex, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Language.Shape.Stlc.Index (IxStep(..), IxStepLabel(..), ixDownListItem, ixStepArgItem, ixStepArrowType, ixStepBuf, ixStepCaseItem, ixStepData, ixStepLam, ixStepLet, ixStepMatch, ixStepNeu)
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

one :: forall a. a -> List a
one x = x : Nil

{-
This function allows you to operate on Syntax as a tree. Using it, you don't need to case about the
structure of the piece of syntax you have beyond that of the structure of a tree.

It returns a list of Syntax, which are the child nodes of your syntax.

It also returns (although for some purposes may not need this information) a fragment of an index which
goes from the syntax to that child. The reason its a list is because in the Neu case, it skips over
the ArgItems and goes directly to the terms.
-}
getChildren :: Syntax -> List (Syntax /\ List IxStep)
getChildren (SyntaxType (ArrowType {dom, cod}))
    = (SyntaxType dom /\ one ixStepArrowType.dom) : (SyntaxType cod /\ one ixStepArrowType.cod) : Nil
getChildren (SyntaxType (DataType _)) = Nil
getChildren (SyntaxType (HoleType _)) = Nil
getChildren (SyntaxTerm (Lam {termBind, body}))
    = (SyntaxTermBind termBind /\ one ixStepLam.termBind) : (SyntaxTerm body /\ one ixStepLam.body) : Nil
getChildren (SyntaxTerm (Let {termBind, sign, impl, body}))
    = (SyntaxTermBind termBind /\ one ixStepLet.termBind) : (SyntaxType sign /\ one ixStepLet.sign)
        : (SyntaxTerm impl /\ one ixStepLet.impl) : (SyntaxTerm body /\ one ixStepLet.body) : Nil
getChildren (SyntaxTerm (Neu {termId, argItems}))
    = one (SyntaxTermId termId /\ one ixStepNeu.termId)
    <> mapWithIndex (\n {term} -> SyntaxTerm term /\ (unwrap (ixDownListItem n) <> one ixStepArgItem.term)) argItems
getChildren (SyntaxTerm (Buf {sign, impl, body}))
    = (SyntaxType sign /\ one ixStepBuf.sign) : (SyntaxTerm impl /\ one ixStepBuf.impl) : (SyntaxTerm body /\ one ixStepBuf.body) : Nil
getChildren (SyntaxTerm (Data {typeBind, sumItems, body}))
    = one (SyntaxTypeBind typeBind /\ one ixStepData.typeBind)
    -- TODO: this should return the various constructors as children!!!!
getChildren (SyntaxTerm (Match {term, caseItems}))
    = one (SyntaxTerm term /\ one ixStepMatch.term)
    <> mapWithIndex (\n caseItem -> SyntaxCaseItem caseItem /\ unwrap (ixDownListItem n)) caseItems
getChildren (SyntaxTerm (Hole _)) = Nil
getChildren (SyntaxTermBind _) = Nil
getChildren (SyntaxTermId _) = Nil
getChildren (SyntaxTypeBind _) = Nil
getChildren (SyntaxArgItem _) = error "shouldn't get here because skips over the ArgItem case in the Neu case"
getChildren (SyntaxSumItem _) = error "not implemented yet"
getChildren (SyntaxCaseItem {termBindItems, body})
    = mapWithIndex (\n termBindItem -> SyntaxTermBindItem termBindItem /\ (ixStepCaseItem.termBindItems : unwrap (ixDownListItem n))) termBindItems
    <> one (SyntaxTerm body /\ one ixStepCaseItem.body)
getChildren (SyntaxParamItem _) = Nil -- TODO: implement this! Should have the various parameters as children!
getChildren (SyntaxTermBindItem _) = Nil
getChildren (SyntaxList l) = error " shouldn't get here unless I messed something up"
-- TODO: can't I just implement this and then I don't have to implement the other ones?
-- Answer: only if I want the list itself to be something that can be selected!