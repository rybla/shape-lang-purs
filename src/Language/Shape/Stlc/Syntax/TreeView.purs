module Language.Shape.Stlc.Syntax.TreeView where

import Data.Tuple.Nested
import Prelude
import Prim hiding (Type)

import Data.List (List(..), Pattern, find, findIndex, findMap, fold, foldr, index, mapWithIndex, stripPrefix, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Tuple (snd)
import Language.Shape.Stlc.Index (IxDown(..), IxStep(..), IxStepLabel(..), ixDownListItem, ixStepArgItem, ixStepArrowType, ixStepBuf, ixStepCaseItem, ixStepData, ixStepLam, ixStepLet, ixStepMatch, ixStepNeu, ixStepTermBindItem)
import Language.Shape.Stlc.Syntax (Syntax(..), Term(..), Type(..))
import Undefined (undefined)
import Unsafe (error)

{-
The concept of this file is to give functions for looking at Syntax as a tree.
Often, when you are working with syntax, you don't care about the distinction between
e.g. an ArrowType and a Buffer, and you just want to view it as a tree.
-}

{-
When we think of our syntax as a tree, we want certian things to be nodes of the tree, like an ArrowType,
a Lambda, or a Neu. However, this doesn't always correspond with how things are programmed
in Index. So, we make our own type TreeIndexStep, which represents a step from one node of a tree
to the next.
-}
type TreeIndexStep = List IxStep

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
getChildren :: Syntax -> List (Syntax /\ TreeIndexStep)
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
    -- = one (SyntaxTermId termId /\ one ixStepNeu.termId)
    -- <> mapWithIndex (\n {term} -> SyntaxTerm term /\ (unwrap (ixDownListItem n) <> one ixStepArgItem.term)) argItems
    = mapWithIndex (\n {term} -> SyntaxTerm term /\ (one ixStepNeu.argItems <> unwrap (ixDownListItem n) <> one ixStepArgItem.term)) argItems
getChildren (SyntaxTerm (Buf {sign, impl, body}))
    = (SyntaxTerm impl /\ one ixStepBuf.impl) : (SyntaxType sign /\ one ixStepBuf.sign) : (SyntaxTerm body /\ one ixStepBuf.body) : Nil
getChildren (SyntaxTerm (Data {typeBind, sumItems, body}))
    -- TODO: this should return the actually constructors somehow as well
    = (SyntaxTypeBind typeBind /\ one ixStepData.typeBind)
    : (SyntaxTerm body /\ one ixStepData.body)
    : Nil
getChildren (SyntaxTerm (Match {term, caseItems}))
    = one (SyntaxTerm term /\ one ixStepMatch.term)
    <> mapWithIndex (\n caseItem -> SyntaxCaseItem caseItem /\ (one ixStepMatch.caseItems <> unwrap (ixDownListItem n))) caseItems
getChildren (SyntaxTerm (Hole _)) = Nil
getChildren (SyntaxTermBind _) = Nil
getChildren (SyntaxTermId _) = Nil
getChildren (SyntaxTypeBind _) = Nil
getChildren (SyntaxArgItem _) = error "shouldn't get here because skips over the ArgItem case in the Neu case"
getChildren (SyntaxSumItem _) = error "not implemented yet"
getChildren (SyntaxCaseItem {termBindItems, body})
    = mapWithIndex (\n termBindItem -> SyntaxTermBindItem termBindItem /\ (ixStepCaseItem.termBindItems : unwrap (ixDownListItem n) <> one ixStepTermBindItem.termBind)) termBindItems
    <> one (SyntaxTerm body /\ one ixStepCaseItem.body)
getChildren (SyntaxParamItem _) = Nil -- TODO: implement this! Should have the various parameters as children!
getChildren (SyntaxTermBindItem _) = Nil
getChildren (SyntaxList l) = error " shouldn't get here unless I messed something up"
-- TODO: can't I just implement this and then I don't have to implement the other ones?
-- Answer: only if I want the list itself to be something that can be selected!

{-
The concept of this function is that there are certian index steps that we
want to sort of treat as one step, like stepping into an argument of a Neu.
This function takes an index, returns Nothing if it is empty, and otherwise
returns the index split into the next step and the rest.
-}
-- popIndex :: IxDown -> Maybe (TreeIndexStep /\ IxDown)
-- popIndex (IxDown Nil) = Nothing
-- popIndex idx@(IxDown (step@(IxStep label _) : _))
--     | label == IxStepList || {- label == IxStepArgItem || -} step == ixStepMatch.caseItems
--     = let before /\ after = untilAfterList (unwrap idx)
--       in  Just (before /\ IxDown after)
-- popIndex (IxDown ((IxStep label child) : rest)) = Just (one (IxStep label child) /\ IxDown rest)

-- untilAfterList :: List IxStep -> List IxStep /\ List IxStep
-- untilAfterList Nil = error "shouldn't get here 23o4234932"
-- untilAfterList (IxStep label child : rest)
--     | label == IxStepList || label == IxStepArgItem
--     = let (before /\ after) = untilAfterList rest
--       in (IxStep label child : before) /\ after
-- untilAfterList idx = Nil /\ idx

popIndex2 :: Syntax -> IxDown -> Maybe (TreeIndexStep /\ IxDown)
popIndex2 syn (IxDown idx)
    =  let children = getChildren syn
    in let nextSteps = map snd children
    in findMap (
        \step -> map (\rest -> step /\ IxDown rest) (stripPrefix (wrap step) idx)
    ) nextSteps

nextChild :: Syntax -> TreeIndexStep -> Maybe (List IxStep)
nextChild syn child =
    let children = getChildren syn
    in case findIndex (\(_ /\ ix) -> ix == child) children of
        Nothing -> error "child doesn't exist"
        Just childIndex -> case index children (childIndex + 1) of
                            Nothing -> Nothing
                            Just (_ /\ nextChildIdx) -> Just nextChildIdx

previousChild :: Syntax -> TreeIndexStep -> Maybe (List IxStep)
previousChild syn child =
    let children = getChildren syn
    in case findIndex (\(_ /\ ix) -> ix == child) children of
        Nothing -> error "child doesn't exist"
        Just childIndex -> case index children (childIndex - 1) of
                            Nothing -> Nothing
                            Just (_ /\ nextChildIdx) -> Just nextChildIdx

childAtStep :: Syntax -> TreeIndexStep -> Syntax
childAtStep syn idx =
    let children = getChildren syn
    in case find (\(_ /\ ix) -> ix == idx) children of
        Nothing -> error ("child doesn't exista sdflka jsdflajks df. \nLooked for index idx = "
            <> show idx
            <> "But the child indices are: "
            <> show (map snd children))
        Just (child /\ _) -> child

-- {-
-- Given some syntax, returns either the IxStepLabel associated with it, or if it
-- a node without children, like for example the type Nat, then it returns Nothing.
-- There are a few things in Syntax that I am unsure what they are, which I commented below.
-- -}
-- -- wait, why did I need this function again?
-- getStepLabelAt :: Syntax -> Maybe IxStepLabel
-- getStepLabelAt (SyntaxType (ArrowType _)) = Just IxStepArrowType
-- getStepLabelAt (SyntaxType (DataType _)) = Nothing
-- getStepLabelAt (SyntaxType (HoleType _)) = Nothing
-- getStepLabelAt (SyntaxTerm (Lam _)) = Just IxStepLam
-- getStepLabelAt (SyntaxTerm (Let _)) = Just IxStepLet
-- getStepLabelAt (SyntaxTerm (Neu _)) = Just IxStepNeu
-- getStepLabelAt (SyntaxTerm (Buf _)) = Just IxStepBuf
-- getStepLabelAt (SyntaxTerm (Data _)) = Just IxStepData
-- getStepLabelAt (SyntaxTerm (Match _)) = Just IxStepMatch
-- getStepLabelAt (SyntaxTerm (Hole _)) = Nothing
-- getStepLabelAt (SyntaxTermBind _) = Just IxStepTermBind -- error "what is a termbind"
-- getStepLabelAt (SyntaxTermId _) = Nothing
-- getStepLabelAt (SyntaxTypeBind _) = error "what is a typebind henry"
-- getStepLabelAt (SyntaxArgItem _) = Just IxStepArgItem
-- getStepLabelAt (SyntaxSumItem _) = Just IxStepSumItem
-- getStepLabelAt (SyntaxCaseItem _) = Just IxStepCaseItem
-- getStepLabelAt (SyntaxParamItem _) = Just IxStepParamItem
-- getStepLabelAt (SyntaxTermBindItem _) = Just IxStepTermBindItem -- also what is a termbind here?
-- getStepLabelAt (SyntaxList _) = error "what is syntaxList for????"