module KeyboardCursor where

import Prelude
import Prim hiding (Type)

import Data.Tuple.Nested
import Data.List (List(..), index, length, (:))
import Data.List.Unsafe (index')
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple (fst)
import Language.Shape.Stlc.Index (IxDown(..), IxStep(..), IxStepLabel(..))
import Language.Shape.Stlc.Syntax (Syntax(..), Type(..), Term(..))
import Language.Shape.Stlc.Syntax.TreeView (getChildren)
import Undefined (undefined)
import Unsafe (error)

{-
Given some syntax, returns either the IxStepLabel associated with it, or if it
a node without children, like for example the type Nat, then it returns Nothing.
There are a few things in Syntax that I am unsure what they are, which I commented below.
-}
-- wait, why did I need this function again?
getStepLabelAt :: Syntax -> Maybe IxStepLabel
getStepLabelAt (SyntaxType (ArrowType _)) = Just IxStepArrowType
getStepLabelAt (SyntaxType (DataType _)) = Nothing
getStepLabelAt (SyntaxType (HoleType _)) = Nothing
getStepLabelAt (SyntaxTerm (Lam _)) = Just IxStepLam
getStepLabelAt (SyntaxTerm (Let _)) = Just IxStepLet
getStepLabelAt (SyntaxTerm (Neu _)) = Just IxStepNeu
getStepLabelAt (SyntaxTerm (Buf _)) = Just IxStepBuf
getStepLabelAt (SyntaxTerm (Data _)) = Just IxStepData
getStepLabelAt (SyntaxTerm (Match _)) = Just IxStepMatch
getStepLabelAt (SyntaxTerm (Hole _)) = Nothing
getStepLabelAt (SyntaxTermBind _) = Just IxStepTermBind -- error "what is a termbind"
getStepLabelAt (SyntaxTermId _) = Nothing
getStepLabelAt (SyntaxTypeBind _) = error "what is a typebind henry"
getStepLabelAt (SyntaxArgItem _) = Just IxStepArgItem
getStepLabelAt (SyntaxSumItem _) = Just IxStepSumItem
getStepLabelAt (SyntaxCaseItem _) = Just IxStepCaseItem
getStepLabelAt (SyntaxParamItem _) = Just IxStepParamItem
getStepLabelAt (SyntaxTermBindItem _) = Just IxStepTermBindItem -- also what is a termbind here?
getStepLabelAt (SyntaxList _) = error "what is syntaxList for????"

{-
Either moves cursor forwards one (forwards in the sense of the ordering that
we discussed) or returns Nothing if there is no where further to move.
-}
stepCursorForwards :: Syntax -> IxDown -> Maybe IxDown
stepCursorForwards syn (IxDown Nil) = Nothing
stepCursorForwards syn (IxDown ((IxStep label child) : rest))
    = let children = (getChildren syn) in
      let tryInChild = stepCursorForwards (fst (index' children child)) (wrap rest) in
      case tryInChild of
      Just (IxDown idx) -> Just (IxDown (IxStep label child : idx))
      Nothing ->
        let nextChildIx = child + 1 in
        case index children nextChildIx of
        Just (nextChildSyn /\ steps) -> Just (IxDown steps)
        Nothing -> Nothing
{-
1) call stepCursorForwards on child syntax pointed to by the next step.
    - if returns Just something, then done.
    - else, continue:
2) next child
3) if no next child then return Nothing?
-}

{-
Plan: make a function Syntax -> Int -> Syntax which returns
the nth child node of a given syntax
-}