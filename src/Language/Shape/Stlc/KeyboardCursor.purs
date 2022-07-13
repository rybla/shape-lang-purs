module KeyboardCursor where

import Data.Tuple.Nested
import Prelude
import Prim hiding (Type)

import Data.List (List(..), index, length, (:))
import Data.List.Unsafe (index')
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple (fst)
import Language.Shape.Stlc.Index (IxDown(..), IxStep(..), IxStepLabel(..))
import Language.Shape.Stlc.Syntax (Syntax(..), Type(..), Term(..))
import Language.Shape.Stlc.Syntax.TreeView (childAtStep, getChildren, nextChild, popIndex)
import Undefined (undefined)
import Unsafe (error)

{-
Either moves cursor forwards one (forwards in the sense of the ordering that
we discussed) or returns Nothing if there is no where further to move.
-}
stepCursorForwards :: Syntax -> IxDown -> Maybe IxDown
stepCursorForwards syn idx
    = case popIndex idx of
      Nothing -> Nothing
      Just (step /\ rest) ->
        -- let children = (getChildren syn) in
        let child = childAtStep syn step in
        let tryInChild = stepCursorForwards child rest in
        case tryInChild of
        Just (IxDown idx) -> Just (IxDown (step <> idx))
        Nothing ->
            let nextChildStep = nextChild syn step in
            case nextChildStep of
            Just (newStep) -> Just (IxDown step)
            Nothing -> Nothing

{-
There is a problem here, because there are certian index steps that are skipped over and can't be
selected. So the way that it pattern matches on the first step of the IxDown doesn't work. What if
the first step is a ixStepList.tail or ixStepList.head?

So what should I do about that? I need (in TreeView.purs) some sort of function which given an index
into a syntax, pops off a part of the index and returns the rest of the index.
-}

stepCursorBackwards :: Syntax -> IxDown -> Maybe IxDown
stepCursorBackwards _ _ = error "not implemnted yet"