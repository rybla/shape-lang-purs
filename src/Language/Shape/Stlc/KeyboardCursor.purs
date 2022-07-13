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

stepCursorBackwards :: Syntax -> IxDown -> Maybe IxDown
stepCursorBackwards = error "not implemnted yet"