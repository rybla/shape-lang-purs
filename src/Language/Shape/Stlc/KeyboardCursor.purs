module KeyboardCursor where

import Data.Tuple.Nested
import Prelude
import Prim hiding (Type)

import Data.List (List(..), head, index, length, (:))
import Data.List.Unsafe (index')
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple (fst)
import Debug (trace)
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
      Nothing -> let children = getChildren syn in
        case head children of
          Nothing -> Nothing
          Just (_ /\ newStep) -> Just (IxDown newStep)
      Just (step /\ rest) ->
        let child = childAtStep syn step in
        let tryInChild = stepCursorForwards child rest in
        case tryInChild of
        Just (IxDown idx) -> trace "yeetus" (\_ -> Just (IxDown (step <> idx)))
        Nothing ->
            let nextChildStep = nextChild syn step in
            case nextChildStep of
            Just (newStep) -> Just (IxDown newStep)
            Nothing -> Nothing

stepCursorBackwards :: Syntax -> IxDown -> Maybe IxDown
stepCursorBackwards _ _ = error "not implemnted yet"