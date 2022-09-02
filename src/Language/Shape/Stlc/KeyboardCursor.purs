module KeyboardCursor where

import Data.Tuple.Nested
import Prelude
import Prim hiding (Type)
import Data.List (List(..), head, index, last, length, (:))
import Data.List.Unsafe (index')
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Tuple (fst)
import Debug (trace)
import Language.Shape.Stlc.Index (IxDown(..), IxStep(..), IxStepLabel(..))
import Language.Shape.Stlc.Syntax (Syntax(..), Type(..), Term(..))
import Language.Shape.Stlc.Syntax.TreeView (childAtStep, getChildren, nextChild, popIndex2, previousChild)

{-
Either moves cursor forwards one (forwards in the sense of the ordering that
we discussed) or returns Nothing if there is no where further to move.
-}
stepCursorForwards :: Syntax -> IxDown -> Maybe IxDown
stepCursorForwards syn idx -- = case popIndex idx of
 = case popIndex2 syn idx of
  Nothing ->
    let
      children = getChildren syn
    in
      case head children of
        Nothing -> Nothing
        Just (_ /\ newStep) -> Just (IxDown newStep)
  Just (step /\ rest) ->
    let
      child = childAtStep syn step
    in
      let
        tryInChild = stepCursorForwards child rest
      in
        case tryInChild of
          Just (IxDown idx) -> Just (IxDown (step <> idx))
          Nothing ->
            let
              nextChildStep = nextChild syn step
            in
              case nextChildStep of
                Just (newStep) -> Just (IxDown newStep)
                Nothing -> Nothing

-- Gets index pointing to the very last node in the input syntax
getLastIndex :: Syntax -> List IxStep
getLastIndex syn =
  let
    children = getChildren syn
  in
    case last children of
      Nothing -> Nil
      Just (child /\ step) -> step <> getLastIndex child

stepCursorBackwards :: Syntax -> IxDown -> Maybe IxDown
stepCursorBackwards syn idx = case popIndex2 syn idx of
  Nothing -> Nothing
  Just (step /\ rest) ->
    let
      child = childAtStep syn step
    in
      let
        tryInChild = stepCursorBackwards child rest
      in
        case tryInChild of
          Just (IxDown idx) -> Just (IxDown (step <> idx))
          Nothing ->
            let
              nextChildStep = previousChild syn step
            in
              case nextChildStep of
                Just (newStep) -> Just (IxDown (newStep <> (getLastIndex (childAtStep syn newStep))))
                Nothing -> Just (IxDown Nil)
