module Test.Main where

import Language.Shape.Stlc.ChangeAtIndex
import Language.Shape.Stlc.Changes
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Syntax
import Prelude

import Data.List (List(..), (:))
import Data.Map.Unsafe as Map
import Data.Set (fromFoldable)
import Data.Tuple (Tuple(..))
import Data.UUID as UUID
import Effect (Effect)
import Effect.Console (log)
import Language.Shape.Stlc.RenderingTypes (ChangeHistory)
import Partial.Unsafe (unsafeCrashWith)
import Test.ChangeHistory as ChangeHistory
import Unsafe (fromJust)

main :: Effect Unit
main = do
  ChangeHistory.runChangeHistory $ Map.lookup' "digLambda1" ChangeHistory.changeHistories
  pure unit
