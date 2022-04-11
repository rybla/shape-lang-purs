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

-- main :: Effect Unit
-- main = do
--   log "hello world"
--   -- ChangeHistory.runChangeHistory $ Map.lookup' "test1" ChangeHistory.changeHistories
--   ChangeHistory.runChangeHistory $ Map.lookup' "test2" ChangeHistory.changeHistories
--   pure unit

f :: Unit -> Unit 
f _ = unsafeCrashWith "error"

main :: Effect Unit 
main = do 
  pure $ f unit

