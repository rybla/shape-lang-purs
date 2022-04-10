module Test.Main where

import Data.String.Parse
import Prelude
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log <<< show $ parseString' "a" "a"
  pure unit
