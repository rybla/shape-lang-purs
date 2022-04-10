module Data.Serialize where

import Data.Tuple.Nested
import Prelude
import Data.Tuple

class Serialize a where
  encode :: a -> String
  decode' :: String -> (String /\ a)

decode :: forall a. Serialize a => String -> a
decode = snd <<< decode'
