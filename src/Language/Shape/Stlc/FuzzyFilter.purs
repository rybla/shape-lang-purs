module Language.Shape.Stlc.FuzzyFilter where

import Prelude
import Data.Fuzzy as Fuzzy
import Data.Maybe (Maybe(..))
import Undefined (undefined)

fuzzyDistance :: String -> String -> Maybe Fuzzy.Distance
fuzzyDistance pat str = case Fuzzy.matchStr true pat str of
  Fuzzy.FuzzyStr fuzzyStr -> case fuzzyStr.distance of
    Fuzzy.Distance a b c d e f
      | a > 0 -> Nothing -- unacceptably distant 
    d -> Just d

{-
newtype FuzzyStr = FuzzyStr
  { original :: String
  , segments :: Segments
  , distance :: Distance
  , ratio    :: Rational
  }
-}
