module Data.String.Parse where

import Control.Monad
import Data.Array
import Data.String.CodePoints
import Data.Tuple
import Data.Tuple.Nested
import Prelude

import Data.Maybe (Maybe(..), maybe')
import Data.String as String
import Debug as Debug
import Partial.Unsafe (unsafeCrashWith)

parseString' :: String -> String -> Maybe String
parseString' p s = case String.uncons p of
  Just { head: p_head, tail: p_tail } -> case String.uncons s of
    Just { head: s_head, tail: s_tail } ->
      if p_head == s_head then
        parseString' p_tail s_tail
      else
        Nothing
    Nothing -> Nothing
  Nothing -> Just s

parseString :: String -> String -> String
parseString p s =
  maybe' (\_ -> unsafeCrashWith $ "[error:parseString] expected pattern '" <> p <> "' but found '" <> s <> "'")
    identity
    (parseString' p s)

parseStringIn :: forall a. Array (String /\ a) -> String -> (String /\ a)
parseStringIn ps s = case foldl
    ( case _ of
        Just r -> \_ -> Just r
        Nothing -> \(p /\ a) -> (_ /\ a) <$> parseString' p s
    )
    Nothing
    ps of
  Just r -> r
  Nothing -> unsafeCrashWith $ "[error:parseStringIn] expected pattern in '" <> show (fst <$> ps) <> "' but found '" <> s <> "'"

parseStringWhile :: (CodePoint -> Boolean) -> String -> (String /\ String)
parseStringWhile cond s =
  case String.uncons s of 
    Just {head, tail} -> 
      if cond head then 
        let s1 /\ s2 = parseStringWhile cond tail in
        (String.singleton head <> s1) /\ s2
      else
        "" /\ s
    Nothing ->
      "" /\ s
