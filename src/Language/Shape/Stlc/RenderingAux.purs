module Language.Shape.Stlc.RenderingAux where

import Data.Homogeneous.Record
import Data.Tuple.Nested
import Language.Shape.Stlc.Recursion.Wrap
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import App as App
import Data.Foldable (class Foldable)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple as Tuple
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Language.Shape.Stlc.Typing (Context)
import Prim.Row (class Cons)
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe as Unsafe

keyword :: _
keyword =
  fromHomogeneous $ (pure makeKeyword)
    <*> homogeneous
        { data_: "data"
        , match: "match"
        , with: "with"
        , let_: "let"
        , in: "in"
        }
  where
  makeKeyword label =
    HH.span
      [ HP.class_ (HH.ClassName $ List.intercalate " " [ "keyword", label ]) ]
      [ HH.text label ]

punctuation :: _
punctuation =
  fromHomogeneous $ (pure makePunctuation)
    <*> homogeneous
        { period: "."
        , comma: ","
        , lparen: "("
        , rparen: ")"
        , alt: "|"
        , arrow: "->"
        , termdef: ":="
        , typedef: "::="
        , colon: ":"
        , mapsto: "=>"
        , space: " "
        , indent: "  "
        , newline: "\n"
        }
  where
  makePunctuation label =
    if label == "\n" then
      HH.br_
    else
      HH.span
        [ HP.class_ (HH.ClassName $ List.intercalate " " [ "punctuation", label ]) ]
        [ HH.text label ]

intercalateHTML inter = HH.span_ <<< List.toUnfoldable <<< List.intercalate inter <<< map List.singleton

intersperseLeftHTML inter = HH.span_ <<< List.toUnfoldable <<< List.foldMap (\x -> inter <> (List.singleton x))
