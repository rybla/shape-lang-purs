module Language.Shape.Stlc.RenderingAuxOld where

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
import Type.Proxy (Proxy)
import Undefined (undefined)
import Unsafe as Unsafe

keywords :: forall w i. Map.Map String (HH.HTML w i)
keywords =
  Map.fromFoldable <<< map makeKeyword
    $ [ "data"
      , "match"
      , "with"
      , "let"
      ]
  where
  makeKeyword title = title /\ HH.span [ HP.class_ (HH.ClassName (List.intercalate " " [ title, " keyword" ])) ] [ HH.text title ]

renderKeyword :: forall w i. String -> HH.HTML w i
renderKeyword title = Unsafe.lookup title keywords

punctuations :: forall w i. Map.Map String (HH.HTML w i)
punctuations =
  Map.fromFoldable
    $ ( map (Tuple.uncurry makePunctuation)
          $ [ "period" /\ "."
            , "comma" /\ ","
            , "colon" /\ ":"
            , "lparen" /\ "("
            , "rparen" /\ ")"
            , "alt" /\ "|"
            , "arrow" /\ "->"
            , "assign" /\ ":="
            , "mapsto" /\ "=>"
            , "space" /\ " "
            , "indent" /\ "  "
            ]
      )
    <> [ "newline" /\ HH.br_ ]
  where
  makePunctuation title punc = title /\ HH.span [ HP.class_ (HH.ClassName (List.intercalate " " [ title, "punctuation" ])) ] [ HH.text punc ]

renderPunctuation :: forall w i. String -> HH.HTML w i
renderPunctuation title = Unsafe.lookup title punctuations

-- intercalate
intercalate :: forall t126 t127 t140. Foldable t140 => Functor t140 => List.List (HH.HTML t126 t127) -> t140 (HH.HTML t126 t127) -> HH.HTML t126 t127
intercalate inter = HH.span_ <<< List.toUnfoldable <<< List.intercalate inter <<< map List.singleton

intercalateAlts = intercalate $ List.fromFoldable [ renderPunctuation "space", renderPunctuation "alt", renderPunctuation "space" ]

intercalateCommas = intercalate $ List.fromFoldable [ renderPunctuation "comma", renderPunctuation "space" ]

intercalateNewlines = intercalate $ List.fromFoldable [ renderPunctuation "newline" ]

intercalateDoubleNewlines = intercalate $ List.fromFoldable [ renderPunctuation "newline", renderPunctuation "newline" ]

intercalateSpaces = intercalate $ List.fromFoldable [ renderPunctuation "space" ]

-- intersperse
intersperseLeft inter = HH.span_ <<< List.toUnfoldable <<< List.foldMap (\x -> inter <> (List.singleton x))

intersperseLeftSpaces = intersperseLeft $ List.fromFoldable [ renderPunctuation "space" ]

intersperseLeftNewlines = intersperseLeft $ List.fromFoldable [ renderPunctuation "newline" ]
