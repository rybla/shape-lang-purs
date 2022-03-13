module Language.Shape.Stlc.RenderingAux where

import Data.Homogeneous.Record
import Data.Tuple.Nested
import Language.Shape.Stlc.Recursion.Wrap
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple as Tuple
import Language.Shape.Stlc.Recursion.MetaContext (MetaContext)
import Language.Shape.Stlc.Typing (Context)
import Prim.Row (class Cons)
import React as React
import React.DOM as DOM
import React.DOM.Props as Props
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
        , in_: "in"
        }
  where
  makeKeyword label =
    DOM.span
      [ Props.className ("keyword " <> label) ]
      [ DOM.text label ]

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
        , termdef: "="
        , typedef: "::="
        , colon: ":"
        , mapsto: "=>"
        , space: " "
        , newline: "\n"
        }
  where
  makePunctuation label =
    if label == "\n" then
      DOM.br'
    else
      DOM.span
        [ Props.className "punctuation" ]
        [ DOM.text label ]

intercalateHTML inter = DOM.span' <<< Array.intercalate inter <<< map Array.singleton

intersperseLeftHTML inter = DOM.span' <<< Array.foldMap (\x -> inter <> [ x ])

intersperseRightHTML inter = DOM.span' <<< Array.foldMap (\x -> [ x ] <> inter)

indent :: forall r. { indented :: Boolean | r } -> MetaContext -> React.ReactElement
indent { indented } metaGamma =
  if indented then
    DOM.span' [ punctuation.newline, indentation metaGamma ]
  else
    DOM.span' []

indentOrSpace :: forall r. { indented :: Boolean | r } -> MetaContext -> React.ReactElement
indentOrSpace { indented } metaGamma =
  if indented then
    DOM.span' [ punctuation.newline, indentation metaGamma ]
  else
    DOM.span' [ punctuation.space ]

-- without newline
indentation :: MetaContext -> React.ReactElement
indentation metaGamma =
  DOM.span
    [ Props.className "indentation" ]
    $ Array.replicate metaGamma.indentation (DOM.span [ Props.className "indent" ] [])
