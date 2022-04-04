module Language.Shape.Stlc.RenderingAux where

import Data.Array
import Data.String as String
import Data.Homogeneous.Record
import Data.Tuple.Nested
import Data.Variant
import Language.Shape.Stlc.Recursion.Wrap
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Data.Foldable (class Foldable)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple as Tuple
import Language.Shape.Stlc.Recursion.MetaContext (MetaContext)
import Language.Shape.Stlc.Typing (Context)
import Prim.Row (class Cons)
import React (ReactElement)
import React as React
import React.DOM as DOM
import React.DOM.Props as Props
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe as Unsafe

_literal = Proxy :: Proxy "literal"

_element = Proxy :: Proxy "element"

token :: _
token =
  fromHomogeneous $ pure makeToken
    <*> homogeneous
        -- Definition 
        { dataDef_head: inj _literal "type "
        , dataDef_sep: inj _literal " = "
        , constr_head: inj _literal "| "
        , constr_sep: inj _literal " of "
        , termDef_sig_head: inj _literal "val "
        , termDef_sig_sep: inj _literal " : "
        , termDef_imp_head: inj _literal "let "
        , termDef_imp_sep: inj _literal " = "
        -- Type
        , arrow_sep: inj _literal " → "
        -- Term
        , lambda_head: inj _literal "fun "
        , lambda_sep: inj _literal " ⇒ "
        , match_head: inj _literal "match "
        , match_sep: inj _literal " with "
        -- Case
        , case_head: inj _literal "| "
        , case_sep: inj _literal " ⇒ "
        -- Parameter
        , param_sep: inj _literal " : "
        -- ArgItems
        , argItems_end: inj _literal "•"
        -- Separators
        , defSep: inj _literal "•"
        , constrSep: inj _literal "•"
        , paramSep: inj _literal "•"
        , caseSep: inj _literal "•"
        , argSep: inj _literal " "
        , termIdSep: inj _literal "•"
        -- Misc
        , lparen: inj _literal "("
        , rparen: inj _literal ")"
        , newline: inj _element DOM.br'
        , space: inj _literal " "
        }
  where
  makeToken :: Variant ( literal :: String, element :: ReactElement ) -> ReactElement
  makeToken value =
    DOM.span [ Props.className "token" ]
      [ case_
          # on _literal (\str -> DOM.text str)
          # on _element identity
          $ value
      ]

-- keyword :: _
-- keyword =
--   fromHomogeneous $ (pure makeKeyword)
--     <*> homogeneous
--         { data_head: "type"
--         , sig_head: "val"
--         , imp_head: "let"
--         , constr_sep: "of"
--         , match_head: "match"
--         , match_sep: "with"
--         , lambda_head: "fun"
--         , let_: "let"
--         , fun: "fun"
--         , in_: "in"
--         , lambda: "λ"
--         }
--   where
--   makeKeyword label =
--     DOM.span
--       [ Props.className ("keyword " <> label) ]
--       [ DOM.text label ]
-- punctuation :: _
-- punctuation =
--   fromHomogeneous $ (pure makePunctuation)
--     <*> homogeneous
--         { period: "."
--         , comma: ","
--         , lparen: "("
--         , rparen: ")"
--         , alt: "|"
--         , arrow: "→"
--         , termdef: "="
--         , typedef: "="
--         , colon: ":"
--         , mapsto: "⇒"
--         , space: " "
--         , newline: "\n"
--         , sep: "•"
--         }
--   where
--   makePunctuation label =
--     if label == "\n" then
--       DOM.br'
--     else
--       DOM.span
--         [ Props.className "punctuation" ]
--         [ DOM.text label ]
intercalateHTML inter = DOM.span' <<< intercalate inter <<< map singleton

intersperseLeftHTML inter = DOM.span' <<< foldMap (\x -> inter <> [ x ])

intersperseRightHTML inter = DOM.span' <<< foldMap (\x -> [ x ] <> inter)

indentation :: forall r. { indented :: Boolean | r } -> MetaContext -> Array ReactElement
indentation { indented } metaGamma =
  if indented then
    [ DOM.span [ Props.className "indentation" ] [ DOM.text $ "\n" <> String.joinWith "" (replicate metaGamma.indentation "  ") ] ]
  else
    []

-- indentation :: MetaContext -> ReactElement
-- indentation metaGamma =
--   DOM.span [ Props.className "indentation" ]
--     [ DOM.text $ String.joinWith "" $ replicate metaGamma.indentation "  " ]
-- indentOrSpace :: forall r. { indented :: Boolean | r } -> MetaContext -> ReactElement
-- indentOrSpace arg@{ indented } metaGamma =
--   if indented then
--     DOM.span' [ indent arg metaGamma ]
--   else
--     DOM.span' [ token.space ]
-- indentOrNothing :: forall r. { indented :: Boolean | r } -> MetaContext -> ReactElement
-- indentOrNothing arg@{ indented } metaGamma =
--   if indented then
--     DOM.span' [ indent arg metaGamma ]
--   else
--     DOM.span' []
-- without newline
