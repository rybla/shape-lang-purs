module Language.Shape.Stlc.Rendering.Token where

import Data.Array
import Prelude
import Data.Char as Char
import Data.Newtype (unwrap)
import Data.String as String
import Language.Shape.Stlc.Metacontext (Metacontext(..))
import React (ReactElement)
import React.DOM as DOM
import React.DOM.Props as Props

token :: _
token =
  { arrowType1: DOM.span [ Props.className "token" ] [ DOM.text " → " ]
  , lam1: DOM.span [ Props.className "token delimiter" ] [ DOM.text "fun " ]
  , lam2: DOM.span [ Props.className "token" ] [ DOM.text " ⇒ " ]
  , data1: DOM.span [ Props.className "token delimiter" ] [ DOM.text "type " ]
  , data2: DOM.span [ Props.className "token" ] [ DOM.text " = " ]
  , data3: DOM.span [ Props.className "token delimiter" ] [ DOM.text " in " ]
  , neu1: DOM.span [ Props.className "token" ] [ DOM.text " " ]
  , let1: DOM.span [ Props.className "token delimiter" ] [ DOM.text "let " ]
  , let2: DOM.span [ Props.className "token" ] [ DOM.text " : " ]
  , let3: DOM.span [ Props.className "token" ] [ DOM.text " = " ]
  , let4: DOM.span [ Props.className "token delimiter" ] [ DOM.text " in " ]
  , buf1: DOM.span [ Props.className "token delimiter" ] [ DOM.text "buf " ]
  , buf2: DOM.span [ Props.className "token" ] [ DOM.text " : " ]
  , buf3: DOM.span [ Props.className "token delimiter" ] [ DOM.text " in " ]
  , match1: DOM.span [ Props.className "token delimiter" ] [ DOM.text "match " ]
  , match2: DOM.span [ Props.className "token delimiter" ] [ DOM.text "with " ]
  , sumItem1: DOM.span [ Props.className "token" ] [ DOM.text "| " ]
  , sumItem2: DOM.span [ Props.className "token" ] [ DOM.text " " ]
  , caseItem1: DOM.span [ Props.className "token" ] [ DOM.text "| " ]
  , caseItem2: DOM.span [ Props.className "token" ] [ DOM.text " ⇒ " ]
  , lparen: DOM.span [ Props.className "token punctuation paren lparen" ] [ DOM.text "(" ]
  , rparen: DOM.span [ Props.className "token punctuation paren rparen" ] [ DOM.text ")" ]
  , space: DOM.span [ Props.className "token space" ] [ DOM.text " " ]
  }

indentation :: Int -> ReactElement
indentation i = DOM.span [ Props.className "indentation" ] [ DOM.text $ String.joinWith "" (replicate i "  ") ]

newline :: Metacontext -> Boolean -> Array ReactElement
newline meta indented =
  if indented then
    (if (unwrap meta).indentation == 0 then [ DOM.br', DOM.br' ] else [ DOM.br' ])
      <> [ indentation (unwrap meta).indentation ]
  else
    []

newlineOrSpace :: Metacontext -> Boolean -> Array ReactElement
newlineOrSpace meta indented =
  if indented then
    [ DOM.br', indentation (unwrap meta).indentation ]
  else
    [ token.space ]
