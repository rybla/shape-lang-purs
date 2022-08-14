module Language.Shape.Stlc.Rendering.Token where

import Data.Array
import Data.Tuple.Nested
import Prelude
import Data.Char as Char
import Data.Homogeneous.Record (fromHomogeneous, homogeneous)
import Data.Newtype (unwrap)
import Data.String as String
import Language.Shape.Stlc.Metacontext (Metacontext(..))
import React (ReactElement)
import React.DOM as DOM
import React.DOM.Props as Props
import Type.Proxy (Proxy(..))
import Undefined (undefined)

data SyntaxTheme
  = MlSyntaxTheme
  | TypescriptSyntaxTheme
  | MinimalistSyntaxTheme
  | HaskellSynaxTheme
  | VerboseSyntaxTheme

derive instance eqSyntaxTheme :: Eq SyntaxTheme

syntaxtheme_token :: _
syntaxtheme_token =
  { typescript:
      (fromHomogeneous <<< homogeneous)
        { lam1: [] :: Array ReactElement
        , data1: makeToken "type " "keyword"
        , data3: makeToken " ; " "punctuation"
        , let1: makeToken "let " "keyword"
        , buf1: makeToken "/* " "keyword"
        , buf3: makeToken " */ " "punctuation"
        , buf4: [] :: Array ReactElement
        , match1: makeToken "switch " "keyword"
        , match2: makeToken " { " "punctuation"
        , match3: makeToken " } " "punctuation"
        , caseItem1: makeToken "case " "keyword"
        , caseItem3: makeToken ": { " "punctuation"
        , caseItem4: makeToken " } " "punctuation"
        }
  , minimalist:
      (fromHomogeneous <<< homogeneous)
        { lam1: []
        , data1: [] :: Array ReactElement
        , data2: [] :: Array ReactElement
        , data3: makeToken " ; " "punctuation"
        , sumItem1: []
        , let1: [] :: Array ReactElement
        , let4: makeToken " ; " "punctuation"
        , buf1: makeToken "buf " "keyword"
        , buf4: makeToken " ; " "punctuation"
        , match1: [] :: Array ReactElement
        , match2: [] :: Array ReactElement
        , caseItem1: [] :: Array ReactElement
        }
  , ml:
      (fromHomogeneous <<< homogeneous)
        { lam1: makeToken "fun " "keyword"
        , lam2: makeToken " -> " ""
        , data1: makeToken "type " "keyword"
        , data3: makeToken " in " "keyword"
        , caseItem3: makeToken " -> " "punctuation"
        , let1: makeToken "let " "keyword"
        , buf1: makeToken "let _ = " "keyword"
        , buf2: makeToken " : " ""
        , buf3: [] :: Array ReactElement
        , buf4: makeToken " in " "keyword"
        }
  , haskell:
      (fromHomogeneous <<< homogeneous)
        { arrowType1: makeToken " → " "punctuation"
        , lam1: makeToken "λ " "punctuation"
        , lam2: makeToken " -> " ""
        , data1: makeToken "data " "keyword"
        , data2: makeToken " = " ""
        , data3: makeToken " in " "keyword"
        , sumItem1: makeToken "| " ""
        , let1: makeToken "let " "keyword"
        , let2: makeToken " :: " ""
        , let3: makeToken " = " ""
        , let4: makeToken " in " ""
        , buf1: makeToken "let _ = " "keyword"
        , buf2: makeToken " :: " ""
        , buf4: makeToken " in " "keyword"
        , match1: makeToken "case " "keyword"
        , match2: makeToken " of " "keyword"
        , match3: [] :: Array ReactElement
        , caseItem1: []
        , caseItem3: makeToken "-> " ""
        }
  , verbose:
      (fromHomogeneous <<< homogeneous)
        { lam1: makeToken "function " "keyword"
        , data1: makeToken "define datatype " "keyword"
        , data2: makeToken " by " "keyword"
        , data3: makeToken " in " "keyword"
        , sumItem1: makeToken " constructor " "keyword"
        , sumItem2: makeToken " of " "keyword"
        , let1: makeToken "define term " "keyword"
        , let2: makeToken " of type " "keyword"
        , let3: makeToken " to be " "keyword"
        , let4: makeToken " in " "keyword"
        , buf1: makeToken "buffer term " "keyword"
        , buf4: makeToken " in " "keyword"
        , match1: makeToken "pattern match on " "keyword"
        , match2: makeToken " where " "keyword"
        , caseItem1: makeToken " in case " "keyword"
        , caseItem3: makeToken "then " "keyword"
        }
  , default:
      (fromHomogeneous <<< homogeneous)
        { arrowType1: makeToken " → " "punctuation"
        , lam1: makeToken "fun " "keyword"
        , lam2: makeToken " ⇒ " ""
        , data1: makeToken "data " "keyword"
        , data2: makeToken " = " ""
        , data3: makeToken " in " "keyword"
        , sumItem1: makeToken "| " ""
        , sumItem2: makeToken " " "token space"
        , sumItem3: [] :: Array ReactElement
        , let1: makeToken "let " "keyword"
        , let2: makeToken " : " ""
        , let3: makeToken " = " ""
        , let4: makeToken " ; " ""
        , buf1: makeToken "buf " "keyword"
        , buf2: makeToken " : " ""
        , buf3: [] :: Array ReactElement
        , buf4: makeToken " ; " "keyword"
        , match1: makeToken "match " "keyword"
        , match2: makeToken " with " "keyword"
        , match3: [] :: Array ReactElement
        , caseItem1: makeToken "| " ""
        , caseItem2: makeToken " " "token space"
        , caseItem3: makeToken "⇒ " ""
        , caseItem4: [] :: Array ReactElement
        , lparen: makeToken "(" "token punctuation paren lparen"
        , rparen: makeToken ")" "token punctuation paren rparen"
        , space: makeToken " " "token space"
        }
  }

-- token' :: ProxySyntaxTheme 
token :: _
token =
  (fromHomogeneous <<< homogeneous)
    { arrowType1:
        case _ of
          _ -> syntaxtheme_token.default.arrowType1
    , lam1:
        case _ of
          TypescriptSyntaxTheme -> syntaxtheme_token.typescript.lam1
          MinimalistSyntaxTheme -> syntaxtheme_token.minimalist.lam1
          MlSyntaxTheme -> syntaxtheme_token.ml.lam1
          HaskellSynaxTheme -> syntaxtheme_token.haskell.lam1
          VerboseSyntaxTheme -> syntaxtheme_token.verbose.lam1
    , lam2:
        case _ of
          MlSyntaxTheme -> syntaxtheme_token.ml.lam2
          HaskellSynaxTheme -> syntaxtheme_token.haskell.lam2
          _ -> syntaxtheme_token.default.lam2
    , data1:
        case _ of
          TypescriptSyntaxTheme -> syntaxtheme_token.typescript.data1
          MinimalistSyntaxTheme -> syntaxtheme_token.minimalist.data1
          MlSyntaxTheme -> syntaxtheme_token.ml.data1
          HaskellSynaxTheme -> syntaxtheme_token.haskell.data1
          VerboseSyntaxTheme -> syntaxtheme_token.verbose.data1
    , data2:
        case _ of
          MinimalistSyntaxTheme -> syntaxtheme_token.minimalist.data2
          HaskellSynaxTheme -> syntaxtheme_token.haskell.data2
          VerboseSyntaxTheme -> syntaxtheme_token.verbose.data2
          _ -> syntaxtheme_token.default.data2
    , data3:
        case _ of
          TypescriptSyntaxTheme -> syntaxtheme_token.typescript.data3
          MinimalistSyntaxTheme -> syntaxtheme_token.minimalist.data3
          MlSyntaxTheme -> syntaxtheme_token.ml.data3
          HaskellSynaxTheme -> syntaxtheme_token.haskell.data3
          VerboseSyntaxTheme -> syntaxtheme_token.verbose.data3
    , sumItem1:
        case _ of
          HaskellSynaxTheme -> syntaxtheme_token.haskell.sumItem1
          MinimalistSyntaxTheme -> syntaxtheme_token.minimalist.sumItem1
          VerboseSyntaxTheme -> syntaxtheme_token.verbose.sumItem1
          _ -> syntaxtheme_token.default.sumItem1
    , sumItem2:
        case _ of
          VerboseSyntaxTheme -> syntaxtheme_token.verbose.sumItem2
          _ -> syntaxtheme_token.default.sumItem2
    , sumItem3:
        case _ of
          _ -> syntaxtheme_token.default.sumItem3
    , let1:
        case _ of
          TypescriptSyntaxTheme -> syntaxtheme_token.typescript.let1
          MinimalistSyntaxTheme -> syntaxtheme_token.minimalist.let1
          MlSyntaxTheme -> syntaxtheme_token.ml.let1
          HaskellSynaxTheme -> syntaxtheme_token.haskell.let1
          VerboseSyntaxTheme -> syntaxtheme_token.verbose.let1
    , let2:
        case _ of
          HaskellSynaxTheme -> syntaxtheme_token.haskell.let2
          VerboseSyntaxTheme -> syntaxtheme_token.verbose.let2
          _ -> syntaxtheme_token.default.let2
    , let3:
        case _ of
          HaskellSynaxTheme -> syntaxtheme_token.haskell.let3
          VerboseSyntaxTheme -> syntaxtheme_token.verbose.let3
          _ -> syntaxtheme_token.default.let3
    , let4:
        case _ of
          MinimalistSyntaxTheme -> syntaxtheme_token.minimalist.let4
          HaskellSynaxTheme -> syntaxtheme_token.haskell.let4
          VerboseSyntaxTheme -> syntaxtheme_token.verbose.let4
          _ -> syntaxtheme_token.default.let4
    , buf1:
        case _ of
          MlSyntaxTheme -> syntaxtheme_token.typescript.buf1
          TypescriptSyntaxTheme -> syntaxtheme_token.typescript.buf1
          MinimalistSyntaxTheme -> syntaxtheme_token.minimalist.buf1
          HaskellSynaxTheme -> syntaxtheme_token.haskell.buf1
          VerboseSyntaxTheme -> syntaxtheme_token.verbose.buf1
    , buf2:
        case _ of
          MlSyntaxTheme -> syntaxtheme_token.ml.buf2
          HaskellSynaxTheme -> syntaxtheme_token.haskell.buf2
          _ -> syntaxtheme_token.default.buf2
    , buf3:
        case _ of
          MlSyntaxTheme -> syntaxtheme_token.ml.buf3
          TypescriptSyntaxTheme -> syntaxtheme_token.typescript.buf3
          _ -> syntaxtheme_token.default.buf4
    , buf4:
        case _ of
          MlSyntaxTheme -> syntaxtheme_token.ml.buf4
          TypescriptSyntaxTheme -> syntaxtheme_token.typescript.buf4
          MinimalistSyntaxTheme -> syntaxtheme_token.minimalist.buf4
          HaskellSynaxTheme -> syntaxtheme_token.haskell.buf4
          VerboseSyntaxTheme -> syntaxtheme_token.verbose.buf4
    , match1:
        case _ of
          TypescriptSyntaxTheme -> syntaxtheme_token.typescript.match1
          MinimalistSyntaxTheme -> syntaxtheme_token.minimalist.match1
          HaskellSynaxTheme -> syntaxtheme_token.haskell.match1
          VerboseSyntaxTheme -> syntaxtheme_token.verbose.match1
          _ -> syntaxtheme_token.default.match1
    , match2:
        case _ of
          TypescriptSyntaxTheme -> syntaxtheme_token.typescript.match2
          MinimalistSyntaxTheme -> syntaxtheme_token.minimalist.match2
          VerboseSyntaxTheme -> syntaxtheme_token.verbose.match2
          HaskellSynaxTheme -> syntaxtheme_token.haskell.match2
          _ -> syntaxtheme_token.default.match2
    , match3:
        case _ of
          TypescriptSyntaxTheme -> syntaxtheme_token.typescript.match3
          HaskellSynaxTheme -> syntaxtheme_token.haskell.match3
          _ -> syntaxtheme_token.default.match3
    , caseItem1:
        case _ of
          TypescriptSyntaxTheme -> syntaxtheme_token.typescript.caseItem1
          MinimalistSyntaxTheme -> syntaxtheme_token.minimalist.caseItem1
          HaskellSynaxTheme -> syntaxtheme_token.haskell.caseItem1
          VerboseSyntaxTheme -> syntaxtheme_token.verbose.caseItem1
          _ -> syntaxtheme_token.default.caseItem1
    , caseItem2:
        case _ of
          _ -> syntaxtheme_token.default.caseItem2
    , caseItem3:
        case _ of
          TypescriptSyntaxTheme -> syntaxtheme_token.typescript.caseItem3
          HaskellSynaxTheme -> syntaxtheme_token.haskell.caseItem3
          VerboseSyntaxTheme -> syntaxtheme_token.verbose.caseItem3
          _ -> syntaxtheme_token.default.caseItem3
    , caseItem4:
        case _ of
          TypescriptSyntaxTheme -> syntaxtheme_token.typescript.caseItem4
          _ -> syntaxtheme_token.default.caseItem4
    , lparen:
        case _ of
          _ -> syntaxtheme_token.default.lparen
    , rparen:
        case _ of
          _ -> syntaxtheme_token.default.rparen
    , space:
        case _ of
          _ -> syntaxtheme_token.default.space
    }

makeToken :: String -> String -> Array ReactElement
makeToken str className = [ DOM.span [ Props.className ("token " <> className) ] [ DOM.text str ] ]

indentation :: Int -> ReactElement
indentation i = DOM.span [ Props.className "indentation" ] [ DOM.text $ String.joinWith "" (replicate i "  ") ]

newline :: Metacontext -> Boolean -> Array ReactElement
newline meta indented =
  if indented then
    (if (unwrap meta).indentation == 0 then [ DOM.br', DOM.br' ] else [ DOM.br' ])
      <> [ indentation (unwrap meta).indentation ]
  else
    []

newlineOrSpace :: SyntaxTheme -> Metacontext -> Boolean -> Array ReactElement
newlineOrSpace synthm meta indented =
  if indented then
    [ DOM.br', indentation (unwrap meta).indentation ]
  else
    token.space synthm
