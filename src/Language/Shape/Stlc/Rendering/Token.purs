module Language.Shape.Stlc.Rendering.Token where

import Language.Shape.Stlc.Metadata
import Prelude
import Data.Array (concat, foldMap, intercalate, null, replicate)
import Data.Homogeneous.Record (fromHomogeneous, homogeneous)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.String as String
import Language.Shape.Stlc.Metacontext (Metacontext(..), incrementIndentation, incrementIndentationUnless)
import React (ReactElement)
import React.DOM as DOM
import React.DOM.Props as Props

type Res
  = Array ReactElement

type SyntaxTheme
  = { meta ::
        { name :: String
        }
    , type_ ::
        { arr :: { dom :: Res, cod :: Res, dom_assoc :: Boolean, cod_arr :: Boolean, cod_assoc :: Boolean, meta :: ArrowTypeMetadata, metactx :: Metacontext } -> Res
        , data_ :: { typeId :: Res, meta :: DataTypeMetadata, metactx :: Metacontext } -> Res
        , hole :: { holeId :: Res, weakening :: Maybe Res, meta :: HoleTypeMetadata, metactx :: Metacontext } -> Res
        }
    , term ::
        { lam :: { termBind :: Res, body :: Res, parent_lam :: Boolean, body_lam :: Boolean, body_assoc :: Boolean, meta :: LamMetadata, metactx :: Metacontext } -> Res
        , neu :: { termId :: Res, argItems :: Array Res, meta :: NeuMetadata, metactx :: Metacontext } -> Res
        , let_ :: { termBind :: Res, sign :: Res, impl :: Res, body :: Res, meta :: LetMetadata, metactx :: Metacontext, body_noindent :: Boolean } -> Res
        , buf :: { sign :: Res, impl :: Res, body :: Res, meta :: BufMetadata, metactx :: Metacontext, body_noindent :: Boolean } -> Res
        , data_ :: { typeBind :: Res, sumItems :: Array Res, body :: Res, meta :: DataMetadata, metactx :: Metacontext, body_noindent :: Boolean } -> Res
        , match :: { term :: Res, caseItems :: Array Res, meta :: MatchMetadata, metactx :: Metacontext } -> Res
        , hole :: { hole :: Res, meta :: HoleMetadata, metactx :: Metacontext } -> Res
        }
    , argItem :: { term :: Res, term_assoc :: Boolean, meta :: ArgItemMetadata, metactx :: Metacontext } -> Res
    , typeBind :: { typeBind :: Res, meta :: TypeBindMetadata, metactx :: Metacontext } -> Res
    , termBind :: { termBind :: Res, meta :: TermBindMetadata, metactx :: Metacontext } -> Res
    , sumItem :: { termBind :: Res, paramItems :: Array Res, meta :: SumItemMetadata, metactx :: Metacontext } -> Res
    , caseItem :: { termId :: Res, termBindItems :: Array Res, body :: Res, meta :: CaseItemMetadata, metactx :: Metacontext } -> Res
    , paramItem :: { type_ :: Res, meta :: ParamItemMetadata, metactx :: Metacontext } -> Res
    , termBindItem :: { termBind :: Res, meta :: TermBindItemMetadata, metactx :: Metacontext } -> Res
    , dataContextItem :: { typeBind :: Res, metactx :: Metacontext } -> Res
    , varContextItem :: { termId :: Res, type_ :: Res, metactx :: Metacontext } -> Res
    }

defaultSyntaxTheme :: SyntaxTheme
defaultSyntaxTheme =
  { meta:
      { name: "default"
      }
  , type_:
      { arr: \{ dom, cod, dom_assoc, cod_arr, cod_assoc, meta, metactx } -> concat [ assocIf dom_assoc dom, tk.space, tk.arrow, tk.space, cod ]
      , data_: \{ typeId, meta, metactx } -> typeId
      , hole: \{ holeId, weakening, meta, metactx } -> holeId
      }
  , term:
      { lam: \{ termBind, body, parent_lam, body_lam, body_assoc, meta, metactx } -> concat [ tk.lambda, tk.space, termBind, tk.space, tk.mapsto, indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedBody, body ]
      , neu: \{ termId, argItems, meta, metactx } -> concat [ termId, if null argItems then [] else concat [ tk.space, intercalate tk.space argItems ] ]
      , let_: \{ termBind, sign, impl, body, meta, metactx, body_noindent } -> concat [ tk.let_, tk.space, termBind, tk.space, tk.colon, indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedSign, sign, tk.space, tk.equals, indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedImpl, impl, tk.space, if (unwrap meta).indentedBody then indent metactx else concat [ tk.in_, tk.space ], body ]
      , buf: \{ sign, impl, body, meta, metactx, body_noindent } -> concat [ tk.buf, indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedImpl, impl, tk.space, tk.colon, indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedSign, sign, if (unwrap meta).indentedBody then indent metactx else concat [ tk.in_, tk.space ], body ]
      , data_: \{ typeBind, sumItems, body, meta, metactx, body_noindent } -> concat [ tk.data_, tk.space, typeBind, tk.space, tk.equals, foldMap (concat [ indent (incrementIndentation metactx), tk.bar, tk.space ] <> _) sumItems, tk.space, if (unwrap meta).indentedBody then indent metactx else concat [ tk.in_, tk.space ], body ]
      , match: \{ term, caseItems, meta, metactx } -> concat [ tk.match, tk.space, term, tk.space, tk.with, foldMap (concat [ indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedCaseItems, tk.bar, tk.space ] <> _) caseItems ]
      , hole: \{ hole, meta, metactx } -> hole
      }
  -- TODO should this have incrementIndentation?
  , argItem: \{ term, term_assoc, meta, metactx } -> concat [ if (unwrap meta).indented then indent metactx else [], assocIf term_assoc term ]
  , typeBind: \{ typeBind, meta, metactx } -> typeBind
  , termBind: \{ termBind, meta, metactx } -> termBind
  , sumItem: \{ termBind, paramItems, meta, metactx } -> concat [ termBind, tk.space, intercalate tk.space paramItems ]
  , caseItem: \{ termId, termBindItems, body, meta, metactx } -> concat [ termId, if null termBindItems then [] else concat [ tk.space, intercalate tk.space termBindItems ], tk.space, tk.mapsto, tk.space, body ]
  , paramItem: \{ type_, meta, metactx } -> type_
  , termBindItem: \{ termBind, meta, metactx } -> termBind
  , dataContextItem: \{ typeBind, metactx } -> concat [ tk.data_, tk.space, typeBind ]
  , varContextItem: \{ termId, type_, metactx } -> concat [ termId, tk.space, tk.colon, tk.space, type_ ]
  }

expandedSyntaxTheme :: SyntaxTheme
expandedSyntaxTheme =
  { meta:
      { name: "expanded"
      }
  , type_:
      { arr: \{ dom, cod, dom_assoc, cod_arr, cod_assoc, meta, metactx } -> concat [ assocIf dom_assoc dom, tk.space, tk.arrow, tk.space, assocIf cod_arr cod ]
      , data_: \{ typeId, meta, metactx } -> typeId
      , hole: \{ holeId, weakening, meta, metactx } -> holeId
      }
  , term:
      { lam: \{ termBind, body, parent_lam, body_lam, body_assoc, meta, metactx } -> concat [ tk.lambda, tk.space, termBind, tk.space, tk.mapsto, indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedBody, body ]
      , neu: \{ termId, argItems, meta, metactx } -> concat [ termId, if null argItems then [] else concat [ tk.space, intercalate tk.space argItems ] ]
      , let_: \{ termBind, sign, impl, body, meta, metactx, body_noindent } -> concat [ tk.let_, tk.space, termBind, tk.space, tk.colon, indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedSign, sign, tk.space, tk.equals, indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedImpl, impl, tk.space, if (unwrap meta).indentedBody then indent metactx else concat [ tk.in_, tk.space ], body ]
      , buf: \{ sign, impl, body, meta, metactx, body_noindent } -> concat [ tk.buf, indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedImpl, impl, tk.space, tk.colon, indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedSign, sign, if (unwrap meta).indentedBody then indent metactx else concat [ tk.in_, tk.space ], body ]
      , data_: \{ typeBind, sumItems, body, meta, metactx, body_noindent } -> concat [ tk.data_, tk.space, typeBind, tk.space, tk.equals, foldMap (concat [ indent (incrementIndentation metactx), tk.bar, tk.space ] <> _) sumItems, tk.space, if (unwrap meta).indentedBody then indent metactx else concat [ tk.in_, tk.space ], body ]
      , match: \{ term, caseItems, meta, metactx } -> concat [ tk.match, tk.space, term, tk.space, tk.with, foldMap (concat [ indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedCaseItems, tk.bar, tk.space ] <> _) caseItems ]
      , hole: \{ hole, meta, metactx } -> hole
      }
  -- TODO should this have incrementIndentation?
  , argItem: \{ term, term_assoc, meta, metactx } -> concat [ if (unwrap meta).indented then indent metactx else [], assocIf term_assoc term ]
  , typeBind: \{ typeBind, meta, metactx } -> typeBind
  , termBind: \{ termBind, meta, metactx } -> termBind
  , sumItem: \{ termBind, paramItems, meta, metactx } -> concat [ termBind, tk.space, intercalate tk.space paramItems ]
  , caseItem: \{ termId, termBindItems, body, meta, metactx } -> concat [ termId, if null termBindItems then [] else concat [ tk.space, intercalate tk.space termBindItems ], tk.space, tk.mapsto, tk.space, body ]
  , paramItem: \{ type_, meta, metactx } -> type_
  , termBindItem: \{ termBind, meta, metactx } -> termBind
  , dataContextItem: \{ typeBind, metactx } -> concat [ tk.data_, tk.space, typeBind ]
  , varContextItem: \{ termId, type_, metactx } -> concat [ termId, tk.space, tk.colon, tk.space, type_ ]
  }

contractedSyntaxTheme :: SyntaxTheme
contractedSyntaxTheme =
  { meta:
      { name: "contracted"
      }
  , type_:
      { arr: \{ dom, cod, dom_assoc, cod_arr, cod_assoc, meta, metactx } -> concat [ assocIf dom_assoc dom, tk.space, tk.arrow, tk.space, cod ]
      , data_: \{ typeId, meta, metactx } -> typeId
      , hole: \{ holeId, weakening, meta, metactx } -> holeId
      }
  , term:
      { lam: \{ termBind, body, parent_lam, body_lam, body_assoc, meta, metactx } -> concat [ if parent_lam then [] else concat [ tk.lambda, tk.space ], termBind, if body_lam then [] else concat [ tk.space, tk.mapsto ], indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedBody, body ]
      , neu: \{ termId, argItems, meta, metactx } -> concat [ termId, if null argItems then [] else concat [ tk.space, intercalate tk.space argItems ] ]
      , let_: \{ termBind, sign, impl, body, meta, metactx, body_noindent } -> concat [ tk.let_, tk.space, termBind, tk.space, tk.colon, indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedSign, sign, tk.space, tk.equals, indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedImpl, impl, tk.space, if (unwrap meta).indentedBody then indent metactx else concat [ tk.in_, tk.space ], body ]
      , buf: \{ sign, impl, body, meta, metactx, body_noindent } -> concat [ tk.buf, indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedImpl, impl, tk.space, tk.colon, indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedSign, sign, if (unwrap meta).indentedBody then indent metactx else concat [ tk.in_, tk.space ], body ]
      , data_: \{ typeBind, sumItems, body, meta, metactx, body_noindent } -> concat [ tk.data_, tk.space, typeBind, tk.space, tk.equals, foldMap (concat [ indent (incrementIndentation metactx), tk.bar, tk.space ] <> _) sumItems, tk.space, if (unwrap meta).indentedBody then indent metactx else concat [ tk.in_, tk.space ], body ]
      , match: \{ term, caseItems, meta, metactx } -> concat [ tk.match, tk.space, term, tk.space, tk.with, foldMap (concat [ indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedCaseItems, tk.bar, tk.space ] <> _) caseItems ]
      , hole: \{ hole, meta, metactx } -> hole
      }
  -- TODO should this have incrementIndentation?
  , argItem: \{ term, term_assoc, meta, metactx } -> concat [ if (unwrap meta).indented then indent metactx else [], assocIf term_assoc term ]
  , typeBind: \{ typeBind, meta, metactx } -> typeBind
  , termBind: \{ termBind, meta, metactx } -> termBind
  , sumItem: \{ termBind, paramItems, meta, metactx } -> concat [ termBind, tk.space, intercalate tk.space paramItems ]
  , caseItem: \{ termId, termBindItems, body, meta, metactx } -> concat [ termId, if null termBindItems then [] else concat [ tk.space, intercalate tk.space termBindItems ], tk.space, tk.mapsto, tk.space, body ]
  , paramItem: \{ type_, meta, metactx } -> type_
  , termBindItem: \{ termBind, meta, metactx } -> termBind
  , dataContextItem: \{ typeBind, metactx } -> concat [ tk.data_, tk.space, typeBind ]
  , varContextItem: \{ termId, type_, metactx } -> concat [ termId, tk.space, tk.colon, tk.space, type_ ]
  }

minimalistSyntaxTheme :: SyntaxTheme
minimalistSyntaxTheme =
  { meta:
      { name: "minimalist"
      }
  , type_:
      { arr: \{ dom, cod, dom_assoc, cod_arr, cod_assoc, meta, metactx } -> concat [ assocIf dom_assoc dom, tk.space, tk.arrow, tk.space, cod ]
      , data_: \{ typeId, meta, metactx } -> typeId
      , hole: \{ holeId, weakening, meta, metactx } -> holeId
      }
  , term:
      { lam: \{ termBind, body, parent_lam, body_lam, body_assoc, meta, metactx } -> concat [ termBind, if body_lam then [] else concat [ tk.space, tk.mapsto ], indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedBody, body ]
      , neu: \{ termId, argItems, meta, metactx } -> concat [ termId, if null argItems then [] else concat [ tk.space, intercalate tk.space argItems ] ]
      , let_: \{ termBind, sign, impl, body, meta, metactx, body_noindent } -> concat [ termBind, tk.space, tk.colon, indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedSign, sign, tk.space, tk.equals, indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedImpl, impl, tk.space, if (unwrap meta).indentedBody then indent metactx else concat [ tk.space, tk.semicolon, tk.space ], body ]
      , buf: \{ sign, impl, body, meta, metactx, body_noindent } -> concat [ tk.hash, indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedImpl, impl, tk.space, tk.colon, indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedSign, sign, if (unwrap meta).indentedBody then indent metactx else concat [ tk.space, tk.semicolon, tk.space ], body ]
      , data_: \{ typeBind, sumItems, body, meta, metactx, body_noindent } -> concat [ typeBind, tk.space, foldMap (concat [ indent (incrementIndentation metactx), tk.bar, tk.space ] <> _) sumItems, tk.space, if (unwrap meta).indentedBody then indent metactx else concat [ tk.in_, tk.space ], body ]
      , match: \{ term, caseItems, meta, metactx } -> concat [ term, tk.space, foldMap (concat [ indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedCaseItems, tk.bar, tk.space ] <> _) caseItems ]
      , hole: \{ hole, meta, metactx } -> hole
      }
  , argItem: \{ term, term_assoc, meta, metactx } -> concat [ if (unwrap meta).indented then indent metactx else [], assocIf term_assoc term ]
  , typeBind: \{ typeBind, meta, metactx } -> typeBind
  , termBind: \{ termBind, meta, metactx } -> termBind
  , sumItem: \{ termBind, paramItems, meta, metactx } -> concat [ termBind, tk.space, intercalate tk.space paramItems ]
  , caseItem: \{ termId, termBindItems, body, meta, metactx } -> concat [ termId, if null termBindItems then [] else concat [ tk.space, intercalate tk.space termBindItems ], tk.space, tk.mapsto, tk.space, body ]
  , paramItem: \{ type_, meta, metactx } -> type_
  , termBindItem: \{ termBind, meta, metactx } -> termBind
  , dataContextItem: \{ typeBind, metactx } -> concat [ typeBind ]
  , varContextItem: \{ termId, type_, metactx } -> concat [ termId, tk.space, tk.colon, tk.space, type_ ]
  }

typescriptSyntaxTheme :: SyntaxTheme
typescriptSyntaxTheme =
  { meta:
      { name: "typescript"
      }
  , type_:
      { arr: \{ dom, cod, dom_assoc, cod_arr, cod_assoc, meta, metactx } -> concat [ assocIf dom_assoc dom, tk.space, tk.arrow, tk.space, cod ]
      , data_: \{ typeId, meta, metactx } -> typeId
      , hole: \{ holeId, weakening, meta, metactx } -> holeId
      }
  , term:
      { lam: \{ termBind, body, parent_lam, body_lam, body_assoc, meta, metactx } -> concat [ tk.lparen, termBind, tk.rparen, tk.space, tk.mapsto, indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedBody, body ]
      , neu: \{ termId, argItems, meta, metactx } -> concat [ termId, if null argItems then [] else intercalate [] argItems ]
      , let_: \{ termBind, sign, impl, body, meta, metactx, body_noindent } -> concat [ tk.let_, tk.space, termBind, tk.space, tk.colon, indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedSign, sign, tk.space, tk.equals, indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedImpl, impl, tk.semicolon, if (unwrap meta).indentedBody then indent metactx else tk.space, body ]
      , buf: \{ sign, impl, body, meta, metactx, body_noindent } -> concat [ impl, tk.space, tk.colon, indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedSign, sign, tk.semicolon, if (unwrap meta).indentedBody then indent metactx else tk.space, body ]
      , data_: \{ typeBind, sumItems, body, meta, metactx, body_noindent } -> concat [ tk.type_, tk.space, typeBind, tk.space, tk.equals, foldMap (concat [ indent (incrementIndentation metactx), tk.bar, tk.space ] <> _) sumItems, tk.space, tk.semicolon, if (unwrap meta).indentedBody then indent metactx else tk.space, body ]
      , match: \{ term, caseItems, meta, metactx } -> concat [ tk.switch, tk.space, term, tk.space, tk.lbrace, foldMap (concat [ indent_otherwise_space (incrementIndentation metactx) (unwrap meta).indentedCaseItems ] <> _) caseItems, tk.space, tk.rbrace ]
      , hole: \{ hole, meta, metactx } -> hole
      }
  , argItem: \{ term, term_assoc, meta, metactx } -> concat [ if (unwrap meta).indented then indent metactx else [], assoc term ]
  , typeBind: \{ typeBind, meta, metactx } -> typeBind
  , termBind: \{ termBind, meta, metactx } -> termBind
  , sumItem: \{ termBind, paramItems, meta, metactx } -> concat [ termBind, if null paramItems then [] else concat [ tk.space, intercalate tk.space paramItems ] ]
  , caseItem: \{ termId, termBindItems, body, meta, metactx } -> concat [ tk.case_, tk.space, termId, if null termBindItems then [] else concat [ tk.space, tk.lparen, intercalate (tk.comma <> tk.space) termBindItems, tk.rparen ], tk.colon, tk.space, body ]
  , paramItem: \{ type_, meta, metactx } -> type_
  , termBindItem: \{ termBind, meta, metactx } -> termBind
  , dataContextItem: \{ typeBind, metactx } -> concat [ tk.type_, tk.space, typeBind ]
  , varContextItem: \{ termId, type_, metactx } -> concat [ termId, tk.space, tk.colon, tk.space, type_ ]
  }

makeStringToken :: String -> String -> Res
makeStringToken str className = makeToken [ DOM.text str ] className

makeToken :: Res -> String -> Res
makeToken res className = [ DOM.span [ Props.className ("token " <> className) ] res ]

tk :: _
tk =
  (fromHomogeneous <<< homogeneous)
    { space: makeStringToken " " "whitespace"
    , lparen: makeStringToken "(" "punctuation paren lparen"
    , rparen: makeStringToken ")" "punctuation paren rparen"
    , lbrace: makeStringToken "{" "punctuation brace lbrace"
    , rbrace: makeStringToken "}" "punctuation brace rbrace"
    , arrow: makeStringToken "->" "punctuation"
    , mapsto: makeStringToken "=>" "punctuation"
    , fun: makeStringToken "fun" "keyword"
    , lambda: makeStringToken "λ" "keyword"
    , let_: makeStringToken "let" "keyword"
    , in_: makeStringToken "in" "keyword"
    , buf: makeStringToken "buf" "keyword"
    , data_: makeStringToken "data" "keyword"
    , type_: makeStringToken "type" "keyword"
    , match: makeStringToken "match" "keyword"
    , with: makeStringToken "with" "keyword"
    , equals: makeStringToken "=" "punctuation"
    , colon: makeStringToken ":" "punctuation"
    , bar: makeStringToken "|" "punctuation"
    , hash: makeStringToken "#" "punctuation"
    , semicolon: makeStringToken ";" "punctuation"
    , comma: makeStringToken "," "punctuation"
    , switch: makeStringToken "switch" "keyword"
    , case_: makeStringToken "case" "keyword"
    , newline: makeToken [ DOM.br' ] "whitespace newline"
    }

assocIf :: Boolean -> Res -> Res
assocIf cond res
  | cond = concat [ tk.lparen, res, tk.rparen ]
  | otherwise = res

assoc :: Res -> Res
assoc res = concat [ tk.lparen, res, tk.rparen ]

indent_otherwise_space :: Metacontext -> Boolean -> Res
indent_otherwise_space metactx cond
  | cond = indent metactx
  | otherwise = tk.space

indent :: Metacontext -> Res
indent metactx =
  let
    i = (unwrap metactx).indentation
  in
    if i == 0 then
      -- an additional newline if at top level (i.e. indentation 0)
      concat [ tk.newline, makeIndent metactx, tk.newline, makeIndent metactx ]
    else
      concat [ tk.newline, makeIndent metactx ]

makeIndent :: Metacontext -> Res
makeIndent metactx =
  makeStringToken
    (String.joinWith "" (replicate (unwrap metactx).indentation indent_str))
    "whitespace indent"
  where
  indent_str = "  "

{-
  { meta:
      { name: "ml"
      }
  , type_:
      { arr: \{ dom, cod, cod_arr, cod_assoc, meta, metactx } -> ?a
      , data_: \{ typeId, meta, metactx } -> ?a
      , hole: \{ holeId, weakening, meta, metactx } -> ?a
      }
  , term:
      { lam: \{ termBind, body, parent_lam, body_lam, body_assoc, meta, metactx } -> ?a
      , neu: \{ termId, argItems, meta, metactx } -> ?a
      , let_: \{ termBind, sign, impl, body, meta, metactx } -> ?a
      , buf: \{ sign, impl, body, meta, metactx } -> ?a
      , data_: \{ typeBind, sumItems, body, meta, metactx } -> ?a
      , match: \{ term, caseItems, meta, metactx } -> ?a
      , hole: \{ hole, meta, metactx } -> ?a
      }
  , argItem: \{ term, term_assoc, meta, metactx } -> ?a
  , typeBind: \{ typeBind, meta, metactx } -> ?a
  , termBind: \{ typeBind, meta, metactx } -> ?a
  , sumItem: \{ termBind, paramItems, meta, metactx } -> ?a
  , caseItem: \{ termId, termBindItems, body, meta, metactx } -> ?a
  , paramItem: \{ type_, meta, metactx } -> ?a
  , termBindItem: \{ termBind, meta, metactx } -> ?a
  , dataContextItem: \{ typeBind, metactx } -> ?a
  , varContextItem: \{ termId, type_, metactx } -> ?a
  }
-}
