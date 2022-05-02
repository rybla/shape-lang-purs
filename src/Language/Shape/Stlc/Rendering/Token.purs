module Language.Shape.Stlc.Rendering.Keyword where

import Data.Homogeneous.Record
import Prelude
import Data.Array (singleton)
import React.DOM (text)
import React.DOM.Dynamic (span)
import React.DOM.Props (className)

token :: _
token =
  { arrowType1: span [ className "token" ] [ text "->" ]
  , lam1: span [ className "token" ] [ text "fun" ]
  , lam2: span [ className "token" ] [ text "=>" ]
  , data1: span [ className "token" ] [ text "data" ]
  , data2: span [ className "token" ] [ text "=" ]
  , data3: span [ className "token" ] [ text "in" ]
  , neu1: span [ className "token" ] [ text " " ]
  , let1: span [ className "token" ] [ text "let" ]
  , let2: span [ className "token" ] [ text ":" ]
  , let3: span [ className "token" ] [ text "=" ]
  , let4: span [ className "token" ] [ text "in" ]
  , buf1: span [ className "token" ] [ text "buf" ]
  , buf2: span [ className "token" ] [ text ":" ]
  , buf3: span [ className "token" ] [ text "in" ]
  , match1: span [ className "token" ] [ text "match" ]
  , match2: span [ className "token" ] [ text "with" ]
  }
