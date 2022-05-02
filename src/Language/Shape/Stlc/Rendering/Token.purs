module Language.Shape.Stlc.Rendering.Keyword where

import Data.Homogeneous.Record
import Prelude
import Data.Array (singleton)
import React.DOM (text)
import React.DOM.Dynamic (span)
import React.DOM.Props (className)

token :: _
token =
  fromHomogeneous $ pure (span [ className "token" ] <<< singleton)
    <*> homogeneous
        { arrowType1: text "->"
        , lam1: text "fun"
        , lam2: text "=>"
        , data1: text "data"
        , data2: text "="
        , data3: text "in"
        }
