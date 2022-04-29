module Language.Shape.Stlc.Rendering.Keyword where

import Data.Homogeneous.Record
import Prelude
import Data.Array (singleton)
import React.DOM (text)
import React.DOM.Dynamic (span)
import React.DOM.Props (className)

keyword :: _
keyword =
  fromHomogeneous $ pure (span [ className "keyword" ] <<< singleton)
    <*> homogeneous
        { arrowType1: text "->"
        , lam1: text "fun"
        , lam2: text "=>"
        }
