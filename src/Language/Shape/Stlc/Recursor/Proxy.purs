module Language.Shape.Stlc.Recursor.Proxy where

import Prelude
import Type.Proxy (Proxy(..))

_here = Proxy :: Proxy "here"

_type_ = Proxy :: Proxy "type_"

_visit = Proxy :: Proxy "visit"

_term = Proxy :: Proxy "term"
_goal = Proxy :: Proxy "goal"