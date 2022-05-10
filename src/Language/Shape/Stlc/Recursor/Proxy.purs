module Language.Shape.Stlc.Recursor.Proxy where

import Prelude
import Type.Proxy (Proxy(..))

_here = Proxy :: Proxy "here"

_type_ = Proxy :: Proxy "type_"

_visit = Proxy :: Proxy "visit"

_term = Proxy :: Proxy "term"

_goal = Proxy :: Proxy "goal"

_ctx = Proxy :: Proxy "ctx"

_actions = Proxy :: Proxy "actions"

_alpha = Proxy :: Proxy "alpha"

_argItems = Proxy :: Proxy "argItems"

_caseItems = Proxy :: Proxy "caseItems"

_sumItems = Proxy :: Proxy "sumItems"

_paramItems = Proxy :: Proxy "paramItems"

_termBindItems = Proxy :: Proxy "termBindItems"
