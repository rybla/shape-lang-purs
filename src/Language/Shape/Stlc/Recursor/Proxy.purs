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

_argItem = Proxy :: Proxy "argItem"

_caseItem = Proxy :: Proxy "caseItem"

_sumItem = Proxy :: Proxy "sumItem"

_paramItem = Proxy :: Proxy "paramItem"

_termBindItem = Proxy :: Proxy "termBindItem"

_typeId = Proxy :: Proxy "typeId"
_typeBind = Proxy :: Proxy "typeBind"
_termBind = Proxy :: Proxy "termBind"