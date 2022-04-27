module Language.Shape.Stlc.ChAtIndex where

import Prelude

import Language.Shape.Stlc.Recursor.Context as Rec
import Undefined (undefined)

type Syntax = Unit

chAtTerm :: Rec.ProtoRec Rec.ArgsTerm () (Unit -> Unit -> Boolean -> Unit)
chAtTerm _ _ _ true = unit
chAtTerm args _ _ false = Rec.recTerm {
    lam : \args _ _ sbjoss -> undefined
    , neu : undefined
    , let_ : undefined
    , buf : undefined
    , data_ : undefined
    , match : undefined
    , hole : undefined
} args unit unit false