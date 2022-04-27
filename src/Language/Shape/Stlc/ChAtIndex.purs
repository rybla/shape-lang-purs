module Language.Shape.Stlc.ChAtIndex where

import Prelude

import Language.Shape.Stlc.Recursor.Context as Rec

type Syntax = Unit

chAtTerm :: Rec.ProtoRec Rec.ArgsTerm () (Unit -> Unit -> Boolean -> Unit)
chAtTerm _ _ _ true = unit
chAtTerm args _ _ false = Rec.recTerm {
    lam : \args _ _ sbjoss -> ?h
    , neu : ?h
    , let_ : ?h
    , buf : ?h
    , data_ : ?h
    , match : ?h
    , hole : ?h
} args unit unit false