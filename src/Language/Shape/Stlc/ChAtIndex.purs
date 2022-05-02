module Language.Shape.Stlc.ChAtIndex where

import Prelude

import Ansi.Codes (EscapeCode(..))
import Data.List (List(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Language.Shape.Stlc.Index (IxDown(..), IxStep(..), IxStepLabel(..), ixStepBuf)
import Language.Shape.Stlc.Recursor.Index (visitIxStep)
import Language.Shape.Stlc.Recursor.Index as Rec
import Undefined (undefined)
import Unsafe (error)

chAtTerm :: Rec.ProtoRec Rec.ArgsTerm () (Unit -> Unit -> Maybe Unit)
chAtTerm {ix: {visit: {csr: Just (IxDown Nil)}}} _ _ = undefined -- pure unit
chAtTerm args _ _ = Rec.recTerm {
    lam : \args _ _ -> undefined
    , neu : undefined
    , let_ : undefined
    , buf : \args _ _ ->
        if isJust args.ix.term.csr then 
            undefined
        else if isJust args.ix.body.csr then
            undefined
        else
        error "no"
                            
    , data_ : undefined
    , match : undefined
    , hole : undefined
} args unit unit