module Language.Shape.Stlc.ChAtIndex where

import Data.Tuple.Nested
import Prelude
import Prim hiding (Type)

import Ansi.Codes (EscapeCode(..))
import Control.Monad.State (runState)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), isJust, maybe)
import Language.Shape.Stlc.Changes (ConstructorChange, TypeChange(..), chTerm, chTerm', emptyChanges, varChange)
import Language.Shape.Stlc.Hole (HoleEq, emptyHoleEq, emptyHoleSub)
import Language.Shape.Stlc.Index (IxDown(..), IxStep(..), IxStepLabel(..), ixStepBuf, ixStepLet)
import Language.Shape.Stlc.Recursor.Context as Rec
import Language.Shape.Stlc.Syntax (Term(..), Type(..), ArrowType)
import Undefined (undefined)
import Unsafe (error)

data ToReplace = ReplaceTerm Term TypeChange | ReplaceType Type TypeChange

-- old thing:
-- data Change
--     = Changetypechange Typechange
--     | ChangeConstructorChange ConstructorChange
--     | Changenone

{-
why is chatterm in a maybe monad? because if you try to apply a type change in an argument position, then
it should fail?
should that be taken care of somewhere else, like i assume that input are in good positions?
or i just fail in the neutral case?
No, that doesn't work. Its only if chAtTerm tries to pass something UP to an argument position that is not
NoChange. It is OK if something in an argument changes but it doesn't change the overall type.
-}

chAtTerm :: Record (Rec.ArgsTerm ()) -> ToReplace -> IxDown -> Maybe (Term /\ IxDown /\ TypeChange /\ HoleEq)
chAtTerm args (ReplaceTerm newTerm tc) (IxDown Nil)
  = Just (newTerm /\ IxDown Nil /\ tc /\ emptyHoleEq)
chAtTerm args tRep idx = Rec.recTerm {
  lam : \args tRep -> case _ of
    (IxDown (IxStep IxStepLam 1 : rest)) -> do -- body of lambda
      body' /\ IxDown idx' /\ tc /\ holeEq <- chAtTerm args.body tRep (IxDown rest)
      pure $ Lam args.lam {body=body'} /\ IxDown (IxStep IxStepLam 1 : idx') /\ tc /\ holeEq
    _ -> error "no"
  , neu : \args tRep -> case _ of
    (IxDown (IxStep IxStepNeu 1 : rest)) -> error "unimplemented" -- argument of neutral
    _ -> error "no"
  , let_ : \args tRep -> case _ of
    (IxDown (IxStep IxStepLet 1 : rest)) -> do -- type of let
      ty' /\ IxDown idx' /\ tc /\ holeEq <- chAtType args.sign tRep (IxDown rest)
      let impl' /\ holeEq1 = runState (chTerm' args.impl emptyChanges tc) emptyHoleEq
      let body' /\ holeEq2 = runState (chTerm' args.body (varChange emptyChanges args.termBind.termBind.termId tc) NoChange) holeEq1
      pure $ Let args.let_ {sign= ty', impl= impl', body= body'} /\ IxDown (ixStepLet.sign : idx') /\ NoChange /\ holeEq2
    (IxDown (IxStep IxStepLet 2 : rest)) -> do -- definition of let
      undefined
    (IxDown (IxStep IxStepLet 3 : rest)) -> do -- body of let
      undefined
    _ -> error "no"
  , buf : \args tRep -> case _ of
    (IxDown (IxStep IxStepBuf 0 : rest)) -> undefined
    (IxDown (IxStep IxStepBuf 1 : rest)) -> undefined
    _ -> error "no"
  , data_ : \args tRep -> case _ of
    (IxDown (IxStep IxStepData 1 : rest)) -> undefined
    (IxDown (IxStep IxStepData 2 : rest)) -> undefined
    (IxDown (IxStep IxStepData 3 : rest)) -> undefined
    _ -> error "no"
  , match : \args tRep -> case _ of
    (IxDown (IxStep IxStepMatch 1 : rest)) -> error "unimplemented"
    (IxDown (IxStep IxStepMatch 2 : rest)) -> error "unimplemented"
    _ -> error "no"
  , hole : \args tRep -> case _ of _ -> error "no"
} args tRep idx

chAtType :: Record (Rec.ArgsType ()) -> ToReplace -> IxDown -> Maybe (Type /\ IxDown /\ TypeChange /\ HoleEq)
chAtType args (ReplaceType ty tc) (IxDown Nil) = Just (ty /\ IxDown Nil /\ tc /\ emptyHoleEq)
chAtType args tRep idx = Rec.recType {
  arrowType : \args tRep -> case _ of
    (IxDown (IxStep IxStepArrowType 0 : rest)) -> do -- left of arrow
      (dom' /\ (IxDown idx') /\ tcIn /\ holeEq) <- chAtType args.dom tRep (IxDown rest)
      pure $ ArrowType (args.arrowType {dom=dom'}) /\ IxDown ((IxStep IxStepArrowType 0) : idx') /\ ArrowCh tcIn NoChange /\ holeEq
    (IxDown (IxStep IxStepArrowType 1 : rest)) -> undefined
    _ -> error "no"
  , dataType : \args tRep -> case _ of
    _ -> error "no"
  , holeType : \args tRep -> case _ of
    _ -> error "no"
} args tRep idx

-- It would really be nice to have an argItems recursor which actually is a recursor and not just map...
-- chAtArgs :: Rec.Args

-- chAtTerm :: Record (Rec.ArgsTerm ()) -> (Unit -> Unit -> Maybe Unit)
-- chAtTerm args arg1 arg2
--   | isSelected args.visit = undefined
--   | otherwise =
--     Rec.recTerm
--       { lam: undefined
--       , neu: undefined
--       , let_: undefined
--       , buf:
--           \args arg1 arg2 ->
--             if isSelectionAncestor args.visit then
--               undefined
--             else
--               undefined
--       , data_: undefined
--       , match: undefined
--       , hole: undefined
--       }
--       args
--       arg1
--       arg2

-- chAtTerm {ix: {visit: {csr: Just (IxDown Nil)}}} _ _ = undefined -- pure unit
-- chAtTerm args _ _ = Rec.recTerm {
--     lam : \args _ _ -> undefined
--     , neu : undefined
--     , let_ : undefined
--     , buf : \args _ _ ->
--         if isJust args.ix.term.csr then 
--             undefined
--         else if isJust args.ix.body.csr then
--             undefined
--         else
--         error "no"
--     , data_ : undefined
--     , match : undefined
--     , hole : undefined
-- } args unit unit
