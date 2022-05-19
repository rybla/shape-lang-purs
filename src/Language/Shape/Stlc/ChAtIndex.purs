module Language.Shape.Stlc.ChAtIndex where

import Data.Tuple.Nested
import Prelude
import Prim hiding (Type)

import Ansi.Codes (EscapeCode(..))
import Control.Monad.State (runState)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Show.Generic (genericShow)
import Language.Shape.Stlc.Changes (ConstructorChange, TypeChange(..), applyTC, chTerm, chTerm', emptyChanges, varChange)
import Language.Shape.Stlc.Hole (HoleEq, emptyHoleEq, emptyHoleSub)
import Language.Shape.Stlc.Index (IxDown(..), IxStep(..), IxStepLabel(..), ixStepBuf, ixStepData, ixStepLet)
import Language.Shape.Stlc.Recursor.Context as Rec
import Language.Shape.Stlc.Syntax (ArrowType, Term(..), Type(..), Buf)
import Undefined (undefined)
import Unsafe (error)

type Change = {ix :: IxDown, toReplace :: ToReplace}

data ToReplace = ReplaceTerm Term TypeChange | ReplaceType Type TypeChange

derive instance genericToReplace :: Generic ToReplace _
instance showToReplace :: Show ToReplace where show x = genericShow x

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
    _ -> error "no8"
  , neu : \args tRep -> case _ of
    (IxDown (IxStep IxStepNeu 1 : rest)) -> error "unimplemented" -- argument of neutral
    _ -> error "no9"
  , let_ : \args tRep -> case _ of
    (IxDown (IxStep IxStepLet 1 : rest)) -> do -- type of let
      ty' /\ IxDown idx' /\ tc /\ holeEq0 <- chAtType args.sign tRep (IxDown rest)
      let impl' /\ holeEq1 = runState (chTerm' args.impl emptyChanges tc) holeEq0
      let body' /\ holeEq2 = runState (chTerm' args.body (varChange emptyChanges args.termBind.termBind.termId tc) NoChange) holeEq1
      pure $ Let args.let_ {sign= ty', impl= impl', body= body'} /\ IxDown (ixStepLet.sign : idx') /\ NoChange /\ holeEq2
    (IxDown ((IxStep IxStepLet 2) : rest)) -> do -- definition of let
      impl' /\ IxDown idx' /\ tc /\ holeEq1 <- chAtTerm args.impl tRep (IxDown rest)
      let sign' = applyTC tc args.sign.type_
      let body' /\ holeEq2 = runState (chTerm' args.body (varChange emptyChanges args.termBind.termBind.termId tc) NoChange) holeEq1
      pure $ Let args.let_ {sign = sign', impl = impl', body = body'} /\ IxDown (ixStepLet.impl : idx') /\ NoChange /\ holeEq2
    (IxDown (IxStep IxStepLet 3 : rest)) -> do -- body of let
      body' /\ IxDown idx' /\ tc /\ holeEq <- chAtTerm args.body tRep (IxDown rest)
      pure $ Let args.let_ {body = body'} /\ IxDown (ixStepLet.body : idx') /\ tc /\ holeEq
    _ -> error "no10"
  , buf : \args tRep -> case _ of
    (IxDown (IxStep IxStepBuf 0 : rest)) -> do -- type of thing in buffer
      ty' /\ IxDown idx' /\ tc /\ holeEq0 <- chAtType args.sign tRep (IxDown rest)
      let impl' /\ holeEq1 = runState (chTerm' args.impl emptyChanges tc) holeEq0
      pure $ Buf args.buf {sign= ty', impl= impl'} /\ IxDown (ixStepBuf.sign : idx') /\ NoChange /\ holeEq1
    (IxDown (IxStep IxStepBuf 1 : rest)) -> do -- thing in buffer
      impl' /\ IxDown idx' /\ tc /\ holeEq <- chAtTerm args.impl tRep (IxDown rest)
      let sign' = applyTC tc args.sign.type_
      pure $ Buf args.buf {sign = sign', impl = impl'} /\ IxDown (ixStepBuf.impl : idx') /\ NoChange /\ holeEq
    (IxDown (IxStep IxStepBuf 2 : rest)) -> do -- body of buffer (rest of program)
      body' /\ IxDown idx' /\ tc /\ holeEq <- chAtTerm args.body tRep (IxDown rest)
      pure $ Buf args.buf {body = body'} /\ IxDown (ixStepBuf.body : idx') /\ tc /\ holeEq
    _ -> error "no1"
  , data_ : \args tRep -> case _ of
    (IxDown (IxStep IxStepData 1 : rest)) -> error "unimplemented"
    (IxDown (IxStep IxStepData 2 : rest)) -> do -- body of data type definition
      body' /\ IxDown idx' /\ tc /\ holeEq <- chAtTerm args.body tRep (IxDown rest)
      pure $ Data args.data_ {body = body'} /\ IxDown (ixStepData.body : idx') /\ tc /\ holeEq
    _ -> error "no2"
  , match : \args tRep -> case _ of
    (IxDown (IxStep IxStepMatch 1 : rest)) -> error "unimplemented"
    (IxDown (IxStep IxStepMatch 2 : rest)) -> error "unimplemented"
    _ -> error "no3"
  , hole : \args tRep -> case _ of _ -> error ("no4 bla " <> (show idx))
} args tRep idx

chAtType :: Record (Rec.ArgsType ()) -> ToReplace -> IxDown -> Maybe (Type /\ IxDown /\ TypeChange /\ HoleEq)
chAtType args (ReplaceType ty tc) (IxDown Nil) = Just (ty /\ IxDown Nil /\ tc /\ emptyHoleEq)
chAtType args tRep idx = Rec.recType {
  arrowType : \args tRep -> case _ of
    (IxDown (IxStep IxStepArrowType 0 : rest)) -> do -- left of arrow
      (dom' /\ (IxDown idx') /\ tcIn /\ holeEq) <- chAtType args.dom tRep (IxDown rest)
      pure $ ArrowType (args.arrowType {dom=dom'}) /\ IxDown ((IxStep IxStepArrowType 0) : idx') /\ ArrowCh tcIn NoChange /\ holeEq
    (IxDown (IxStep IxStepArrowType 1 : rest)) -> undefined
    _ -> error "no5"
  , dataType : \args tRep -> case _ of
    _ -> error "no6"
  , holeType : \args tRep -> case _ of
    _ -> error "no7"
} args tRep idx