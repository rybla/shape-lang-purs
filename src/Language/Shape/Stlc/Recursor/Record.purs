module Language.Shape.Stlc.Recursor.Record where

import Prelude
import Prim.Row
import Record
import Record as Record
import Data.Symbol (class IsSymbol)
import Record as Record
import Type.Proxy (Proxy(..))
import Undefined (undefined)

modifyHetero ::
  forall label r1 r1' r2 r3 r3'.
  IsSymbol label =>
  Lacks label r2 =>
  Cons label (Record r1) r2 r3 =>
  Cons label (Record r1') r2 r3' =>
  Proxy label ->
  (Record r1 -> Record r1') ->
  Record r3 ->
  Record r3'
modifyHetero label f args =
  insert label (f (get label args))
    $ delete (Proxy :: Proxy label) args

modifyHeteroM ::
  forall m label r1 r1' r2 r3 r3'.
  Monad m =>
  IsSymbol label =>
  Lacks label r2 =>
  Cons label (Record r1) r2 r3 =>
  Cons label (Record r1') r2 r3' =>
  Proxy label ->
  (Record r1 -> m (Record r1')) ->
  Record r3 ->
  m (Record r3')
modifyHeteroM label f args = do
  item <- f (get label args)
  pure $ insert label item $ delete (Proxy :: Proxy label) args
