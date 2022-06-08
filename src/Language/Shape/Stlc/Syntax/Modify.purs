module Language.Shape.Stlc.Syntax.Modify where

import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Recursor.Proxy
import Language.Shape.Stlc.Syntax
import Prelude
import Prim.Row
import Data.List (List(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Symbol (class IsSymbol)
import Data.Traversable (sequence, traverse)
import Record as R
import Record.Builder as Record
import Type.Proxy (Proxy(..))
import Undefined (undefined)

modifySyntaxAt :: (Syntax -> Maybe Syntax) -> IxDown -> (Syntax -> Maybe Syntax)
modifySyntaxAt f (IxDown steps) syn = go steps syn
  where
  go Nil syn = f syn

  go (Cons step steps) syn = case syn of
    SyntaxType (ArrowType arrow)
      | step == ixStepArrowType.dom -> w _dom (SyntaxType <<< ArrowType) SyntaxType toType steps arrow
      | step == ixStepArrowType.cod -> w _cod (SyntaxType <<< ArrowType) SyntaxType toType steps arrow
    SyntaxTerm (Lam lam)
      | step == ixStepLam.termBind -> w _termBind (SyntaxTerm <<< Lam) SyntaxTermBind toTermBind steps lam
      | step == ixStepLam.body -> w _body (SyntaxTerm <<< Lam) SyntaxTerm toTerm steps lam
    SyntaxTerm (Neu neu)
      | step == ixStepNeu.termId -> w _termId (SyntaxTerm <<< Neu) SyntaxTermId toTermId steps neu
      | step == ixStepNeu.argItems -> w _argItems (SyntaxTerm <<< Neu) (SyntaxList <<< map SyntaxArgItem) (join <<< map (traverse toArgItem) <<< toSyntaxList) steps neu
    SyntaxTerm (Let let_)
      | step == ixStepLet.termBind -> w _termBind (SyntaxTerm <<< Let) SyntaxTermBind toTermBind steps let_
      | step == ixStepLet.sign -> w _sign (SyntaxTerm <<< Let) SyntaxType toType steps let_
      | step == ixStepLet.impl -> w _impl (SyntaxTerm <<< Let) SyntaxTerm toTerm steps let_
      | step == ixStepLet.body -> w _body (SyntaxTerm <<< Let) SyntaxTerm toTerm steps let_
    SyntaxTerm (Buf buf)
      | step == ixStepBuf.sign -> w _sign (SyntaxTerm <<< Buf) SyntaxType toType steps buf
      | step == ixStepBuf.impl -> w _impl (SyntaxTerm <<< Buf) SyntaxTerm toTerm steps buf
      | step == ixStepBuf.body -> w _body (SyntaxTerm <<< Buf) SyntaxTerm toTerm steps buf
    SyntaxTerm (Data data_)
      | step == ixStepData.typeBind -> w _typeBind (SyntaxTerm <<< Data) SyntaxTypeBind toTypeBind steps data_
      | step == ixStepData.sumItems -> w _sumItems (SyntaxTerm <<< Data) (SyntaxList <<< map SyntaxSumItem) (join <<< map (traverse toSumItem) <<< toSyntaxList) steps data_
      | step == ixStepData.body -> w _body (SyntaxTerm <<< Data) SyntaxTerm toTerm steps data_
    SyntaxTerm (Match match)
      | step == ixStepMatch.term -> w _term (SyntaxTerm <<< Match) SyntaxTerm toTerm steps match
      | step == ixStepMatch.caseItems -> w _caseItems (SyntaxTerm <<< Match) (SyntaxList <<< map SyntaxCaseItem) (join <<< map (traverse toCaseItem) <<< toSyntaxList) steps match
    SyntaxArgItem argItem
      | step == ixStepArgItem.term -> w _term SyntaxArgItem SyntaxTerm toTerm steps argItem
    SyntaxSumItem sumItem
      | step == ixStepSumItem.termBind -> w _termBind SyntaxSumItem SyntaxTermBind toTermBind steps sumItem
      | step == ixStepSumItem.paramItems -> w _paramItems SyntaxSumItem (SyntaxList <<< map SyntaxParamItem) (join <<< map (traverse toParamItem) <<< toSyntaxList) steps sumItem
    SyntaxCaseItem caseItem
      | step == ixStepCaseItem.body -> w _body SyntaxCaseItem SyntaxTerm toTerm steps caseItem
      | step == ixStepCaseItem.termBindItems -> w _termBindItems SyntaxCaseItem (SyntaxList <<< map SyntaxTermBindItem) (join <<< map (traverse toTermBindItem) <<< toSyntaxList) steps caseItem
    SyntaxParamItem paramItem
      | step == ixStepParamItem.type_ -> w _type_ SyntaxParamItem SyntaxType toType steps paramItem
    SyntaxTermBindItem termBindItem
      | step == ixStepTermBindItem.termBind -> w _termBind SyntaxTermBindItem SyntaxTermBind toTermBind steps termBindItem
    SyntaxList (Cons syn _)
      | step == ixStepList.head -> go steps syn
    SyntaxList (Cons _ syns)
      | step == ixStepList.tail -> go steps (SyntaxList syns)
    _ -> Nothing

  w ::
    forall l a r' r.
    IsSymbol l =>
    Cons l a r' r =>
    Proxy l ->
    (Record r -> Syntax) -> -- wrapR
    (a -> Syntax) -> -- wrap 
    (Syntax -> Maybe a) -> -- unwrap
    List IxStep -> -- steeps 
    Record r -> -- r 
    Maybe Syntax
  w label wrapR wrap unwrap steps r = (wrapR <<< (\a' -> R.set label a' r)) <$> (unwrap =<< go steps (wrap (R.get label r)))
