module Language.Shape.Stlc.Syntax.Modify where

import Data.Tuple.Nested
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
import Debug as Debug
import Record as R
import Record.Builder as Record
import Type.Proxy (Proxy(..))
import Undefined (undefined)

modifySyntaxAt :: (Syntax -> Maybe Syntax) -> IxDown -> (Syntax -> Maybe Syntax)
modifySyntaxAt f (IxDown steps) syn = go steps syn
  where
  go Nil syn =
    Debug.trace ("nil, syn = " <> show syn) \_ ->
      f syn

  go (Cons step steps) syn =
    Debug.trace ("step = " <> show step <> ", syn = " <> show syn) \_ -> case syn of
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
        | step == ixStepData.sumItems -> do
          -- FIX lists are handled wrong
          -- TODO old: w _sumItems (SyntaxTerm <<< Data) (SyntaxList <<< map SyntaxSumItem) (join <<< map (traverse toSumItem) <<< toSyntaxList) steps data_
          synList <- go steps $ rollSyntaxList SyntaxSumItem data_.sumItems
          sumItems <- unrollSyntaxList toSumItem synList
          pure $ SyntaxTerm $ Data data_ { sumItems = sumItems }
        | step == ixStepData.body -> w _body (SyntaxTerm <<< Data) SyntaxTerm toTerm steps data_
      SyntaxTerm (Match match)
        | step == ixStepMatch.term -> w _term (SyntaxTerm <<< Match) SyntaxTerm toTerm steps match
        | step == ixStepMatch.caseItems -> do
          -- TODO old: w _caseItems (SyntaxTerm <<< Match) (SyntaxList <<< map SyntaxCaseItem) (join <<< map (traverse toCaseItem) <<< toSyntaxList) steps match
          -- syntaxCaseItems <- traverse (\caseItem -> go steps (SyntaxCaseItem caseItem)) match.caseItems
          -- caseItems <- traverse toCaseItem syntaxCaseItems
          synList <- go steps $ rollSyntaxList SyntaxCaseItem match.caseItems
          caseItems <- unrollSyntaxList toCaseItem synList
          pure $ SyntaxTerm $ Match match { caseItems = caseItems }
      SyntaxArgItem argItem
        | step == ixStepArgItem.term -> w _term SyntaxArgItem SyntaxTerm toTerm steps argItem
      SyntaxSumItem sumItem
        | step == ixStepSumItem.termBind -> w _termBind SyntaxSumItem SyntaxTermBind toTermBind steps sumItem
        | step == ixStepSumItem.paramItems -> do
          -- FIX lists are handled wrong
          -- TODO old: w _paramItems SyntaxSumItem (SyntaxList <<< map SyntaxParamItem) (join <<< map (traverse toParamItem) <<< toSyntaxList) steps sumItem
          synList <- go steps $ rollSyntaxList SyntaxParamItem sumItem.paramItems
          paramItems <- unrollSyntaxList toParamItem synList
          pure $ SyntaxSumItem $ sumItem { paramItems = paramItems }
      SyntaxCaseItem caseItem
        | step == ixStepCaseItem.termBindItems -> do
          -- FIX this is wrong somehow.. im handling lists wrong 
          -- TODO old: -- w _termBindItems SyntaxCaseItem (SyntaxList <<< map SyntaxTermBindItem) (join <<< map (traverse toTermBindItem) <<< toSyntaxList) steps caseItem
          synList <- go steps $ rollSyntaxList SyntaxTermBindItem caseItem.termBindItems
          Debug.traceM $ "SyntaxCaseItem.synList = " <> show synList
          termBindItems <- unrollSyntaxList toTermBindItem synList
          Debug.traceM $ "SyntaxCaseItem.termBindItems = " <> show termBindItems
          pure $ SyntaxCaseItem $ caseItem { termBindItems = termBindItems }
        | step == ixStepCaseItem.body -> w _body SyntaxCaseItem SyntaxTerm toTerm steps caseItem
      SyntaxParamItem paramItem
        | step == ixStepParamItem.type_ -> w _type_ SyntaxParamItem SyntaxType toType steps paramItem
      SyntaxTermBindItem termBindItem
        | step == ixStepTermBindItem.termBind -> w _termBind SyntaxTermBindItem SyntaxTermBind toTermBind steps termBindItem
      SyntaxList (Cons synHead synsTail)
        | step == ixStepList.head -> (\synHead' -> SyntaxList (Cons synHead' synsTail)) <$> go steps synHead
      SyntaxList (Cons synHead synsTail)
        | step == ixStepList.tail -> (\synsTail' -> SyntaxList (Cons synHead synsTail')) <$> (toSyntaxList =<< go steps (SyntaxList synsTail))
      _ -> Nothing

  w ::
    forall l a r' r.
    IsSymbol l =>
    Cons l a r' r =>
    Show a =>
    Proxy l ->
    (Record r -> Syntax) -> -- wrapR
    (a -> Syntax) -> -- wrap 
    (Syntax -> Maybe a) -> -- unwrap
    List IxStep -> -- steps 
    Record r -> -- r 
    Maybe Syntax
  -- w label wrapR wrap unwrap steps r = (wrapR <<< (\a' -> R.set label a' r)) <$> (unwrap =<< go steps (wrap (R.get label r)))
  w label wrapR wrap unwrap steps r = do
    -- Debug.traceM "===[ w ]====================================================="
    -- Debug.traceM $ "label = " <> show label
    -- Debug.traceM $ "r = "
    -- Debug.traceM $ r
    -- Debug.traceM $ "steps = " <> show steps
    let
      a0 = R.get label r :: a
    -- Debug.traceM $ "a0 = " <> show a0
    let
      syn0 = wrap a0 :: Syntax
    -- Debug.traceM $ "syn0 = " <> show syn0
    syn1 <- go steps syn0
    -- Debug.traceM $ "syn1 = " <> show syn1
    a1 <- unwrap syn1
    -- Debug.traceM $ "a1 = " <> show a1
    let
      r' = R.set label a1 r
    -- Debug.traceM "r' = "
    -- Debug.traceM r'
    let
      syn2 = wrapR r'
    -- Debug.traceM $ "syn2 = " <> show syn2
    pure syn2
