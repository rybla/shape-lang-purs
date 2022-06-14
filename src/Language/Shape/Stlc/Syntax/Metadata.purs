module Language.Shape.Stlc.Syntax.Metadata where

import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Prelude
import Data.Array as Array
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (over, unwrap, wrap)
import Language.Shape.Stlc.Syntax.Modify
import Undefined (undefined)

indentSyntaxAt :: IxDown -> Syntax -> Maybe Syntax
indentSyntaxAt =
  modifySyntaxAt case _ of
    SyntaxTerm term -> pure <<< SyntaxTerm $ indentTerm term
    _ -> Nothing

-- TODO: now just have to implement this for each Syntax constructor... but there really isn't better way, believe me I've tried oh god I've tried. Maybe something with fundef typeclass magic, but Idk if that counts as better
indentTerm :: Term -> Term
indentTerm term = case term of
  Lam lam -> Lam lam { meta = over LamMetadata (\o -> o { indentedBody = not o.indentedBody }) lam.meta }
  Let let_ -> Let let_ { meta = over LetMetadata (\o -> o { indentedSign = not o.indentedSign, indentedImpl = not o.indentedImpl, indentedBody = not o.indentedBody }) let_.meta }
  Buf buf -> Buf buf { meta = over BufMetadata (\o -> o { indentedSign = not o.indentedSign, indentedImpl = not o.indentedImpl, indentedBody = not o.indentedBody }) buf.meta }
  Data data_ -> Data data_ { meta = over DataMetadata (\o -> o { indentedSumItems = not o.indentedSumItems, indentedBody = not o.indentedBody }) data_.meta }
  Match data_ -> Match data_ { meta = over MatchMetadata (\o -> o { indentedCaseItems = not o.indentedCaseItems }) data_.meta }
  -- Lam lam -> Lam lam { meta = over LamMetadata (\o -> o { indentedBody = true }) lam.meta }
  -- Let let_ -> Let let_ { meta = over LetMetadata (\o -> o { indentedSign = true, indentedImpl = true, indentedBody = not o.indentedBody }) let_.meta }
  -- Buf buf -> Buf buf { meta = over BufMetadata (\o -> o { indentedSign = true, indentedImpl = true, indentedBody = true }) buf.meta }
  -- Data data_ -> Data data_ { meta = over DataMetadata (\o -> o { indentedSumItems = true, indentedBody = true }) data_.meta }
  -- Match data_ -> Match data_ { meta = over MatchMetadata (\o -> o { indentedCaseItems = true }) data_.meta }
  _ -> term

stepUpToNearestIndentableParentIxUp :: IxUp -> IxUp
-- stepUpToNearestIndentableParentIxUp ix = case List.uncons (unwrap ix) of
--   Nothing -> ix
--   Just { head: step, tail: steps } ->
--     if isIndentableIxStep step then
--       ix
--     else
--       stepUpToNearestIndentableParentIxUp (wrap steps)
stepUpToNearestIndentableParentIxUp ix = ix -- TODO: make this work properly

isIndentableIxStep :: IxStep -> Boolean
isIndentableIxStep (IxStep lbl _) = lbl `Array.elem` indentableIxStepLabels
  where
  indentableIxStepLabels =
    [ IxStepArrowType, IxStepLet, IxStepBuf, IxStepData, IxStepMatch, IxStepArgItem, IxStepSumItem, IxStepCaseItem, IxStepParamItem, IxStepTermBindItem, IxStepList
    ]
