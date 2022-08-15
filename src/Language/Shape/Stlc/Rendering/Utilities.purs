module Language.Shape.Stlc.Rendering.Utilities where

import Data.Tuple
import Data.Tuple.Nested
import Language.Shape.Stlc.ChAtIndex
import Language.Shape.Stlc.Changes
import Language.Shape.Stlc.Context
import Language.Shape.Stlc.CopyPasteBackend
import Language.Shape.Stlc.Rendering.Token
import Language.Shape.Stlc.Rendering.Types
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Types
import Prelude
import Prim hiding (Type)
import Control.Monad.State (State, gets)
import Control.Monad.State as State
import Data.Array (concat)
import Data.Array as Array
import Data.Default (default)
import Data.Foldable (foldM)
import Data.List.Unsafe (List(..), reverse)
import Data.List.Unsafe as List
import Data.Map.Unsafe as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.OrderedSet (OrderedSet)
import Data.OrderedSet as OrderedSet
import Data.Set as Set
import Data.String (joinWith)
import Data.Traversable (sequence)
import Debug as Debug
import Effect (Effect)
import Effect.Console as Console
import Language.Shape.Stlc.Hole (subTerm, subType)
import Language.Shape.Stlc.Index (IxDown(..), nilIxDown, nilIxUp, toIxDown)
import Language.Shape.Stlc.Metacontext (Metacontext(..), incrementIndentation)
import Language.Shape.Stlc.Metadata (Name(..))
import Language.Shape.Stlc.Recursor.Index (Visit, nilVisit, nonVisit)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Union)
import React (ReactElement, getState, modifyState)
import React.DOM as DOM
import React.DOM.Props as Props
import React.SyntheticEvent (shiftKey, stopPropagation)
import Record as Record
import Type.Proxy (Proxy(..))

renderConcatList :: List (M (Array ReactElement)) -> M (Array ReactElement)
renderConcatList = (List.foldl append [] <$> _) <<< sequence

renderConcatArray :: Array (M (Array ReactElement)) -> M (Array ReactElement)
renderConcatArray = (Array.foldl append [] <$> _) <<< sequence

maybeArray :: forall a b. Maybe a -> (a -> b) -> Array b
maybeArray ma f = maybe [] (Array.singleton <<< f) ma

-- enParen :: M (Array ReactElement) -> M (Array ReactElement)
-- enParen m = do
--   synthm <- gets _.syntaxtheme
--   renderConcatArray [ pure (token.lparen synthm), m, pure (token.rparen synthm) ]
-- enParenIf :: M (Array ReactElement) -> Boolean -> M (Array ReactElement)
-- enParenIf m true = enParen m
-- enParenIf m false = m
requiresParenType :: Type -> Boolean
requiresParenType = case _ of
  ArrowType _ -> true
  _ -> false

requiresParenTerm :: Term -> Boolean
requiresParenTerm = case _ of
  Lam _ -> true
  Neu neu -> List.length neu.argItems /= 0
  Let _ -> true
  Buf _ -> true
  Data _ -> true
  Match _ -> true
  _ -> false
