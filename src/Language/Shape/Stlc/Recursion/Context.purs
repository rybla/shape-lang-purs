module Language.Shape.Stlc.Recursion.Context where

import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Data.List (List)
import Language.Shape.Stlc.Typing (Context)
import Undefined (undefined)

-- Recursion principles which handle context & type
recModule ::
  forall a.
  { module_ :: List Definition -> Context -> a
  } ->
  Module -> Context -> a
recModule rec module_ gamma = undefined

recBlock ::
  forall a.
  { block :: List Definition -> Term -> Context -> Type -> a
  } ->
  Block -> Context -> Type -> a
recBlock rec block gamma alpha = undefined

recDefinitions ::
  forall a.
  { definitions :: Int -> Definition -> Context -> a
  } ->
  List Definition -> Context -> a
recDefinitions rec defs gamma = undefined

recDefinition ::
  forall a.
  { term :: TermID -> Type -> Term -> Context -> a
  , data :: TypeID -> (List Constructor) -> Context -> a
  } ->
  Definition -> Context -> a
recDefinition def gamma = undefined

recConstructor ::
  forall a.
  { constructor :: TermID -> Type -> Context -> a
  } ->
  Constructor -> Context -> a
recConstructor constr gamma = undefined

recTerm ::
  forall a.
  { lambda :: TermID -> Block -> Context -> Type -> a
  , application :: TermID -> List Term -> Context -> Type -> a
  , hole :: Context -> Type -> a
  , match :: TypeID -> Term -> List Term -> Context -> Type -> a
  } ->
  Term -> Context -> Type -> a
recTerm a gamma alpha = undefined

recType ::
  forall a.
  { arrow :: Type -> Type -> Context -> a
  , data :: TypeID -> Context -> a
  , hole :: HoleID -> TypeWeakening -> Context -> a
  } ->
  Type -> Context -> a
recType alpha gamma = undefined
