module Language.Shape.Stlc.Recursion.Context where

import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Data.List (List)
import Data.Map as Map
import Language.Shape.Stlc.Recursion.Base as Rec
import Language.Shape.Stlc.Typing (Context, addDefinitionToContext, addDefinitionsToContext)
import Undefined (undefined)
import Unsafe.Error as Unsafe

-- Recursion principles which handle context & type
recModule ::
  forall a.
  { module_ :: List Definition -> Context -> a
  } ->
  Module -> Context -> a
recModule rec =
  Rec.recModule
    { module_: \defs meta gamma -> rec.module_ defs (addDefinitionsToContext defs gamma) }

recBlock ::
  forall a.
  { block :: List Definition -> Term -> Context -> Type -> a
  } ->
  Block -> Context -> Type -> a
recBlock rec =
  Rec.recBlock
    { block: \defs a meta gamma alpha -> rec.block defs a (addDefinitionsToContext defs gamma) alpha }

recDefinitions ::
  forall a.
  { definitions :: Int -> Definition -> Context -> a
  } ->
  List Definition -> Context -> a
recDefinitions rec =
  Rec.recDefinitions
    { definitions: \i def gamma -> rec.definitions i def (addDefinitionToContext def gamma) }

recDefinition ::
  forall a.
  { term :: TermID -> Type -> Term -> Context -> a
  , data :: TypeID -> (List Constructor) -> Context -> a
  } ->
  Definition -> Context -> a
recDefinition rec =
  Rec.recDefinition
    { term: \id alpha a meta gamma -> rec.term id alpha a gamma
    , data: \id constrs meta gamma -> rec.data id constrs gamma
    }

recConstructor ::
  forall a.
  { constructor :: TermID -> Type -> Context -> a
  } ->
  Constructor -> Context -> a
recConstructor rec =
  Rec.recConstructor
    { constructor: \id alpha meta gamma -> rec.constructor id alpha gamma }

recTerm ::
  forall a.
  { lambda :: TermID -> Block -> Context -> Type -> a
  , application :: TermID -> List Term -> Context -> Type -> a
  , hole :: Context -> Type -> a
  , match :: TypeID -> Term -> List Term -> Context -> Type -> a
  } ->
  Term -> Context -> Type -> a
recTerm rec =
  Rec.recTerm
    { lambda:
        \id block meta gamma alpha -> case alpha of
          ArrowType beta delta _ -> rec.lambda id block (Map.insert id beta gamma) delta
          _ -> Unsafe.error "impossible"
    , application: \id args meta gamma alpha -> rec.application id args gamma alpha
    , hole: \meta gamma alpha -> rec.hole gamma alpha
    , match: \dataID a cases meta gamma alpha -> rec.match dataID a cases gamma alpha
    }

recType ::
  forall a.
  { arrow :: Type -> Type -> Context -> a
  , data :: TypeID -> Context -> a
  , hole :: HoleID -> TypeWeakening -> Context -> a
  } ->
  Type -> Context -> a
recType rec =
  Rec.recType
    { arrow: \alpha beta meta gamma -> rec.arrow alpha beta gamma
    , data: \id meta gamma -> rec.data id gamma
    , hole: \id wkn meta gamma -> rec.hole id wkn gamma
    }
