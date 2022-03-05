module Language.Shape.Stlc.Recursion.Context where

import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Data.List (List)
import Data.Map as Map
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Recursion.Base as Rec
import Language.Shape.Stlc.Typing
import Undefined (undefined)
import Unsafe.Error as Unsafe

-- Recursion principles which handle context & type
recModule ::
  forall a.
  { module_ :: List Definition -> ModuleMetadata -> Context -> a
  } ->
  Module -> Context -> a
recModule rec =
  Rec.recModule
    { module_:
        \defs meta gamma ->
          rec.module_ defs meta (addDefinitionsToContext defs gamma)
    }

recBlock ::
  forall a.
  { block :: List Definition -> Term -> BlockMetadata -> Context -> Type -> a
  } ->
  Block -> Context -> Type -> a
recBlock rec =
  Rec.recBlock
    { block:
        \defs a meta gamma alpha ->
          rec.block defs a meta (addDefinitionsToContext defs gamma) alpha
    }

recDefinition ::
  forall a.
  { term :: TermID -> Type -> Term -> TermDefinitionMetadata -> Context -> a
  , data :: TypeID -> (List Constructor) -> DataDefinitionMetadata -> Context -> a
  } ->
  Definition -> Context -> a
recDefinition = Rec.recDefinition

recConstructor ::
  forall a.
  { constructor :: TermID -> Type -> ConstructorMetadata -> Context -> a
  } ->
  Constructor -> Context -> a
recConstructor = Rec.recConstructor

recTerm ::
  forall a.
  { lambda :: TermID -> Block -> LambdaTermMetadata -> Context -> Type -> a
  , application :: TermID -> List Term -> ApplicationTermMetadata -> Context -> Type -> a
  , hole :: HoleTermMetadata -> Context -> Type -> a
  , match :: TypeID -> Term -> List Term -> MatchTermMetadata -> Context -> Type -> a
  } ->
  Term -> Context -> Type -> a
recTerm rec =
  Rec.recTerm
    { lambda:
        \id block meta gamma alpha -> case alpha of
          ArrowType beta delta _ -> rec.lambda id block meta (Map.insert id beta gamma) delta
          _ -> Unsafe.error "impossible"
    , application:
        \id args meta gamma alpha ->
          rec.application id args meta gamma alpha
    , hole:
        \meta gamma alpha ->
          rec.hole meta gamma alpha
    , match:
        \dataID a cases meta gamma alpha ->
          rec.match dataID a cases meta gamma alpha
    }

recType ::
  forall a.
  { arrow :: Type -> Type -> ArrowTypeMetadata -> Context -> a
  , data :: TypeID -> DataTypeMetadata -> Context -> a
  , hole :: HoleID -> TypeWeakening -> HoleTypeMetadata -> Context -> a
  } ->
  Type -> Context -> a
recType = Rec.recType
