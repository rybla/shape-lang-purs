module Language.Shape.Stlc.Recursion.Context where

import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Typing
import Prelude
import Prim hiding (Type)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Language.Shape.Stlc.Recursion.Base as Rec
import Undefined (undefined)
import Unsafe as Unsafe

-- Recursion principles for handling context & type
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

recDefinitions ::
  forall a.
  { definitions :: List Definition -> Context -> a } ->
  List Definition -> Context -> a
recDefinitions rec = rec.definitions

recConstructor ::
  forall a.
  { constructor :: TermBinding -> List Parameter -> ConstructorMetadata -> Context -> TypeBinding -> a
  } ->
  Constructor -> Context -> TypeBinding -> a
recConstructor = Rec.recConstructor

recType ::
  forall a.
  { arrow :: Parameter -> Type -> ArrowTypeMetadata -> Context -> a
  , data :: TypeID -> DataTypeMetadata -> Context -> a
  , hole :: HoleID -> TypeWeakening -> HoleTypeMetadata -> Context -> a
  , proxyHole :: HoleID -> Context -> a
  } ->
  Type -> Context -> a
recType = Rec.recType

recTerm ::
  forall a.
  { lambda :: TermID -> Block -> LambdaTermMetadata -> Context -> Type -> a
  , neutral :: TermID -> Args -> NeutralTermMetadata -> Context -> Type -> a
  , hole :: HoleTermMetadata -> Context -> Type -> a
  , match :: TypeID -> Term -> List Case -> MatchTermMetadata -> Context -> Type -> a
  } ->
  Term -> Context -> Type -> a
recTerm rec =
  Rec.recTerm
    { lambda:
        \x block meta gamma alpha -> case alpha of
          ArrowType (Parameter beta _) delta _ -> rec.lambda x block meta (Map.insert x beta gamma) delta
          _ -> Unsafe.error "impossible"
    , neutral:
        \termID args meta gamma alpha ->
          rec.neutral termID args meta gamma alpha
    , hole:
        \meta gamma alpha ->
          rec.hole meta gamma alpha
    , match:
        \dataID a cases meta gamma alpha ->
          rec.match dataID a cases meta gamma alpha
    }

recArgs ::
  forall a.
  { none :: a
  , cons :: Term -> Args -> ArgConsMetaData -> Context -> Type -> List Type -> a
  } ->
  Args -> Context -> List Type -> a
recArgs rec =
  Rec.recArgs
    { none: \_ _ -> rec.none
    , cons:
        \a args meta gamma -> case _ of
          List.Cons alpha alphas -> rec.cons a args meta gamma alpha alphas
          List.Nil -> Unsafe.error "impossible"
    }

recCase ::
  forall a.
  { case_ :: List TermID -> Term -> CaseMetadata -> Context -> Type -> TypeID -> TermID -> a } ->
  Case -> Context -> Type -> TypeID -> TermID -> a
recCase = Rec.recCase

recParameter ::
  forall a.
  { parameter :: Type -> ParameterMetadata -> Context -> a } ->
  Parameter -> Context -> a
recParameter = Rec.recParameter
