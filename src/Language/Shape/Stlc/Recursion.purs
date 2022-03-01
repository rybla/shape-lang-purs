module Language.Shape.Stlc.Recursion where

import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Data.List (List)
import Data.Map as Map
import Data.UUID (UUID)
import Language.Shape.Stlc.Typing (Context, addDefinitionsToContext, addParametersToContext, typeOfNeutralTerm)
import Unsafe as Unsafe

indModule :: forall a. (Context -> List Definition -> List Buffer -> Unit -> a) -> Module -> a
indModule rec (Module defs bufs _) = rec (addDefinitionsToContext Map.empty defs) defs bufs unit

indBlock :: forall a. (Context -> Type -> List Definition -> List Buffer -> Unit -> a) -> Context -> Type -> Block -> a
indBlock rec gamma alpha (Block defs bufs a _) = rec (addDefinitionsToContext gamma defs) alpha defs bufs unit

indDefinition ::
  forall a.
  { termDefinition :: Context -> TermUniqueBinding -> Type -> Term -> Unit -> a
  , dataDefinition :: Context -> TypeUniqueBinding -> List Constructor -> Unit -> a
  } ->
  Context -> Definition -> a
indDefinition recs gamma (TermDefinition x@(TermUniqueBinding id _) alpha a _) = recs.termDefinition gamma' x alpha a unit
  where
  gamma' = Map.insert id alpha gamma

indDefinition recs gamma (DataDefinition x constrs _) = recs.dataDefinition gamma x constrs unit

indConstructor :: forall a. (Context -> TermUniqueBinding -> List Parameter -> Unit -> a) -> Context -> Constructor -> a
indConstructor rec gamma (Constructor x prms _) = rec gamma x prms unit

indBuffer :: forall a. (Context -> BaseType -> NeutralTerm -> a) -> Context -> NeutralTerm -> a
indBuffer rec gamma a = rec gamma (typeOfNeutralTerm gamma a) a

indType ::
  forall a.
  { arrow :: Context -> List Parameter -> BaseType -> Unit -> a
  , base :: Context -> BaseType -> Unit -> a
  } ->
  Context -> Type -> a
indType recs gamma (ArrowType prms beta _) = recs.arrow gamma prms beta unit

indType recs gamma (BaseType alpha) = recs.base gamma alpha unit

indBaseType ::
  forall a.
  { data :: Context -> TypeId -> Unit -> a
  , hole :: Context -> HoleId -> TypeWeakening -> Unit -> a
  } ->
  Context -> BaseType -> a
indBaseType recs gamma (DataType id _) = recs.data gamma id unit

indBaseType recs gamma (HoleType id wkn _) = recs.hole gamma id wkn unit

indTerm ::
  forall a.
  { lambda :: Context -> BaseType -> List TermBinding -> Block -> Unit -> a
  , neutral :: Context -> BaseType -> NeutralTerm -> a
  } ->
  Context -> Type -> Term -> a
indTerm recs gamma (ArrowType prms beta _Arrow) (LambdaTerm xs block _Lambda) = recs.lambda gamma' beta xs block unit
  where
  gamma' = addParametersToContext gamma prms

indTerm recs gamma (BaseType alpha) (NeutralTerm a) = recs.neutral gamma alpha a

indTerm _ _ _ _ = Unsafe.error "impossible"

indNeutralTerm ::
  forall a.
  { application :: Context -> BaseType -> TermReference -> List Term -> Unit -> a
  , match :: Context -> BaseType -> TypeId -> NeutralTerm -> List Case -> Unit -> a
  , hole :: Context -> BaseType -> Unit -> a
  } ->
  Context -> BaseType -> NeutralTerm -> a
indNeutralTerm recs gamma alpha (ApplicationTerm x args _) = recs.application gamma alpha x args unit

indNeutralTerm recs gamma alpha (MatchTerm x a cases _) = recs.match gamma alpha x a cases unit

indNeutralTerm recs gamma alpha (HoleTerm _) = recs.hole gamma alpha unit

indCase :: forall a. (Context -> List TermBinding -> Block -> Unit -> a) -> Context -> Case -> a
indCase rec gamma (Case xs block _) = rec gamma xs block unit

indParameter :: forall a. (Context -> TermLabel -> Type -> Unit -> a) -> Context -> Parameter -> a
indParameter rec gamma (Parameter label alpha _) = rec gamma label alpha unit

indTypeUniqueBinding :: forall a. (Context -> TypeId -> Unit -> a) -> Context -> TypeUniqueBinding -> a
indTypeUniqueBinding rec gamma (TypeUniqueBinding id _) = rec gamma id unit

indTypeId :: forall a. (Context -> UUID -> a) -> Context -> TypeId -> a
indTypeId rec gamma (TypeId uuid) = rec gamma uuid

indHoleId :: forall a. (Context -> UUID -> a) -> Context -> HoleId -> a
indHoleId rec gamma (HoleId uuid) = rec gamma uuid

indTypeName ::
  forall a.
  { name :: Context -> String -> a
  , ignore :: Context -> a
  } ->
  Context -> TypeName -> a
indTypeName recs gamma (TypeName str) = recs.name gamma str

indTypeName recs gamma IgnoreTypeName = recs.ignore gamma

indTermUniqueBinding :: forall a. (Context -> TermId -> Unit -> a) -> Context -> TermUniqueBinding -> a
indTermUniqueBinding rec gamma (TermUniqueBinding id _) = rec gamma id unit

indTermLabel :: forall a. (Context -> TermName -> Unit -> a) -> Context -> TermLabel -> a
indTermLabel rec gamma (TermLabel name _) = rec gamma name unit

indTermBinding :: forall a. (Context -> TermId -> Unit -> a) -> Context -> TermBinding -> a
indTermBinding rec gamma (TermBinding id _) = rec gamma id unit

indTermReference :: forall a. (Context -> TermId -> Unit -> a) -> Context -> TermReference -> a
indTermReference rec gamma (TermReference id _) = rec gamma id unit

indTermId :: forall a. (Context -> UUID -> a) -> Context -> TermId -> a
indTermId rec gamma (TermId uuid) = rec gamma uuid

indTermName ::
  forall a.
  { name :: Context -> String -> a
  , ignore :: Context -> a
  } ->
  Context -> TermName -> a
indTermName recs gamma (TermName str) = recs.name gamma str

indTermName recs gamma IgnoreTermName = recs.ignore gamma
