module Language.Shape.Stlc.Recursion where

import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Data.List (List, mapWithIndex, updateAt)
import Data.Map as Map
import Data.UUID (UUID)
import Language.Shape.Stlc.Model (Changes, InputChange(..), TypeChange(..))
import Language.Shape.Stlc.Typing (Context, addDefinitionsToContext, addParametersToContext, typeOfNeutralTerm)
import Undefined (undefined)
import Unsafe as Unsafe

type Wrap x
  = x -> HoleSub -> TypeChange -> Module

recModule ::
  forall a.
  (Context -> {-Wrap Def...-} List Definition -> List (Wrap Buffer) -> List Buffer -> Unit -> a) ->
  Module -> a
recModule rec (Module defs bufs _) =
  rec (addDefinitionsToContext Map.empty defs) defs
    (mapWithIndex (\i _ -> \buf sub chgs -> Module defs (Unsafe.fromJust $ updateAt i buf bufs) undefined) bufs)
    bufs
    unit

recBlock ::
  forall a.
  (Context -> Type -> List Definition -> List (Wrap Buffer) -> List Buffer -> NeutralTerm -> Unit -> a) ->
  Context -> Type -> Wrap Block -> Block -> a
recBlock rec gamma alpha wrap (Block defs bufs a _) =
  rec (addDefinitionsToContext gamma defs) alpha defs
    -- (mapWithIndex (\i _ -> \buf sub chgs
    -- -> wrap $ Block defs (Unsafe.fromJust $ updateAt i buf bufs) a undefined) bufs)
    ( mapWithIndex
        ( \i _ -> \buf sub chgs ->
            wrap undefined sub NoChange
        )
        bufs
    )
    bufs
    a
    unit

recDefinition ::
  forall a.
  { termDefinition :: Context -> TermUniqueBrecing -> Type -> Term -> Unit -> a
  , dataDefinition :: Context -> TypeUniqueBrecing -> List Constructor -> Unit -> a
  } ->
  Context -> Definition -> a
recDefinition recs gamma (TermDefinition x@(TermUniqueBrecing id _) alpha a _) = recs.termDefinition gamma' x alpha a unit
  where
  gamma' = Map.insert id alpha gamma

recDefinition recs gamma (DataDefinition x constrs _) = recs.dataDefinition gamma x constrs unit

recConstructor :: forall a. (Context -> TermUniqueBrecing -> List Parameter -> Unit -> a) -> Context -> Constructor -> a
recConstructor rec gamma (Constructor x prms _) = rec gamma x prms unit

recBuffer :: forall a. (Context -> BaseType -> NeutralTerm -> a) -> Context -> NeutralTerm -> a
recBuffer rec gamma a = rec gamma (typeOfNeutralTerm gamma a) a

recType ::
  forall a.
  { arrow :: Context -> List Parameter -> BaseType -> Unit -> a
  , base :: Context -> BaseType -> Unit -> a
  } ->
  Context -> Type -> a
recType recs gamma (ArrowType prms beta _) = recs.arrow gamma prms beta unit

recType recs gamma (BaseType alpha) = recs.base gamma alpha unit

recBaseType ::
  forall a.
  { data :: Context -> TypeId -> Unit -> a
  , hole :: Context -> HoleId -> TypeWeakening -> Unit -> a
  } ->
  Context -> BaseType -> a
recBaseType recs gamma (DataType id _) = recs.data gamma id unit

recBaseType recs gamma (HoleType id wkn _) = recs.hole gamma id wkn unit

recTerm ::
  forall a.
  { lambda :: Context -> BaseType -> List TermBrecing -> Block -> Unit -> a
  , neutral :: Context -> BaseType -> NeutralTerm -> a
  } ->
  Context -> Type -> Term -> a
recTerm recs gamma (ArrowType prms beta _Arrow) (LambdaTerm xs block _Lambda) = recs.lambda gamma' beta xs block unit
  where
  gamma' = addParametersToContext gamma prms

recTerm recs gamma (BaseType alpha) (NeutralTerm a) = recs.neutral gamma alpha a

recTerm _ _ _ _ = Unsafe.error "impossible"

recNeutralTerm ::
  forall a.
  { application :: Context -> BaseType -> TermReference -> List Term -> Unit -> a
  , match :: Context -> BaseType -> TypeId -> NeutralTerm -> List Case -> Unit -> a
  , hole :: Context -> BaseType -> Unit -> a
  } ->
  Context -> BaseType -> NeutralTerm -> a
recNeutralTerm recs gamma alpha (ApplicationTerm x args _) = recs.application gamma alpha x args unit

recNeutralTerm recs gamma alpha (MatchTerm x a cases _) = recs.match gamma alpha x a cases unit

recNeutralTerm recs gamma alpha (HoleTerm _) = recs.hole gamma alpha unit

recCase :: forall a. (Context -> List TermBrecing -> Block -> Unit -> a) -> Context -> Case -> a
recCase rec gamma (Case xs block _) = rec gamma xs block unit

recParameter :: forall a. (Context -> TermLabel -> Type -> Unit -> a) -> Context -> Parameter -> a
recParameter rec gamma (Parameter label alpha _) = rec gamma label alpha unit

recTypeUniqueBrecing :: forall a. (Context -> TypeId -> Unit -> a) -> Context -> TypeUniqueBrecing -> a
recTypeUniqueBrecing rec gamma (TypeUniqueBrecing id _) = rec gamma id unit

recTypeId :: forall a. (Context -> UUID -> a) -> Context -> TypeId -> a
recTypeId rec gamma (TypeId uuid) = rec gamma uuid

recHoleId :: forall a. (Context -> UUID -> a) -> Context -> HoleId -> a
recHoleId rec gamma (HoleId uuid) = rec gamma uuid

recTypeName ::
  forall a.
  { name :: Context -> String -> a
  , ignore :: Context -> a
  } ->
  Context -> TypeName -> a
recTypeName recs gamma (TypeName str) = recs.name gamma str

recTypeName recs gamma IgnoreTypeName = recs.ignore gamma

recTermUniqueBrecing :: forall a. (Context -> TermId -> Unit -> a) -> Context -> TermUniqueBrecing -> a
recTermUniqueBrecing rec gamma (TermUniqueBrecing id _) = rec gamma id unit

recTermLabel :: forall a. (Context -> TermName -> Unit -> a) -> Context -> TermLabel -> a
recTermLabel rec gamma (TermLabel name _) = rec gamma name unit

recTermBrecing :: forall a. (Context -> TermId -> Unit -> a) -> Context -> TermBrecing -> a
recTermBrecing rec gamma (TermBrecing id _) = rec gamma id unit

recTermReference :: forall a. (Context -> TermId -> Unit -> a) -> Context -> TermReference -> a
recTermReference rec gamma (TermReference id _) = rec gamma id unit

recTermId :: forall a. (Context -> UUID -> a) -> Context -> TermId -> a
recTermId rec gamma (TermId uuid) = rec gamma uuid

recTermName ::
  forall a.
  { name :: Context -> String -> a
  , ignore :: Context -> a
  } ->
  Context -> TermName -> a
recTermName recs gamma (TermName str) = recs.name gamma str

recTermName recs gamma IgnoreTermName = recs.ignore gamma
