module Language.Shape.Stlc.Recursion.Wrap where

import Data.Foldable
import Data.Tuple.Nested
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Typing
import Prelude
import Prim hiding (Type)
import Control.Monad.State (State, runState)
import Data.List.Unsafe (List)
import Data.List.Unsafe as List
import Data.Map (Map)
import Data.Map as Map
import Language.Shape.Stlc.Changes (TypeChange(..), Changes, chTerm)
import Language.Shape.Stlc.Holes (HoleSub, subModule)
import Language.Shape.Stlc.Recursion.MetaContext (MetaContext)
import Language.Shape.Stlc.Recursion.MetaContext as Rec
import Undefined (undefined)
import Unsafe as Unsafe

type Wrap a
  = a -> TypeChange -> Module

type IndexWrap a
  = Int -> Wrap a

-- Recursion principles for handling IndexWrapng
recModule ::
  forall a.
  { module_ :: List Definition -> ModuleMetadata -> Context -> MetaContext -> Wrap (List Definition) -> a
  } ->
  Module -> Context -> MetaContext -> Wrap Module -> a
recModule rec =
  Rec.recModule
    { module_:
        \defs meta gamma metaGamma wrap_mod ->
          rec.module_ defs meta gamma metaGamma
            (\defs' -> wrap_mod $ Module defs' meta)
    }

recBlock ::
  forall a.
  { block :: List Definition -> Term -> BlockMetadata -> Context -> Type -> MetaContext -> Wrap (List Definition) -> Wrap Term -> a
  } ->
  Block -> Context -> Type -> MetaContext -> Wrap Block -> a
recBlock rec =
  Rec.recBlock
    { block:
        \defs a meta gamma alpha metaGamma wrap_block ->
          rec.block defs a meta gamma alpha metaGamma
            (wrap_block <<< \defs' -> Block defs' a meta)
            (wrap_block <<< \a' -> Block defs a' meta)
    }

recDefinitions ::
  forall a.
  { definitions :: List Definition -> Context -> MetaContext -> Wrap (List Definition) -> a } ->
  List Definition -> Context -> MetaContext -> Wrap (List Definition) -> a
recDefinitions rec =
  Rec.recDefinitions
    -- TODO: put the displaced terms resulting from typechanges applied to terms into this list of definitions
    { definitions:
        \defs gamma metaGamma wrap_defs ->
          rec.definitions defs gamma metaGamma
            ( \defs' tc ->
                let
                  defs'' /\ sub =
                    foldl
                      ( \(defs'' /\ sub) -> case _ of
                          DataDefinition typeBnd constr meta -> undefined -- TODO: not sure what to do here...
                          TermDefinition termBnd alpha a meta ->
                            let
                              changes = undefined :: Changes {-JACOB-}

                              st = undefined :: List Definition /\ Map HoleID Type {-JACOB-}

                              (a' /\ displaceds /\ sub') = runState (chTerm gamma alpha changes tc a) st
                            in
                              (defs'' <> displaceds <> List.singleton (TermDefinition termBnd alpha a' meta)) /\ Map.union sub sub'
                      )
                      (List.Nil /\ Map.empty)
                      defs'
                in
                  subModule sub $ wrap_defs defs'' tc
            )
    }

recConstructor ::
  forall a.
  { constructor :: TermBinding -> List Parameter -> ConstructorMetadata -> Context -> TypeBinding -> MetaContext -> IndexWrap Parameter -> a
  } ->
  Constructor -> Context -> TypeBinding -> MetaContext -> Wrap Constructor -> a
recConstructor rec =
  Rec.recConstructor
    { constructor:
        \termBnd prms meta gamma typeBnd metaGamma wrap_constr ->
          rec.constructor termBnd prms meta gamma typeBnd metaGamma
            (\i -> wrap_constr <<< \prm' -> Constructor termBnd (List.updateAt' i prm' prms) meta)
    }

recType ::
  forall a.
  { arrow :: Parameter -> Type -> ArrowTypeMetadata -> Context -> MetaContext -> Wrap Parameter -> Wrap Type -> a
  , data :: TypeId -> DataTypeMetadata -> Context -> MetaContext -> a
  , hole :: HoleID -> TypeWeakening -> HoleTypeMetadata -> Context -> MetaContext -> a
  , proxyHole :: HoleID -> Context -> MetaContext -> a
  } ->
  Type -> Context -> MetaContext -> Wrap Type -> a
recType rec =
  Rec.recType
    { arrow:
        \prm beta meta gamma metaGamma wrap_arr ->
          rec.arrow prm beta meta gamma metaGamma
            (wrap_arr <<< \prm' -> ArrowType prm' beta meta)
            (wrap_arr <<< \beta' -> ArrowType prm beta' meta)
    , data: \typeId meta gamma metaGamma _ -> rec.data typeId meta gamma metaGamma
    , hole: \holeID wkn meta gamma metaGamma _ -> rec.hole holeID wkn meta gamma metaGamma
    , proxyHole: \holeID gamma metaGamma _ -> rec.proxyHole holeID gamma metaGamma
    }

recTerm ::
  forall a.
  { lambda :: TermId -> Block -> LambdaTermMetadata -> Context -> Parameter -> Type -> MetaContext -> Wrap Block -> a
  , neutral :: TermId -> Args -> NeutralTermMetadata -> Context -> Type -> MetaContext -> Wrap Args -> a
  , hole :: HoleTermMetadata -> Context -> Type -> MetaContext -> a
  , match :: TypeId -> Term -> List Case -> MatchTermMetadata -> Context -> Type -> MetaContext -> List TermId -> Wrap Term -> IndexWrap Case -> a
  } ->
  Term -> Context -> Type -> MetaContext -> Wrap Term -> a
recTerm rec =
  Rec.recTerm
    { lambda:
        \termId block meta gamma prm beta metaGamma wrap_term ->
          rec.lambda termId block meta gamma prm beta metaGamma
            (\block' tc -> wrap_term (LambdaTerm termId block' meta) (ArrowCh NoChange tc))
    , neutral:
        \termId args meta gamma alpha metaGamma wrap_term ->
          rec.neutral termId args meta gamma alpha metaGamma
            (wrap_term <<< \args' -> NeutralTerm termId args' meta)
    , hole:
        \meta gamma alpha metaGamma _ ->
          rec.hole meta gamma alpha metaGamma
    , match:
        \typeId a cases meta gamma alpha metaGamma constrIDs wrap_term ->
          rec.match typeId a cases meta gamma alpha metaGamma constrIDs
            (wrap_term <<< \a' -> MatchTerm typeId a' cases meta)
            (\i -> wrap_term <<< \case' -> MatchTerm typeId a (List.updateAt' i case' cases) meta)
    }

recArgs ::
  forall a.
  { none :: a
  , cons :: Term -> Args -> ArgConsMetaData -> Context -> Parameter -> Type -> MetaContext -> Wrap Term -> Wrap Args -> a
  } ->
  Args -> Context -> Type -> MetaContext -> Wrap Args -> a
recArgs rec =
  Rec.recArgs
    { none: \_ -> rec.none
    , cons:
        \a args meta gamma prm beta metaGamma wrap_args ->
          rec.cons a args meta gamma prm beta metaGamma
            (wrap_args <<< \a' -> ConsArgs a' args meta)
            (wrap_args <<< \args' -> ConsArgs a args' meta)
    }

recCase ::
  forall a.
  { case_ :: List TermId -> Term -> CaseMetadata -> Context -> Type -> TypeId -> TermId -> MetaContext -> Wrap Term -> a } ->
  Case -> Context -> Type -> TypeId -> TermId -> MetaContext -> Wrap Case -> a
recCase rec =
  Rec.recCase
    { case_:
        \termBnds a meta gamma alpha typeId termId metaGamma wrap_case ->
          rec.case_ termBnds a meta gamma alpha typeId termId metaGamma
            (wrap_case <<< \a' -> Case termBnds a' meta)
    }

recParameter ::
  forall a.
  { parameter :: Type -> ParameterMetadata -> Context -> MetaContext -> Wrap Type -> a } ->
  Parameter -> Context -> MetaContext -> Wrap Parameter -> a
recParameter rec =
  Rec.recParameter
    { parameter:
        \alpha meta gamma metaGamma wrap_prm ->
          rec.parameter alpha meta gamma metaGamma
            (wrap_prm <<< \alpha' -> Parameter alpha' meta)
    }
