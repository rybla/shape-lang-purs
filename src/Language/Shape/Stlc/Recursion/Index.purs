module Language.Shape.Stlc.Recursion.Index where

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
import Language.Shape.Stlc.Changes as Ch
import Language.Shape.Stlc.Holes (HoleSub, subModule)
import Language.Shape.Stlc.Recursion.MetaContext (MetaContext)
import Language.Shape.Stlc.Recursion.MetaContext as Rec
import Undefined (undefined)
import Unsafe as Unsafe
import Language.Shape.Stlc.Index

-- Recursion principles for handling indexing
recModule ::
  forall a.
  { module_ :: List Definition -> ModuleMetadata -> Context -> MetaContext -> Index -> (Int -> Index) -> a
  } ->
  Module -> Context -> MetaContext -> Index -> a
recModule rec =
  Rec.recModule
    { module_:
        \defs meta gamma metaGamma ix_mod ->
          rec.module_ defs meta gamma metaGamma
            ix_mod
            (\i -> ix_mod `pushIndex` Module_Definition i)
    }

{-
recBlock ::
  forall a.
  { block :: List Definition -> Term -> BlockMetadata -> Context -> Type -> MetaContext -> Index -> (Int -> Index) -> Index -> a
  } ->
  Block -> Context -> Type -> MetaContext -> Index -> a
recBlock rec =
  Rec.recBlock
    { block:
        \defs a meta gamma alpha metaGamma ix_block ->
          rec.block defs a meta gamma alpha metaGamma
            ix_block
            (ix_block <<< \defs' -> Block defs' a meta)
            (ix_block <<< \a' -> Block defs a' meta)
    }

recDefinitions ::
  forall a.
  { definitions :: List Definition -> Context -> MetaContext -> (Int -> Index) -> a } ->
  List Definition -> Context -> MetaContext -> (Int -> Index) -> a
recDefinitions rec =
  Rec.recDefinitions
    -- TODO: put the displaced terms resulting from typechanges applied to terms into this list of definitions
    { definitions:
        \defs gamma metaGamma ix_defs ->
          let
            ix_defs' = \defs' tc ->
              -- let
              --   defs'' /\ sub =
              --     foldl
              --       ( \(defs'' /\ sub) -> case _ of
              --           DataDefinition typeBnd constr meta -> undefined -- TODO: not sure what to do here...
              --           TermDefinition termBnd alpha a meta ->
              --             let
              --               changes = undefined :: Ch.Changes
              --               st = undefined :: List Definition /\ Map HoleId Type
              --               (a' /\ displaceds /\ sub') = runState (Ch.chTerm gamma alpha changes tc a) st
              --             in
              --               (defs'' <> displaceds <> List.singleton (TermDefinition termBnd alpha a' meta)) /\ Map.union sub sub'
              --       )
              --       (List.Nil /\ Map.empty)
              --       defs'
              -- in
              --   subModule sub $ ix_defs defs'' tc
              ix_defs defs' tc

            ix_def_at i = \def' -> ix_defs' (List.updateAt' i def' defs)
          in
            rec.definitions defs gamma metaGamma ix_defs' ix_def_at
    }

recConstructor ::
  forall a.
  { constructor :: TermBinding -> List Parameter -> ConstructorMetadata -> Context -> TypeBinding -> MetaContext -> Index -> (Int -> Index) -> a
  } ->
  Constructor -> Context -> TypeBinding -> MetaContext -> Index -> a
recConstructor rec =
  Rec.recConstructor
    { constructor:
        \termBnd prms meta gamma typeBnd metaGamma ix_constr ->
          rec.constructor termBnd prms meta gamma typeBnd metaGamma
            ix_constr
            (\i -> ix_constr <<< \prm' -> Constructor termBnd (List.updateAt' i prm' prms) meta)
    }

recType ::
  forall a.
  { arrow :: Parameter -> Type -> ArrowTypeMetadata -> Context -> MetaContext -> Index -> Index -> Index -> a
  , data :: TypeId -> DataTypeMetadata -> Context -> MetaContext -> Index -> a
  , hole :: HoleId -> TypeWeakening -> HoleTypeMetadata -> Context -> MetaContext -> Index -> a
  , proxyHole :: HoleId -> Context -> MetaContext -> Index -> a
  } ->
  Type -> Context -> MetaContext -> Index -> a
recType rec =
  Rec.recType
    { arrow:
        \prm beta meta gamma metaGamma ix_type ->
          rec.arrow prm beta meta gamma metaGamma
            ix_type
            (ix_type <<< \prm' -> ArrowType prm' beta meta)
            (ix_type <<< \beta' -> ArrowType prm beta' meta)
            
    , data: \typeId meta gamma metaGamma ix_type -> rec.data typeId meta gamma metaGamma ix_type
    , hole: \holeID wkn meta gamma metaGamma ix_type -> rec.hole holeID wkn meta gamma metaGamma ix_type
    , proxyHole: \holeID gamma metaGamma ix_type -> rec.proxyHole holeID gamma metaGamma ix_type
    }

recTerm ::
  forall a.
  { lambda :: TermId -> Block -> LambdaTermMetadata -> Context -> Parameter -> Type -> MetaContext -> Index -> Index -> a
  , neutral :: TermId -> Args -> NeutralTermMetadata -> Context -> Type -> MetaContext -> Index -> Index -> a
  , hole :: HoleTermMetadata -> Context -> Type -> MetaContext -> Index -> a
  , match :: TypeId -> Term -> List Case -> MatchTermMetadata -> Context -> Type -> MetaContext -> List TermId -> Index -> Index -> (Int -> Index) -> a
  } ->
  Term -> Context -> Type -> MetaContext -> Index -> a
recTerm rec =
  Rec.recTerm
    { lambda:
        \termId block meta gamma prm beta metaGamma ix_term ->
          rec.lambda termId block meta gamma prm beta metaGamma
            ix_term
            (\block' tc -> ix_term (LambdaTerm termId block' meta) (Ch.ArrowCh Ch.NoChange tc))
    , neutral:
        \termId args meta gamma alpha metaGamma ix_term ->
          rec.neutral termId args meta gamma alpha metaGamma
            ix_term
            (ix_term <<< \args' -> NeutralTerm termId args' meta)
    , hole:
        \meta gamma alpha metaGamma ix_term ->
          rec.hole meta gamma alpha metaGamma ix_term
    , match:
        \typeId a cases meta gamma alpha metaGamma constrIDs ix_term ->
          rec.match typeId a cases meta gamma alpha metaGamma constrIDs
            ix_term
            (ix_term <<< \a' -> MatchTerm typeId a' cases meta)
            (\i -> ix_term <<< \case' -> MatchTerm typeId a (List.updateAt' i case' cases) meta)
    }

recArgs ::
  forall a.
  { none :: a
  , cons :: Term -> Args -> ArgConsMetaData -> Context -> Parameter -> Type -> MetaContext -> Index -> Index -> Index -> a
  } ->
  Args -> Context -> Type -> MetaContext -> Index -> a
recArgs rec =
  Rec.recArgs
    { none: \_ -> rec.none
    , cons:
        \a args meta gamma prm beta metaGamma ix_args ->
          rec.cons a args meta gamma prm beta metaGamma
            ix_args
            (ix_args <<< \a' -> ConsArgs a' args meta)
            (ix_args <<< \args' -> ConsArgs a args' meta)
    }

recCase ::
  forall a.
  { case_ :: List TermId -> Term -> CaseMetadata -> Context -> Type -> TypeId -> TermId -> MetaContext -> Index -> Index -> a } ->
  Case -> Context -> Type -> TypeId -> TermId -> MetaContext -> Index -> a
recCase rec =
  Rec.recCase
    { case_:
        \termBnds a meta gamma alpha typeId termId metaGamma ix_case ->
          rec.case_ termBnds a meta gamma alpha typeId termId metaGamma
            ix_case
            (ix_case <<< \a' -> Case termBnds a' meta)
    }

recParameter ::
  forall a.
  { parameter :: Type -> ParameterMetadata -> Context -> MetaContext -> Index -> Index -> a } ->
  Parameter -> Context -> MetaContext -> Index -> a
recParameter rec =
  Rec.recParameter
    { parameter:
        \alpha meta gamma metaGamma ix_prm ->
          rec.parameter alpha meta gamma metaGamma
            ix_prm
            (ix_prm <<< \alpha' -> Parameter alpha' meta)
    }
-}
