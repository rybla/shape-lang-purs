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
  { module_ :: List Definition -> ModuleMetadata -> Context -> MetaContext -> IndexWrap Definition -> a
  } ->
  Module -> Context -> MetaContext -> Wrap Module -> a
recModule rec =
  Rec.recModule
    { module_:
        \defs meta gamma metaGamma wrap_mod ->
          rec.module_ defs meta gamma metaGamma
            (\i def' -> wrap_mod $ Module (List.updateAt' i def' defs) meta)
    }

recBlock ::
  forall a.
  { block :: List Definition -> Term -> BlockMetadata -> Context -> Type -> MetaContext -> IndexWrap Definition -> a
  } ->
  Block -> Context -> Type -> MetaContext -> Wrap Block -> a
recBlock rec =
  Rec.recBlock
    { block:
        \defs a meta gamma alpha metaGamma wrap_block ->
          rec.block defs a meta gamma alpha metaGamma
            (\i -> wrap_block <<< \def' -> Block (List.updateAt' i def' defs) a meta)
    -- (\i def' tc )
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
            -- foldl ?f defs ?a
            ( \defs' tc ->
                let
                  defs'' /\ sub =
                    foldl
                      ( \(displaceds /\ sub) -> case _ of
                          DataDefinition typeBnd constr meta -> undefined -- TODO: not sure what to do here...
                          TermDefinition termBnd alpha a meta ->
                            let
                              changes = undefined :: Changes {-JACOB-}

                              st = undefined :: List Definition /\ Map HoleID Type {-JACOB-}

                              (a' /\ displaceds' /\ sub') = runState (chTerm gamma alpha changes tc a) st
                            in
                              (displaceds <> List.singleton (TermDefinition termBnd alpha a' meta) <> displaceds') /\ Map.union sub sub'
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
  , data :: TypeID -> DataTypeMetadata -> Context -> MetaContext -> a
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
    , data: \typeID meta gamma metaGamma _ -> rec.data typeID meta gamma metaGamma
    , hole: \holeID wkn meta gamma metaGamma _ -> rec.hole holeID wkn meta gamma metaGamma
    , proxyHole: \holeID gamma metaGamma _ -> rec.proxyHole holeID gamma metaGamma
    }

recTerm ::
  forall a.
  { lambda :: TermID -> Block -> LambdaTermMetadata -> Context -> Type -> MetaContext -> Wrap Block -> a
  , neutral :: TermID -> Args -> NeutralTermMetadata -> Context -> Type -> MetaContext -> Wrap Args -> a
  , hole :: HoleTermMetadata -> Context -> Type -> MetaContext -> a
  , match :: TypeID -> Term -> List Case -> MatchTermMetadata -> Context -> Type -> MetaContext -> List TermID -> Wrap Term -> IndexWrap Case -> a
  } ->
  Term -> Context -> Type -> MetaContext -> Wrap Term -> a
recTerm rec =
  Rec.recTerm
    { lambda:
        \termID block meta gamma alpha metaGamma wrap_term ->
          rec.lambda termID block meta gamma alpha metaGamma
            (\block' tc -> wrap_term (LambdaTerm termID block' meta) (ArrowCh NoChange tc))
    , neutral:
        \termID args meta gamma alpha metaGamma wrap_term ->
          rec.neutral termID args meta gamma alpha metaGamma
            (wrap_term <<< \args' -> NeutralTerm termID args' meta)
    , hole:
        \meta gamma alpha metaGamma _ ->
          rec.hole meta gamma alpha metaGamma
    , match:
        \typeID a cases meta gamma alpha metaGamma constrIDs wrap_term ->
          rec.match typeID a cases meta gamma alpha metaGamma constrIDs
            (wrap_term <<< \a' -> MatchTerm typeID a' cases meta)
            (\i -> wrap_term <<< \case' -> MatchTerm typeID a (List.updateAt' i case' cases) meta)
    }

recArgs ::
  forall a.
  { none :: a
  , cons :: Term -> Args -> ArgConsMetaData -> Context -> Type -> MetaContext -> Wrap Term -> Wrap Args -> a
  } ->
  Args -> Context -> Type -> MetaContext -> Wrap Args -> a
recArgs = undefined

recCase ::
  forall a.
  { case_ :: List TermID -> Term -> CaseMetadata -> Context -> Type -> TypeID -> TermID -> MetaContext -> Wrap Term -> a } ->
  Case -> Context -> Type -> TypeID -> TermID -> MetaContext -> Wrap Case -> a
recCase rec =
  Rec.recCase
    { case_:
        \termBnds a meta gamma alpha typeID termID metaGamma wrap_case ->
          rec.case_ termBnds a meta gamma alpha typeID termID metaGamma
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
