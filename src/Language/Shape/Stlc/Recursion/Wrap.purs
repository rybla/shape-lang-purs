module Language.Shape.Stlc.Recursion.Wrap where

import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Typing
import Prelude
import Prim hiding (Type)
import Data.List.Unsafe (List)
import Data.List.Unsafe as List
import Data.Map (Map)
import Data.Map as Map
import Language.Holes (HoleSub)
import Language.Shape.Stlc.Recursion.MetaContext (MetaContext)
import Language.Shape.Stlc.Recursion.MetaContext as Rec
import Unsafe as Unsafe

data TypeChange

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
    }

recDefinition ::
  forall a.
  { term :: TermBinding -> Type -> Term -> TermDefinitionMetadata -> Context -> MetaContext -> Wrap Type -> Wrap Term -> a
  , data :: TypeBinding -> List Constructor -> DataDefinitionMetadata -> Context -> MetaContext -> IndexWrap Constructor -> a
  } ->
  Definition -> Context -> MetaContext -> Wrap Definition -> a
recDefinition rec =
  Rec.recDefinition
    { term:
        \termBnd alpha a meta gamma metaGamma wrap_def ->
          rec.term termBnd alpha a meta gamma metaGamma
            (wrap_def <<< \alpha' -> TermDefinition termBnd alpha' a meta)
            (wrap_def <<< \a' -> TermDefinition termBnd alpha a' meta)
    , data:
        \typeBnd constrs meta gamma metaGamma wrap_def ->
          rec.data typeBnd constrs meta gamma metaGamma
            (\i -> wrap_def <<< \constr' -> DataDefinition typeBnd (List.updateAt' i constr' constrs) meta)
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
  { lambda :: TermBinding -> Block -> LambdaTermMetadata -> Context -> Type -> MetaContext -> Wrap Block -> a
  , neutral :: NeutralTerm -> NeutralTermMetadata -> Context -> Type -> MetaContext -> Wrap NeutralTerm -> a
  , hole :: HoleTermMetadata -> Context -> Type -> MetaContext -> a
  , match :: TypeID -> Term -> List Case -> MatchTermMetadata -> Context -> Type -> MetaContext -> List TermID -> Wrap Term -> IndexWrap Case -> a
  } ->
  Term -> Context -> Type -> MetaContext -> Wrap Term -> a
recTerm rec =
  Rec.recTerm
    { lambda:
        \termBnd block meta gamma alpha metaGamma wrap_term ->
          rec.lambda termBnd block meta gamma alpha metaGamma
            (wrap_term <<< \block' -> LambdaTerm termBnd block' meta)
    , neutral:
        \neu meta gamma alpha metaGamma wrap_term ->
          rec.neutral neu meta gamma alpha metaGamma
            (wrap_term <<< \neu' -> NeutralTerm neu' meta)
    , hole:
        \meta gamma alpha metaGamma _ ->
          rec.hole meta gamma alpha metaGamma
    , match:
        \typeID a cases meta gamma alpha metaGamma constrIDs wrap_term ->
          rec.match typeID a cases meta gamma alpha metaGamma constrIDs
            (wrap_term <<< \a' -> MatchTerm typeID a' cases meta)
            (\i -> wrap_term <<< \case' -> MatchTerm typeID a (List.updateAt' i case' cases) meta)
    }

recNeutralTerm ::
  forall a.
  { variable :: TermID -> VariableTermMetadata -> Context -> Type -> MetaContext -> a
  , application :: NeutralTerm -> Term -> ApplicationTermMetadata -> Context -> Parameter -> Type -> MetaContext -> Wrap NeutralTerm -> Wrap Term -> a
  } ->
  NeutralTerm -> Context -> Type -> MetaContext -> Wrap NeutralTerm -> a
recNeutralTerm rec =
  Rec.recNeutralTerm
    { variable:
        \termID meta gamma alpha metaGamma _ ->
          rec.variable termID meta gamma alpha metaGamma
    , application:
        \neu a meta gamma prm alpha metaGamma wrap_neu ->
          rec.application neu a meta gamma prm alpha metaGamma
            (wrap_neu <<< \neu' -> ApplicationTerm neu' a meta)
            (wrap_neu <<< \a' -> ApplicationTerm neu a' meta)
    }

recCase ::
  forall a.
  { case_ :: List TermBinding -> Term -> CaseMetadata -> Context -> Type -> TypeID -> TermID -> MetaContext -> Wrap Term -> a } ->
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
