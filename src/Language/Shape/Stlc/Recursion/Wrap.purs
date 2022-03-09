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
import Undefined (undefined)
import Unsafe as Unsafe

data TypeChange

type Wrap a
  = a -> TypeChange -> HoleSub -> Module

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
            $ \i def' -> wrap_mod $ Module (List.updateAt' i def' defs) meta
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
            $ \i -> wrap_block <<< \def' -> Block (List.updateAt' i def' defs) a meta
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
        \x alpha a meta gamma metaGamma wrap_def ->
          rec.term x alpha a meta gamma metaGamma
            (wrap_def <<< \alpha' -> TermDefinition x alpha' a meta)
            (wrap_def <<< \a' -> TermDefinition x alpha a' meta)
    , data:
        \x constrs meta gamma metaGamma wrap_def ->
          rec.data x constrs meta gamma metaGamma
            (\i -> wrap_def <<< \constr' -> DataDefinition x (List.updateAt' i constr' constrs) meta)
    }

recConstructor ::
  forall a.
  { constructor :: TermBinding -> List Parameter -> ConstructorMetadata -> Context -> TypeBinding -> MetaContext -> IndexWrap Parameter -> a
  } ->
  Constructor -> Context -> TypeBinding -> MetaContext -> Wrap Constructor -> a
recConstructor rec = Rec.recConstructor undefined

recType ::
  forall a.
  { arrow :: Parameter -> Type -> ArrowTypeMetadata -> Context -> MetaContext -> Wrap Parameter -> Wrap Type -> a
  , data :: TypeID -> DataTypeMetadata -> Context -> MetaContext -> a
  , hole :: HoleID -> TypeWeakening -> HoleTypeMetadata -> Context -> MetaContext -> a
  , proxyHole :: HoleID -> Context -> MetaContext -> a
  } ->
  Type -> Context -> MetaContext -> Wrap Type -> a
recType rec = Rec.recType undefined

recTerm ::
  forall a.
  { lambda :: TermBinding -> Block -> LambdaTermMetadata -> Context -> Type -> MetaContext -> Wrap Block -> a
  , neutral :: NeutralTerm -> NeutralTermMetadata -> Context -> Type -> MetaContext -> Wrap NeutralTerm -> a
  , hole :: HoleTermMetadata -> Context -> Type -> MetaContext -> a
  , match :: TypeID -> Term -> List Case -> MatchTermMetadata -> Context -> Type -> MetaContext -> Wrap Term -> IndexWrap Case -> a
  } ->
  Term -> Context -> Type -> MetaContext -> Wrap Term -> a
recTerm rec = Rec.recTerm undefined

recNeutralTerm ::
  forall a.
  { variable :: TermID -> VariableTermMetadata -> Context -> Type -> MetaContext -> a
  , application :: NeutralTerm -> Term -> ApplicationTermMetadata -> Context -> Parameter -> Type -> MetaContext -> Wrap NeutralTerm -> Wrap Term -> a
  } ->
  NeutralTerm -> Context -> Type -> MetaContext -> Wrap NeutralTerm -> a
recNeutralTerm rec = Rec.recNeutralTerm undefined

recCase ::
  forall a.
  { case_ :: List TermBinding -> Term -> CaseMetadata -> Context -> Type -> TypeID -> TermID -> MetaContext -> Wrap Term -> a } ->
  Case -> Context -> Type -> MetaContext -> Wrap Case -> a
recCase rec = Rec.recCase undefined

recParameter ::
  forall a.
  { parameter :: Type -> ParameterMetadata -> Context -> MetaContext -> Wrap Type -> a } ->
  Parameter -> Context -> MetaContext -> Wrap Parameter -> a
recParameter rec = Rec.recParameter undefined
