module Language.Shape.Stlc.Recursion.Wrap where

import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Typing
import Prelude
import Prim hiding (Type)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Map (Map)
import Language.Shape.Stlc.Recursion.Context as Rec
import Undefined (undefined)
import Unsafe as Unsafe

data TypeChange

type Wrap a
  = a -> TypeChange -> Module

type WrapI a
  = Int -> Wrap a

-- Recursion principles for handling wraping
recModule ::
  forall a.
  { module_ :: List Definition -> ModuleMetadata -> Context -> WrapI Definition -> a
  } ->
  Module -> Context -> Wrap Module -> a
recModule = undefined

recBlock ::
  forall a.
  { block :: List Definition -> Term -> BlockMetadata -> Context -> Type -> a
  } ->
  Block -> Context -> Type -> a
recBlock = undefined

recDefinition ::
  forall a.
  { term :: TermBinding -> Type -> Term -> TermDefinitionMetadata -> Context -> Wrap Type -> Wrap Term -> a
  , data :: TypeBinding -> (List Constructor) -> DataDefinitionMetadata -> Context -> WrapI Constructor -> a
  } ->
  Definition -> Context -> Wrap Definition -> a
recDefinition = undefined

recConstructor ::
  forall a.
  { constructor :: TermBinding -> Type -> ConstructorMetadata -> Context -> Wrap Type -> a
  } ->
  Constructor -> Context -> Wrap Constructor -> a
recConstructor = undefined

recType ::
  forall a.
  { arrow :: Type -> Type -> ArrowTypeMetadata -> Context -> Wrap Type -> Wrap Type -> a
  , data :: TypeID -> DataTypeMetadata -> Context -> a
  , hole :: HoleID -> TypeWeakening -> HoleTypeMetadata -> Context -> a
  } ->
  Type -> Context -> Wrap Type -> a
recType = undefined

recTerm ::
  forall a.
  { lambda :: TermBinding -> Block -> LambdaTermMetadata -> Context -> Type -> Wrap Block -> a
  , neutral :: NeutralTerm -> NeutralTermMetadata -> Context -> Type -> Wrap NeutralTerm -> a
  , hole :: HoleTermMetadata -> Context -> Type -> a
  , match :: TypeID -> Term -> List Term -> MatchTermMetadata -> Context -> Type -> Wrap Term -> WrapI Term -> a
  } ->
  Term -> Context -> Type -> a
recTerm = undefined

recNeutralTerm ::
  forall a.
  { variable :: TermID -> VariableTermMetadata -> Context -> Type -> a
  , application :: NeutralTerm -> Term -> ApplicationTermMetadata -> Context -> Type -> Wrap NeutralTerm -> Wrap Term -> a
  } ->
  NeutralTerm -> Context -> Type -> Wrap NeutralTerm -> a
recNeutralTerm = undefined
