module Language.Shape.Stlc.Recursion.Wrap where

import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Typing
import Prelude
import Prim hiding (Type)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Language.Shape.Stlc.Recursion.MetaContext (MetaContext)
import Language.Shape.Stlc.Recursion.MetaContext as Rec
import Undefined (undefined)
import Unsafe as Unsafe

data TypeChange

type Wrap a
  = a -> TypeChange -> Module

type WrapI a
  = Int -> MetaContext -> Wrap a

-- Recursion principles for handling wraping
recModule ::
  forall a.
  { module_ :: List Definition -> ModuleMetadata -> Context -> MetaContext -> WrapI Definition -> a
  } ->
  Module -> Context -> MetaContext -> Wrap Module -> a
recModule rec = Rec.recModule undefined

recBlock ::
  forall a.
  { block :: List Definition -> Term -> BlockMetadata -> Context -> Type -> a
  } ->
  Block -> Context -> Type -> MetaContext -> Wrap Block -> a
recBlock rec = Rec.recBlock undefined

recDefinition ::
  forall a.
  { term :: TermBinding -> Type -> Term -> TermDefinitionMetadata -> Context -> MetaContext -> Wrap Type -> Wrap Term -> a
  , data :: TypeBinding -> List Constructor -> DataDefinitionMetadata -> Context -> MetaContext -> WrapI Constructor -> a
  } ->
  Definition -> Context -> MetaContext -> Wrap Definition -> a
recDefinition rec = Rec.recDefinition undefined

recConstructor ::
  forall a.
  { constructor :: TermBinding -> List Parameter -> ConstructorMetadata -> Context -> MetaContext -> WrapI Parameter -> a
  } ->
  Constructor -> Context -> MetaContext -> Wrap Constructor -> a
recConstructor rec = Rec.recConstructor undefined

recType ::
  forall a.
  { arrow :: Parameter -> Type -> ArrowTypeMetadata -> Context -> MetaContext -> Wrap Parameter -> Wrap Type -> a
  , data :: TypeID -> DataTypeMetadata -> Context -> MetaContext -> a
  , hole :: HoleID -> TypeWeakening -> HoleTypeMetadata -> Context -> MetaContext -> a
  } ->
  Type -> Context -> MetaContext -> Wrap Type -> a
recType rec = Rec.recType undefined

recTerm ::
  forall a.
  { lambda :: TermBinding -> Block -> LambdaTermMetadata -> Context -> Type -> MetaContext -> Wrap Block -> a
  , neutral :: NeutralTerm -> NeutralTermMetadata -> Context -> Type -> MetaContext -> Wrap NeutralTerm -> a
  , hole :: HoleTermMetadata -> Context -> Type -> MetaContext -> a
  , match :: TypeID -> Term -> List Case -> MatchTermMetadata -> Context -> Type -> MetaContext -> Wrap Term -> WrapI Term -> a
  } ->
  Term -> Context -> Type -> MetaContext -> Wrap Term -> a
recTerm rec = Rec.recTerm undefined

recNeutralTerm ::
  forall a.
  { variable :: TermID -> VariableTermMetadata -> Context -> Type -> MetaContext -> a
  , application :: NeutralTerm -> Term -> ApplicationTermMetadata -> Context -> Type -> MetaContext -> Wrap NeutralTerm -> Wrap Term -> a
  } ->
  NeutralTerm -> Context -> Type -> MetaContext -> Wrap NeutralTerm -> a
recNeutralTerm rec = Rec.recNeutralTerm undefined

recCase ::
  forall a.
  { case_ :: List TermBinding -> Term -> CaseMetadata -> Context -> Type -> MetaContext -> Wrap Term -> a } ->
  Case -> Context -> Type -> MetaContext -> Wrap Case -> a
recCase rec = Rec.recCase undefined

recParameter ::
  forall a.
  { parameter :: Type -> ParameterMetadata -> MetaContext -> Wrap Type -> a } ->
  Parameter -> Context -> MetaContext -> Wrap Parameter -> a
recParameter rec = Rec.recParameter undefined
