module Language.Shape.Stlc.Recursion.MetaContext where

import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Typing
import Prelude
import Prim hiding (Type)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Map (Map)
import Language.Shape.Stlc.Recursion.Wrap as Rec
import Language.Shape.Stlc.Recursion.Wrap (Wrap, WrapI)
import Undefined (undefined)
import Unsafe as Unsafe

-- TODO: implement this stuff
-- Context for metadata info such as names, constructor names, shadowing, etc.
type MetaContext
  = { names :: Map TermID TermName
    , shadows :: Map TermName Int
    , constructors :: Map TypeID (List TermID)
    }

-- Recursion principles for handling the metacontext
recModule ::
  forall a.
  { module_ :: List Definition -> ModuleMetadata -> Context -> WrapI Definition -> MetaContext -> a
  } ->
  Module -> Context -> Wrap Module -> MetaContext -> a
recModule = undefined

recBlock ::
  forall a.
  { block :: List Definition -> Term -> BlockMetadata -> Context -> Type -> MetaContext -> a
  } ->
  Block -> Context -> Type -> MetaContext -> a
recBlock = undefined

recDefinition ::
  forall a.
  { term :: TermBinding -> Type -> Term -> TermDefinitionMetadata -> Context -> Wrap Type -> Wrap Term -> MetaContext -> a
  , data :: TypeBinding -> (List Constructor) -> DataDefinitionMetadata -> Context -> WrapI Constructor -> MetaContext -> a
  } ->
  Definition -> Context -> Wrap Definition -> MetaContext -> a
recDefinition = undefined

recConstructor ::
  forall a.
  { constructor :: TermBinding -> Type -> ConstructorMetadata -> Context -> Wrap Type -> MetaContext -> a
  } ->
  Constructor -> Context -> Wrap Constructor -> MetaContext -> a
recConstructor = undefined

recType ::
  forall a.
  { arrow :: Type -> Type -> ArrowTypeMetadata -> Context -> Wrap Type -> Wrap Type -> MetaContext -> a
  , data :: TypeID -> DataTypeMetadata -> Context -> MetaContext -> a
  , hole :: HoleID -> TypeWeakening -> HoleTypeMetadata -> Context -> MetaContext -> a
  } ->
  Type -> Context -> Wrap Type -> MetaContext -> a
recType = undefined

recTerm ::
  forall a.
  { lambda :: TermBinding -> Block -> LambdaTermMetadata -> Context -> Type -> Wrap Block -> MetaContext -> a
  , neutral :: NeutralTerm -> NeutralTermMetadata -> Context -> Type -> Wrap NeutralTerm -> MetaContext -> a
  , hole :: HoleTermMetadata -> Context -> Type -> MetaContext -> a
  , match :: TypeID -> Term -> List Term -> MatchTermMetadata -> Context -> Type -> Wrap Term -> WrapI Term -> MetaContext -> a
  } ->
  Term -> Context -> Type -> MetaContext -> a
recTerm = undefined

recNeutralTerm ::
  forall a.
  { variable :: TermID -> VariableTermMetadata -> Context -> Type -> MetaContext -> a
  , application :: NeutralTerm -> Term -> ApplicationTermMetadata -> Context -> Type -> Wrap NeutralTerm -> Wrap Term -> MetaContext -> a
  } ->
  NeutralTerm -> Context -> Type -> Wrap NeutralTerm -> MetaContext -> a
recNeutralTerm = undefined

recCase ::
  forall a.
  { case_ :: List TermBinding -> Term -> CaseMetadata -> Context -> Type -> Wrap Term -> MetaContext -> a } ->
  Case -> Context -> Type -> Wrap Case -> MetaContext -> a
recCase rec = undefined

recParameter ::
  forall a.
  { parameter :: Type -> ParameterMetadata -> Wrap Type -> MetaContext -> a } ->
  Parameter -> Context -> Wrap Parameter -> MetaContext -> a
recParameter rec = undefined
