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
import Language.Shape.Stlc.Recursion.Context as Rec
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
  { module_ :: List Definition -> ModuleMetadata -> Context -> MetaContext -> a
  } ->
  Module -> Context -> MetaContext -> a
recModule rec = Rec.recModule undefined

recBlock ::
  forall a.
  { block :: List Definition -> Term -> BlockMetadata -> Context -> Type -> MetaContext -> a
  } ->
  Block -> Context -> Type -> MetaContext -> a
recBlock rec = Rec.recBlock undefined

recDefinition ::
  forall a.
  { term :: TermBinding -> Type -> Term -> TermDefinitionMetadata -> Context -> MetaContext -> a
  , data :: TypeBinding -> (List Constructor) -> DataDefinitionMetadata -> Context -> MetaContext -> a
  } ->
  Definition -> Context -> MetaContext -> a
recDefinition rec = Rec.recDefinition undefined

recConstructor ::
  forall a.
  { constructor :: TermBinding -> List Parameter -> ConstructorMetadata -> Context -> MetaContext -> a
  } ->
  Constructor -> Context -> MetaContext -> a
recConstructor rec = Rec.recConstructor undefined

recType ::
  forall a.
  { arrow :: Type -> Type -> ArrowTypeMetadata -> Context -> MetaContext -> a
  , data :: TypeID -> DataTypeMetadata -> Context -> MetaContext -> a
  , hole :: HoleID -> TypeWeakening -> HoleTypeMetadata -> Context -> MetaContext -> a
  } ->
  Type -> Context -> MetaContext -> a
recType rec = Rec.recType undefined

recTerm ::
  forall a.
  { lambda :: TermBinding -> Block -> LambdaTermMetadata -> Context -> Type -> MetaContext -> a
  , neutral :: NeutralTerm -> NeutralTermMetadata -> Context -> Type -> MetaContext -> a
  , hole :: HoleTermMetadata -> Context -> Type -> MetaContext -> a
  , match :: TypeID -> Term -> List Case -> MatchTermMetadata -> Context -> Type -> MetaContext -> a
  } ->
  Term -> Context -> Type -> MetaContext -> a
recTerm rec = Rec.recTerm undefined

recNeutralTerm ::
  forall a.
  { variable :: TermID -> VariableTermMetadata -> Context -> Type -> MetaContext -> a
  , application :: NeutralTerm -> Term -> ApplicationTermMetadata -> Context -> Type -> MetaContext -> a
  } ->
  NeutralTerm -> Context -> Type -> MetaContext -> a
recNeutralTerm rec = Rec.recNeutralTerm undefined

recCase ::
  forall a.
  { case_ :: List TermBinding -> Term -> CaseMetadata -> Context -> Type -> MetaContext -> a } ->
  Case -> Context -> Type -> MetaContext -> a
recCase rec = Rec.recCase undefined

recParameter ::
  forall a.
  { parameter :: Type -> ParameterMetadata -> MetaContext -> a } ->
  Parameter -> Context -> MetaContext -> a
recParameter rec = Rec.recParameter undefined
