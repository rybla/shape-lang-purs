module Language.Shape.Stlc.Recursion.Wrap where

import Data.Tuple.Nested
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Typing
import Prelude
import Prelude hiding (mod)
import Prim hiding (Type)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Language.Shape.Stlc.Recursion.Context as Rec
import Undefined (undefined)
import Unsafe as Unsafe

data HoleSubstitution  -- TODO

data ChangeContext  -- TODO

type Wrap a
  = a -> HoleSubstitution -> ChangeContext -> Module /\ ChangeContext

type WrapI a
  = Int -> Wrap a

-- Recursion principles which handle wraping
recModule ::
  forall a.
  { module_ :: List Definition -> ModuleMetadata -> Context -> WrapI Definition -> a
  } ->
  Module -> Context -> Wrap Module -> a
recModule rec =
  Rec.recModule
    { module_:
        \defs meta gamma wrapMod ->
          rec.module_ defs meta gamma
            (\i def' -> wrapMod $ Module (Unsafe.fromJust $ List.updateAt i def' defs) meta)
    }

recBlock ::
  forall a.
  { block :: List Definition -> Term -> BlockMetadata -> Context -> Type -> WrapI Definition -> Wrap Term -> a
  } ->
  Block -> Context -> Type -> Wrap Block -> a
recBlock rec =
  Rec.recBlock
    { block:
        \defs a meta gamma alpha wrapBlock ->
          rec.block defs a meta gamma alpha
            (\i def' -> wrapBlock $ Block (Unsafe.fromJust $ List.updateAt i def' defs) a meta)
            (\a' -> wrapBlock $ Block defs a' meta)
    }

recDefinition ::
  forall a.
  { term :: TermID -> Type -> Term -> TermDefinitionMetadata -> Context -> Wrap Type -> Wrap Term -> a
  , data :: TypeID -> (List Constructor) -> DataDefinitionMetadata -> Context -> WrapI Constructor -> a
  } ->
  Definition -> Context -> Wrap Definition -> a
recDefinition rec =
  Rec.recDefinition
    { term:
        \id alpha a meta gamma wrapDef ->
          rec.term id alpha a meta gamma
            (\alpha' -> wrapDef $ TermDefinition id alpha' a meta)
            (\a' -> wrapDef $ TermDefinition id alpha a' meta)
    , data:
        \id constrs meta gamma wrapDef ->
          rec.data id constrs meta gamma
            (\i constr' -> wrapDef $ DataDefintion id (Unsafe.fromJust $ List.updateAt i constr' constrs) meta)
    }

recConstructor ::
  forall a.
  { constructor :: TermID -> Type -> ConstructorMetadata -> Context -> Wrap Type -> a
  } ->
  Constructor -> Context -> Wrap Constructor -> a
recConstructor rec =
  Rec.recConstructor
    { constructor:
        \id alpha meta gamma wrapConstr ->
          rec.constructor id alpha meta gamma (\alpha' -> wrapConstr $ Constructor id alpha' meta)
    }

recTerm ::
  forall a.
  { lambda :: TermID -> Block -> LambdaTermMetadata -> Context -> Type -> Wrap Block -> a
  , application :: TermID -> List Term -> ApplicationTermMetadata -> Context -> Type -> WrapI Term -> a
  , hole :: HoleTermMetadata -> Context -> Type -> a
  , match :: TypeID -> Term -> List Term -> MatchTermMetadata -> Context -> Type -> Wrap Term -> WrapI Term -> a
  } ->
  Term -> Context -> Type -> Wrap Term -> a
recTerm rec =
  Rec.recTerm
    { lambda:
        \id block meta gamma alpha wrapTerm ->
          rec.lambda id block meta gamma alpha
            (\block' -> wrapTerm $ LambdaTerm id block' meta)
    , application:
        \id args meta gamma alpha wrapTerm ->
          rec.application id args meta gamma alpha
            (\i arg' -> wrapTerm $ ApplicationTerm id (Unsafe.fromJust $ List.updateAt i arg' args) meta)
    , hole:
        \meta gamma alpha wrapTerm ->
          rec.hole meta gamma alpha
    , match:
        \id a cases meta gamma alpha wrapTerm ->
          rec.match id a cases meta gamma alpha
            (\a' -> wrapTerm $ MatchTerm id a' cases meta)
            (\i case' -> wrapTerm $ MatchTerm id a (Unsafe.fromJust $ List.updateAt i case' cases) meta)
    }

recType ::
  forall a.
  { arrow :: Type -> Type -> ArrowTypeMetadata -> Context -> Wrap Type -> Wrap Type -> a
  , data :: TypeID -> DataTypeMetadata -> Context -> a
  , hole :: HoleID -> TypeWeakening -> HoleTypeMetadata -> Context -> a
  } ->
  Type -> Context -> Wrap Type -> a
recType rec =
  Rec.recType
    { arrow:
        \alpha beta meta gamma wrapType ->
          rec.arrow alpha beta meta gamma
            (\alpha' -> wrapType $ ArrowType alpha' beta meta)
            (\beta' -> wrapType $ ArrowType alpha beta' meta)
    , data:
        \id meta gamma wrapType ->
          rec.data id meta gamma
    , hole:
        \id wkn meta gamma wrapType ->
          rec.hole id wkn meta gamma
    }
