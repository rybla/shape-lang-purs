module Language.Shape.Stlc.Recursion.Index where

import Data.Either
import Data.Foldable
import Data.Maybe
import Data.Tuple.Nested
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Typing
import Prelude
import Prim hiding (Type)
import Control.Monad.State (State, runState)
import Data.Array as Array
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

type Cursor
  = Maybe Index

checkCursorStep :: IndexStep -> Cursor -> Cursor
checkCursorStep step cursor = do
  ix <- cursor
  { head, tail } <- Array.uncons ix
  if head == step then
    Just tail
  else
    Nothing

checkCursorHere :: Cursor -> Boolean
checkCursorHere = case _ of
  Nothing -> false
  Just ix -> Array.null ix

-- TODO: add ix_cursor :: Cursor to traversal, keeping track of whether the cursor is here or at child of here
-- Recursion principles for handling indexing
recModule ::
  forall a.
  { module_ :: List Definition -> ModuleMetadata -> Context -> MetaContext -> Index -> (Int -> Index) -> Boolean -> (Int -> Cursor) -> a
  } ->
  Module -> Context -> MetaContext -> Index -> Cursor -> a
recModule rec =
  Rec.recModule
    { module_:
        \defs meta gamma metaGamma ix cursor ->
          rec.module_ defs meta gamma metaGamma
            ix
            (\i -> ix :> Module_Definition i)
            (checkCursorHere cursor)
            (\i -> checkCursorStep (Module_Definition i) cursor)
    }

recBlock ::
  forall a.
  { block :: List Definition -> Term -> BlockMetadata -> Context -> Type -> MetaContext -> Index -> (Int -> Index) -> Index -> Boolean -> (Int -> Cursor) -> Cursor -> a
  } ->
  Block -> Context -> Type -> MetaContext -> Index -> Cursor -> a
recBlock rec =
  Rec.recBlock
    { block:
        \defs a meta gamma alpha metaGamma ix cursor ->
          rec.block defs a meta gamma alpha metaGamma
            ix
            (\i -> ix :> Block_Definition i)
            (ix :> Block_Term)
            (checkCursorHere cursor)
            (\i -> checkCursorStep (Block_Definition i) cursor)
            (checkCursorStep Block_Term cursor)
    }

-- not sure if i need this
{-
recDefinitions ::
  forall a.
  { definitions :: List Definition -> Context -> MetaContext -> (Int -> Index) -> (Int -> Cursor) -> a } ->
  List Definition -> Context -> MetaContext -> (Int -> Index) -> Cursor -> a
recDefinitions rec =
  Rec.recDefinitions
    { definitions:
        \defs gamma metaGamma ix_def_at cursor ->
          -- TODO: put the displaced terms resulting from typechanges applied to terms into this list of definitions
          rec.definitions defs gamma metaGamma ix_def_at
            (\i -> checkCursorStep )
    }
-}
{-
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
-}
recType ::
  forall a.
  { arrow :: Parameter -> Type -> ArrowTypeMetadata -> Context -> MetaContext -> Index -> Index -> Index -> Boolean -> Cursor -> Cursor -> a
  , data :: TypeId -> DataTypeMetadata -> Context -> MetaContext -> Index -> Boolean -> a
  , hole :: HoleId -> TypeWeakening -> HoleTypeMetadata -> Context -> MetaContext -> Index -> Boolean -> a
  , proxyHole :: HoleId -> Context -> MetaContext -> Index -> Boolean -> a
  } ->
  Type -> Context -> MetaContext -> Index -> Cursor -> a
recType rec =
  Rec.recType
    { arrow:
        \prm beta meta gamma metaGamma ix cursor ->
          rec.arrow prm beta meta gamma metaGamma
            ix
            (ix :> ArrowType_Parameter)
            (ix :> ArrowType_Type)
            (checkCursorHere cursor)
            (checkCursorStep ArrowType_Parameter cursor)
            (checkCursorStep ArrowType_Type cursor)
    , data: \typeId meta gamma metaGamma ix cursor -> rec.data typeId meta gamma metaGamma ix (checkCursorHere cursor)
    , hole: \holeID wkn meta gamma metaGamma ix cursor -> rec.hole holeID wkn meta gamma metaGamma ix (checkCursorHere cursor)
    , proxyHole: \holeID gamma metaGamma ix cursor -> rec.proxyHole holeID gamma metaGamma ix (checkCursorHere cursor)
    }

recTerm ::
  forall a.
  { lambda :: TermId -> Block -> LambdaTermMetadata -> Context -> Parameter -> Type -> MetaContext -> Index -> Index -> Index -> Boolean -> Cursor -> Cursor -> a
  , neutral :: TermId -> Args -> NeutralTermMetadata -> Context -> Type -> MetaContext -> Index -> Index -> Index -> Boolean -> Cursor -> Cursor -> a
  , hole :: HoleTermMetadata -> Context -> Type -> MetaContext -> Index -> Boolean -> a
  , match :: TypeId -> Term -> List Case -> MatchTermMetadata -> Context -> Type -> MetaContext -> List TermId -> Index -> Index -> (Int -> Index) -> Boolean -> Cursor -> (Int -> Cursor) -> a
  } ->
  Term -> Context -> Type -> MetaContext -> Index -> Cursor -> a
recTerm rec =
  Rec.recTerm
    { lambda:
        \termId block meta gamma prm beta metaGamma ix cursor ->
          rec.lambda termId block meta gamma prm beta metaGamma
            ix
            (ix :> LambdaTerm_TermId)
            (ix :> LambdaTerm_Block)
            (checkCursorHere cursor)
            (checkCursorStep LambdaTerm_TermId cursor)
            (checkCursorStep LambdaTerm_Block cursor)
    , neutral:
        \termId args meta gamma alpha metaGamma ix cursor ->
          rec.neutral termId args meta gamma alpha metaGamma
            ix
            (ix :> NeutralTerm_TermId)
            (ix :> NeutralTerm_Args)
            (checkCursorHere cursor)
            (checkCursorStep NeutralTerm_TermId cursor)
            (checkCursorStep NeutralTerm_Args cursor)
    , hole:
        \meta gamma alpha metaGamma ix cursor ->
          rec.hole meta gamma alpha metaGamma ix (checkCursorHere cursor)
    , match:
        \typeId a cases meta gamma alpha metaGamma constrIDs ix cursor ->
          rec.match typeId a cases meta gamma alpha metaGamma constrIDs
            ix
            (ix :> MatchTerm_Term)
            (\i -> ix :> MatchTerm_Case i)
            (checkCursorHere cursor)
            (checkCursorStep MatchTerm_Term cursor)
            (\i -> checkCursorStep (MatchTerm_Case i) cursor)
    }

recArgs ::
  forall a.
  { none :: a
  , cons :: Term -> Args -> ArgConsMetaData -> Context -> Parameter -> Type -> MetaContext -> Index -> Index -> Index -> Boolean -> Cursor -> Cursor -> a
  } ->
  Args -> Context -> Type -> MetaContext -> Index -> Cursor -> a
recArgs rec =
  Rec.recArgs
    { none: \_ _ -> rec.none
    , cons:
        \a args meta gamma prm beta metaGamma ix cursor ->
          rec.cons a args meta gamma prm beta metaGamma
            ix
            (ix :> ConsArgs_Term)
            (ix :> ConsArgs_Args)
            (checkCursorHere cursor)
            (checkCursorStep ConsArgs_Term cursor)
            (checkCursorStep ConsArgs_Args cursor)
    }

{-
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
-}
{-
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
