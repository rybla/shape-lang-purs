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
import Language.Shape.Stlc.Recursion.MetaContext as RecMetaContext
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

-- Recursion principles for handling indexing
type RecModule a
  = RecMetaContext.RecModule (Index -> Cursor -> a)

type RecModule_Module a
  = RecMetaContext.RecModule_Module (Index -> (Int -> Index) -> Boolean -> (Int -> Boolean) -> (Int -> Cursor) -> a)

recModule ::
  forall a.
  { module_ :: RecModule_Module a } ->
  RecModule a
recModule rec =
  RecMetaContext.recModule
    { module_:
        \defs meta gamma metaGamma ix cursor ->
          rec.module_ defs meta gamma metaGamma
            ix
            (\i -> ix :> Module_Definition i)
            (checkCursorHere cursor)
            (\i -> checkCursorHere (checkCursorStep (Module_Definition i) cursor))
            (\i -> checkCursorStep (Module_Definition i) cursor)
    }

type RecBlock a
  = RecMetaContext.RecBlock (Index -> Cursor -> a)

type RecBlock_Block a
  = RecMetaContext.RecBlock_Block (Index -> (Int -> Index) -> Index -> Boolean -> (Int -> Boolean) -> (Int -> Cursor) -> Cursor -> a)

recBlock ::
  forall a.
  { block :: RecBlock_Block a } ->
  RecBlock a
recBlock rec =
  RecMetaContext.recBlock
    { block:
        \defs a meta gamma alpha metaGamma ix cursor ->
          rec.block defs a meta gamma alpha metaGamma
            ix
            (\i -> ix :> Block_Definition i)
            (ix :> Block_Term)
            (checkCursorHere cursor)
            (\i -> checkCursorHere (checkCursorStep (Block_Definition i) cursor))
            (\i -> checkCursorStep (Block_Definition i) cursor)
            (checkCursorStep Block_Term cursor)
    }

type RecDefinitions a
  = RecMetaContext.RecDefinitions (a)

type RecDefinitions_Definitions a
  = RecMetaContext.RecDefinitions_Definitions (a)

recDefinitions ::
  forall a.
  { definitions :: RecDefinitions_Definitions a } ->
  RecDefinitions a
recDefinitions = undefined

recDefinition = undefined

recConstructor = undefined

{-
recDefinitions ::
  forall a.
  { definitions :: List Definition -> Context -> MetaContext -> (Int -> Index) -> (Int -> Cursor) -> a } ->
  List Definition -> Context -> MetaContext -> (Int -> Index) -> Cursor -> a
recDefinitions rec =
  RecMetaContext.recDefinitions
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
  RecMetaContext.recConstructor
    { constructor:
        \termBnd prms meta gamma typeBnd metaGamma ix_constr ->
          rec.constructor termBnd prms meta gamma typeBnd metaGamma
            ix_constr
            (\i -> ix_constr <<< \prm' -> Constructor termBnd (List.updateAt' i prm' prms) meta)
    }
-}
type RecType a
  = RecMetaContext.RecType (Index -> Cursor -> a)

type RecType_Arrow a
  = RecMetaContext.RecType_Arrow (Index -> Index -> Index -> Boolean -> Cursor -> Cursor -> a)

type RecType_Data a
  = RecMetaContext.RecType_Data (Index -> Boolean -> a)

type RecType_Hole a
  = RecMetaContext.RecType_Hole (Index -> Boolean -> a)

type RecType_ProxyHole a
  = RecMetaContext.RecType_ProxyHole (Index -> Boolean -> a)

recType ::
  forall a.
  { arrow :: RecType_Arrow a
  , data :: RecType_Data a
  , hole :: RecType_Hole a
  , proxyHole :: RecType_ProxyHole a
  } ->
  RecType a
recType rec =
  RecMetaContext.recType
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

type RecTerm a
  = RecMetaContext.RecTerm (Index -> Cursor -> a)

type RecTerm_Lambda a
  = RecMetaContext.RecTerm_Lambda (Index -> Index -> Index -> Boolean -> Cursor -> Cursor -> a)

type RecTerm_Neutral a
  = RecMetaContext.RecTerm_Neutral (Index -> Index -> Index -> Boolean -> Cursor -> Cursor -> a)

type RecTerm_Match a
  = RecMetaContext.RecTerm_Match (Index -> Index -> (Int -> Index) -> Boolean -> Cursor -> (Int -> Cursor) -> a)

type RecTerm_Hole a
  = RecMetaContext.RecTerm_Hole (Index -> Boolean -> a)

recTerm ::
  forall a.
  { lambda :: RecTerm_Lambda a
  , neutral :: RecTerm_Neutral a
  , match :: RecTerm_Match a
  , hole :: RecTerm_Hole a
  } ->
  RecTerm a
recTerm rec =
  RecMetaContext.recTerm
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
    , match:
        \typeId a cases meta gamma alpha metaGamma constrIDs ix cursor ->
          rec.match typeId a cases meta gamma alpha metaGamma constrIDs
            ix
            (ix :> MatchTerm_Term)
            (\i -> ix :> MatchTerm_Case i)
            (checkCursorHere cursor)
            (checkCursorStep MatchTerm_Term cursor)
            (\i -> checkCursorStep (MatchTerm_Case i) cursor)
    , hole:
        \meta gamma alpha metaGamma ix cursor ->
          rec.hole meta gamma alpha metaGamma ix (checkCursorHere cursor)
    }

type RecArgs a
  = RecMetaContext.RecArgs (Index -> Cursor -> a)

type RecArgs_None (a :: Prim.Type)
  = RecMetaContext.RecArgs_None a

type RecArgs_Cons a
  = RecMetaContext.RecArgs_Cons (Index -> Index -> Index -> Boolean -> Cursor -> Cursor -> a)

recArgs ::
  forall a.
  { none :: RecArgs_None a
  , cons :: RecArgs_Cons a
  } ->
  RecArgs a
recArgs rec =
  RecMetaContext.recArgs
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
  RecMetaContext.recCase
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
  RecMetaContext.recParameter
    { parameter:
        \alpha meta gamma metaGamma ix_prm ->
          rec.parameter alpha meta gamma metaGamma
            ix_prm
            (ix_prm <<< \alpha' -> Parameter alpha' meta)
    }
-}
