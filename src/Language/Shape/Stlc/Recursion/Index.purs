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
  = RecMetaContext.RecModule_Module
      ( Index -> -- module
        Boolean -> -- module
        (Int -> Index) -> -- definition
        (Int -> Cursor) -> -- definition
        a
      )

recModule ::
  forall a.
  { module_ :: RecModule_Module a } ->
  RecModule a
recModule rec =
  RecMetaContext.recModule
    { module_:
        \defs meta gamma metaGamma ix cursor ->
          rec.module_ defs meta gamma metaGamma
            -- module
            ix
            (checkCursorHere cursor)
            -- definitions
            (\i -> ix :> Module_Definition i)
            (\i -> checkCursorStep (Module_Definition i) cursor)
    }

type RecBlock a
  = RecMetaContext.RecBlock (Index -> Cursor -> a)

type RecBlock_Block a
  = RecMetaContext.RecBlock_Block
      ( Index -> -- block
        Boolean -> -- block
        (Int -> Index) -> -- definition
        (Int -> Cursor) -> -- definition
        Index -> -- term
        Cursor -> -- term
        a
      )

recBlock ::
  forall a.
  { block :: RecBlock_Block a } ->
  RecBlock a
recBlock rec =
  RecMetaContext.recBlock
    { block:
        \defs a meta gamma alpha metaGamma ix cursor ->
          rec.block defs a meta gamma alpha metaGamma
            -- block
            ix
            (checkCursorHere cursor)
            -- definitions
            (\i -> ix :> Block_Definition i)
            (\i -> checkCursorStep (Block_Definition i) cursor)
            -- term
            (ix :> Block_Term)
            (checkCursorStep Block_Term cursor)
    }

type RecDefinitions a
  = RecMetaContext.RecDefinitions
      ( Index -> -- module/block
        (Int -> Index) -> -- definition
        (Int -> Cursor) -> -- definition
        a
      )

type RecDefinitions_Definitions a
  = RecMetaContext.RecDefinitions_Definitions
      ( Index -> -- module/block
        (Int -> Index) -> -- definition
        (Int -> Cursor) -> -- definition
        a
      )

recDefinitions ::
  forall a.
  { definitions :: RecDefinitions_Definitions a } ->
  RecDefinitions a
recDefinitions = RecMetaContext.recDefinitions

type RecDefinition a
  = RecMetaContext.RecDefinition
      ( Index -> -- module/block
        Index -> -- definition
        Cursor -> -- definition
        a
      )

type RecDefinition_TermDefinition a
  = RecMetaContext.RecDefinition_TermDefinition
      ( Index -> -- module/block
        Index -> -- definition
        Boolean -> -- definition
        Index -> -- termId
        Cursor -> -- termId
        Index -> -- type
        Cursor -> -- type
        Index -> -- term
        Cursor -> -- term
        a
      )

type RecDefinition_DataDefinition a
  = RecMetaContext.RecDefinition_DataDefinition
      ( Index -> -- module/block
        Index -> -- definition
        Boolean -> -- definition
        Index -> -- typeId
        Cursor -> -- typeId
        (Int -> Index) -> -- constructors
        (Int -> Cursor) -> -- constructors
        a
      )

recDefinition ::
  forall a.
  { term :: RecDefinition_TermDefinition a
  , data :: RecDefinition_DataDefinition a
  } ->
  RecDefinition a
recDefinition rec =
  RecMetaContext.recDefinition
    { term:
        \termBinding alpha a meta gamma metaGamma ix_parent ix cursor ->
          rec.term termBinding alpha a meta gamma metaGamma
            ix_parent
            -- definition
            ix
            (checkCursorHere cursor)
            -- termBinding
            (ix :> TermDefinition_TermBinding)
            (checkCursorStep TermDefinition_TermBinding cursor)
            -- type
            (ix :> TermDefinition_Type)
            (checkCursorStep TermDefinition_Type cursor)
            -- term
            (ix :> TermDefinition_Term)
            (checkCursorStep TermDefinition_Term cursor)
    , data:
        \typeBinding constrs meta gamma metaGamma ix_parent ix cursor ->
          rec.data typeBinding constrs meta gamma metaGamma
            ix_parent
            -- definition
            ix
            (checkCursorHere cursor)
            -- typeBinding
            (ix :> DataDefinition_TypeBinding)
            (checkCursorStep DataDefinition_TypeBinding cursor)
            -- constructors
            (\i -> ix :> DataDefinition_Constructor i)
            (\i -> checkCursorStep (DataDefinition_Constructor i) cursor)
    }

type RecConstructor a
  = RecMetaContext.RecConstructor
      ( Index -> -- module/block
        Index -> -- definition
        Index -> -- constructor
        Cursor -> -- constructor
        a
      )

type RecConstructor_Constructor a
  = RecMetaContext.RecConstructor_Constructor
      ( Index -> -- module/block
        Index -> -- definition
        Index -> -- constructor
        Boolean -> -- constructor
        Index -> -- termBinding
        Cursor -> -- termBinding
        (Int -> Index) -> -- parameters
        (Int -> Cursor) -> -- parameters
        a
      )

-- registration already handled by recDefinitions
recConstructor ::
  forall a.
  { constructor :: RecConstructor_Constructor a } ->
  RecConstructor a
recConstructor rec =
  RecMetaContext.recConstructor
    { constructor:
        \termBinding prms meta typeId gamma metaGamma ix_parent ix_def ix cursor ->
          rec.constructor termBinding prms meta typeId gamma metaGamma
            ix_parent
            ix_def
            -- constructor
            ix
            (checkCursorHere cursor)
            -- termBinding
            (ix :> Constructor_TermBinding)
            (checkCursorStep Constructor_TermBinding cursor)
            -- parameters
            (\i -> ix :> Constructor_Parameter i)
            (\i -> checkCursorStep (Constructor_Parameter i) cursor)
    }

-- TODO: update+annotate order or arguments
type RecType a
  = RecMetaContext.RecType (Index -> Cursor -> a)

type RecType_Arrow a
  = RecMetaContext.RecType_Arrow
      ( Index -> -- type
        Boolean -> -- type
        Index -> -- parameter
        Cursor -> -- parameter
        Index -> -- type (sub)
        Cursor -> -- type (sub)
        a
      )

type RecType_Data a
  = RecMetaContext.RecType_Data
      ( Index -> -- type
        Boolean -> -- type
        a
      )

type RecType_Hole a
  = RecMetaContext.RecType_Hole
      ( Index -> -- type
        Boolean -> -- type
        a
      )

type RecType_ProxyHole a
  = RecMetaContext.RecType_ProxyHole
      ( Index -> -- type
        Boolean -> -- type
        a
      )

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
            -- type
            ix
            (checkCursorHere cursor)
            -- parameter
            (ix :> ArrowType_Parameter)
            (checkCursorStep ArrowType_Parameter cursor)
            -- type (sub)
            (ix :> ArrowType_Type)
            (checkCursorStep ArrowType_Type cursor)
    , data: \typeId meta gamma metaGamma ix cursor -> rec.data typeId meta gamma metaGamma ix (checkCursorHere cursor)
    , hole: \holeID wkn meta gamma metaGamma ix cursor -> rec.hole holeID wkn meta gamma metaGamma ix (checkCursorHere cursor)
    , proxyHole: \holeID gamma metaGamma ix cursor -> rec.proxyHole holeID gamma metaGamma ix (checkCursorHere cursor)
    }

-- TODO: reorder+annotate types
type RecTerm a
  = RecMetaContext.RecTerm (Index -> Cursor -> a)

type RecTerm_Lambda a
  = RecMetaContext.RecTerm_Lambda
      ( Index -> -- term
        Boolean -> -- term
        Index -> -- termId
        Cursor -> -- termId
        Index -> -- block
        Cursor -> -- block
        a
      )

type RecTerm_Neutral a
  = RecMetaContext.RecTerm_Neutral
      ( Index -> -- term
        Boolean -> -- term
        Index -> -- termId
        Cursor -> -- termId
        Index -> -- args
        Cursor -> -- args
        a
      )

type RecTerm_Match a
  = RecMetaContext.RecTerm_Match
      ( Index -> -- term
        Boolean -> -- term
        Index -> -- term (sub)
        Cursor -> -- term (sub)
        (Int -> Index) -> -- cases
        (Int -> Cursor) -> -- cases
        a
      )

type RecTerm_Hole a
  = RecMetaContext.RecTerm_Hole
      ( Index -> -- term
        Boolean -> -- term
        a
      )

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
            -- term
            ix
            (checkCursorHere cursor)
            -- termId
            (ix :> LambdaTerm_TermId)
            (checkCursorStep LambdaTerm_TermId cursor)
            -- block
            (ix :> LambdaTerm_Block)
            (checkCursorStep LambdaTerm_Block cursor)
    , neutral:
        \termId args meta gamma alpha metaGamma ix cursor ->
          rec.neutral termId args meta gamma alpha metaGamma
            -- term
            ix
            (checkCursorHere cursor)
            -- termId
            (ix :> NeutralTerm_TermId)
            (checkCursorStep NeutralTerm_TermId cursor)
            -- args
            (ix :> NeutralTerm_Args)
            (checkCursorStep NeutralTerm_Args cursor)
    , match:
        \typeId a cases meta gamma alpha metaGamma constrIDs ix cursor ->
          rec.match typeId a cases meta gamma alpha metaGamma constrIDs
            -- term
            ix
            (checkCursorHere cursor)
            -- term (sub)
            (ix :> MatchTerm_Term)
            (checkCursorStep MatchTerm_Term cursor)
            -- cases
            (\i -> ix :> MatchTerm_Case i)
            (\i -> checkCursorStep (MatchTerm_Case i) cursor)
    , hole:
        \meta gamma alpha metaGamma ix cursor ->
          rec.hole meta gamma alpha metaGamma ix (checkCursorHere cursor)
    }

-- TODO: reorder+annotate types
type RecArgs a
  = RecMetaContext.RecArgs (Index -> Cursor -> a)

type RecArgs_None (a :: Prim.Type)
  = RecMetaContext.RecArgs_None a

type RecArgs_Cons a
  = RecMetaContext.RecArgs_Cons
      ( Index -> -- args
        Boolean -> -- args
        Index -> -- term
        Cursor -> -- term
        Index -> -- args (sub)
        Cursor -> -- args (sub)
        a
      )

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
            -- args
            ix
            (checkCursorHere cursor)
            -- term
            (ix :> ConsArgs_Term)
            (checkCursorStep ConsArgs_Term cursor)
            -- args (sub)
            (ix :> ConsArgs_Args)
            (checkCursorStep ConsArgs_Args cursor)
    }

type RecCase a
  = RecMetaContext.RecCase
      ( Index -> -- match
        Index -> -- case
        Cursor -> -- case
        a
      )

type RecCase_Case a
  = RecMetaContext.RecCase_Case
      ( Index ->
        Index -> -- case
        Boolean -> -- case
        (Int -> Index) -> -- termId
        (Int -> Cursor) -> -- termId
        Index -> -- term
        Cursor -> -- term 
        a
      )

recCase ::
  forall a.
  { case_ :: RecCase_Case a } ->
  RecCase a
recCase rec =
  RecMetaContext.recCase
    { case_:
        \termIds a meta typeId constrId gamma metaGamma ix_match ix cursor ->
          rec.case_ termIds a meta typeId constrId gamma metaGamma
            -- match
            ix_match
            -- case 
            ix
            (checkCursorHere cursor)
            -- termId
            (\i -> ix :> Case_TermId i)
            (\i -> checkCursorStep (Case_TermId i) cursor)
            -- term
            (ix :> Case_Term)
            (checkCursorStep Case_Term cursor)
    }

type RecParameter a
  = RecMetaContext.RecParameter (Index -> Cursor -> a)

type RecParameter_Parameter a
  = RecMetaContext.RecParameter_Parameter
      ( Index -> -- parameter
        Boolean -> -- parameter
        Index -> -- type 
        Cursor -> -- type
        a
      )

recParameter ::
  forall a.
  { parameter :: RecParameter_Parameter a } ->
  RecParameter a
recParameter rec =
  RecMetaContext.recParameter
    { parameter:
        \alpha meta gamma metaGamma ix cursor ->
          rec.parameter alpha meta gamma metaGamma
            -- parameter
            ix
            (checkCursorHere cursor)
            -- type
            (ix :> Parameter_Type)
            (checkCursorStep Parameter_Type cursor)
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
