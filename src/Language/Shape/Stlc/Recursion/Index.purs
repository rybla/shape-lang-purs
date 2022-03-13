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
checkCursorStep step csr = do
  ix <- csr
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
        \defs meta gamma metaGamma ix csr ->
          rec.module_ defs meta gamma metaGamma
            -- module
            ix
            (checkCursorHere csr)
            -- definitions
            (\i -> ix :> Module_Definition i)
            (\i -> checkCursorStep (Module_Definition i) csr)
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
        \defs a meta gamma alpha metaGamma ix csr ->
          rec.block defs a meta gamma alpha metaGamma
            -- block
            ix
            (checkCursorHere csr)
            -- definitions
            (\i -> ix :> Block_Definition i)
            (\i -> checkCursorStep (Block_Definition i) csr)
            -- term
            (ix :> Block_Term)
            (checkCursorStep Block_Term csr)
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
        Index -> -- typeBinding
        Cursor -> -- typeBinding
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
        \termBinding alpha a meta gamma metaGamma ix_parent ix csr ->
          rec.term termBinding alpha a meta gamma metaGamma
            ix_parent
            -- definition
            ix
            (checkCursorHere csr)
            -- termBinding
            (ix :> TermDefinition_TermBinding)
            (checkCursorStep TermDefinition_TermBinding csr)
            -- type
            (ix :> TermDefinition_Type)
            (checkCursorStep TermDefinition_Type csr)
            -- term
            (ix :> TermDefinition_Term)
            (checkCursorStep TermDefinition_Term csr)
    , data:
        \typeBinding constrs meta gamma metaGamma ix_parent ix csr ->
          rec.data typeBinding constrs meta gamma metaGamma
            ix_parent
            -- definition
            ix
            (checkCursorHere csr)
            -- typeBinding
            (ix :> DataDefinition_TypeBinding)
            (checkCursorStep DataDefinition_TypeBinding csr)
            -- constructors
            (\i -> ix :> DataDefinition_Constructor i)
            (\i -> checkCursorStep (DataDefinition_Constructor i) csr)
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
        \termBinding prms meta typeId gamma metaGamma ix_parent ix_def ix csr ->
          rec.constructor termBinding prms meta typeId gamma metaGamma
            ix_parent
            ix_def
            -- constructor
            ix
            (checkCursorHere csr)
            -- termBinding
            (ix :> Constructor_TermBinding)
            (checkCursorStep Constructor_TermBinding csr)
            -- parameters
            (\i -> ix :> Constructor_Parameter i)
            (\i -> checkCursorStep (Constructor_Parameter i) csr)
    }

type RecDefinitionBindings a
  = RecMetaContext.RecDefinitionBindings
      ( Index -> -- definition
        Index -> -- type
        Cursor -> -- type 
        Index -> -- term
        Cursor -> -- term
        a
      )

type RecDefinitionBindings_ArrowLambda a
  = RecMetaContext.RecDefinitionBindings_ArrowLambda
      ( Index -> -- definition
        Index -> -- type
        Boolean -> -- type
        Index -> -- term
        Boolean -> -- term
        Index -> -- parameter
        Cursor -> -- parameter
        Index -> -- type (sub)
        Cursor -> -- type (sub)
        Index -> -- termId
        Cursor -> -- termId
        Index -> -- block
        Cursor -> -- block
        a
      )

type RecDefinitionBindings_Wildcard a
  = RecMetaContext.RecDefinitionBindings_Wildcard
      ( Index ->
        Index ->
        Boolean ->
        Index ->
        Boolean -> a
      )

-- TODO: if necessary
{-
recDefinitionBindings ::
  forall a.
  { arrow_lambda :: RecDefinitionBindings_ArrowLambda a
  , wildcard :: RecDefinitionBindings_Wildcard a
  } ->
  RecDefinitionBindings a
recDefinitionBindings rec =
  RecMetaContext.recDefinitionBindings
    { arrow_lambda:
        \prm beta termId block meta gamma metaGamma ix_def ix_type csr_type ix_term csr_term ->
          rec.arrow_lambda prm beta termId block meta gamma metaGamma
            -- def
            ix_def
            -- type
            ix_type
            csr_type
            -- term
            ix_term
            csr_term
            -- prm
            (ix_type :> ArrowType_Parameter)
            (checkCursorStep ArrowType_Parameter ?csr_type)
            -- beta
            (ix_type :> ArrowType_Type)
            (checkCursorStep ArrowType_Type ?csr_type)
            -- termId
            (ix_term :> LambdaTerm_TermId)
            (checkCursorStep LambdaTerm_TermId ?csr_term)
            -- block
            (ix_term :> LambdaTerm_Block)
            (checkCursorStep LambdaTerm_Block ?csr_term)
    , wildcard:
        \alpha a gamma metaGamma ix_def ix_alpha csr_alpha ix_a csr_a ->
          rec.wildcard alpha a gamma metaGamma
            -- def
            ix_def
            -- alpha
            ix_alpha
            (checkCursorHere csr_alpha)
            -- a 
            ix_a
            (checkCursorHere csr_a)
    }
-}
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
        \prm beta meta gamma metaGamma ix csr ->
          rec.arrow prm beta meta gamma metaGamma
            -- type
            ix
            (checkCursorHere csr)
            -- prm
            (ix :> ArrowType_Parameter)
            (checkCursorStep ArrowType_Parameter csr)
            -- beta
            (ix :> ArrowType_Type)
            (checkCursorStep ArrowType_Type csr)
    , data: \typeId meta gamma metaGamma ix csr -> rec.data typeId meta gamma metaGamma ix (checkCursorHere csr)
    , hole: \holeID wkn meta gamma metaGamma ix csr -> rec.hole holeID wkn meta gamma metaGamma ix (checkCursorHere csr)
    , proxyHole: \holeID gamma metaGamma ix csr -> rec.proxyHole holeID gamma metaGamma ix (checkCursorHere csr)
    }

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
        \termId block meta gamma prm beta metaGamma ix csr ->
          rec.lambda termId block meta gamma prm beta metaGamma
            -- term
            ix
            (checkCursorHere csr)
            -- termId
            (ix :> LambdaTerm_TermId)
            (checkCursorStep LambdaTerm_TermId csr)
            -- block
            (ix :> LambdaTerm_Block)
            (checkCursorStep LambdaTerm_Block csr)
    , neutral:
        \termId args meta gamma alpha metaGamma ix csr ->
          rec.neutral termId args meta gamma alpha metaGamma
            -- term
            ix
            (checkCursorHere csr)
            -- termId
            (ix :> NeutralTerm_TermId)
            (checkCursorStep NeutralTerm_TermId csr)
            -- args
            (ix :> NeutralTerm_Args)
            (checkCursorStep NeutralTerm_Args csr)
    , match:
        \typeId a cases meta gamma alpha metaGamma constrIDs ix csr ->
          rec.match typeId a cases meta gamma alpha metaGamma constrIDs
            -- term
            ix
            (checkCursorHere csr)
            -- term (sub)
            (ix :> MatchTerm_Term)
            (checkCursorStep MatchTerm_Term csr)
            -- cases
            (\i -> ix :> MatchTerm_Case i)
            (\i -> checkCursorStep (MatchTerm_Case i) csr)
    , hole:
        \meta gamma alpha metaGamma ix csr ->
          rec.hole meta gamma alpha metaGamma ix (checkCursorHere csr)
    }

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
        \a args meta gamma prm beta metaGamma ix csr ->
          rec.cons a args meta gamma prm beta metaGamma
            -- args
            ix
            (checkCursorHere csr)
            -- term
            (ix :> ConsArgs_Term)
            (checkCursorStep ConsArgs_Term csr)
            -- args (sub)
            (ix :> ConsArgs_Args)
            (checkCursorStep ConsArgs_Args csr)
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
      ( Index -> -- match
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
        \termIds a meta typeId constrId gamma alpha metaGamma ix_match ix csr ->
          rec.case_ termIds a meta typeId constrId gamma alpha metaGamma
            -- match
            ix_match
            -- case 
            ix
            (checkCursorHere csr)
            -- termId
            (\i -> ix :> Case_TermId i)
            (\i -> checkCursorStep (Case_TermId i) csr)
            -- term
            (ix :> Case_Term)
            (checkCursorStep Case_Term csr)
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
        \alpha meta gamma metaGamma ix csr ->
          rec.parameter alpha meta gamma metaGamma
            -- parameter
            ix
            (checkCursorHere csr)
            -- type
            (ix :> Parameter_Type)
            (checkCursorStep Parameter_Type csr)
    }

type RecTypeBinding a
  = TypeBinding -> Context -> MetaContext -> Index -> Cursor -> a

type RecTypeBinding_TypeBinding a
  = TypeId -> TypeBindingMetadata -> Context -> MetaContext -> Index -> Boolean -> a

recTypeBinding ::
  forall a.
  { typeBinding :: RecTypeBinding_TypeBinding a
  } ->
  RecTypeBinding a
recTypeBinding rec (TypeBinding typeId meta) gamma metaGamma ix csr = rec.typeBinding typeId meta gamma metaGamma ix (checkCursorHere csr)

type RecTermBinding a
  = TermBinding -> Context -> MetaContext -> Index -> Cursor -> a

type RecTermBinding_TermBinding a
  = TermId -> TermBindingMetadata -> Context -> MetaContext -> Index -> Boolean -> a

recTermBinding ::
  forall a.
  { termBinding :: RecTermBinding_TermBinding a
  } ->
  RecTermBinding a
recTermBinding rec (TermBinding termId meta) gamma metaGamma ix csr = rec.termBinding termId meta gamma metaGamma ix (checkCursorHere csr)

type RecTermId a
  = TermId -> Context -> MetaContext -> Index -> Cursor -> a

type RecTermId_TermId a
  = TermId -> Context -> MetaContext -> Index -> Boolean -> a

recTermId :: forall a. { termId :: RecTermId_TermId a } -> RecTermId a
recTermId rec termId gamma metaGamma ix csr = rec.termId termId gamma metaGamma ix (checkCursorHere csr)
