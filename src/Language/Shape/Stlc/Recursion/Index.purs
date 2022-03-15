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
  = Maybe DownwardIndex

-- check to see if the next step of the recursion "downward" corresponds to the next downward step of the cursor
checkCursorStep :: IndexStep -> Cursor -> Cursor
checkCursorStep step' csr = do
  ix <- csr
  { step, ix' } <- unconsDownwardIndex ix
  if step == step' then
    Just ix'
  else
    Nothing

checkCursorHere :: Cursor -> Boolean
checkCursorHere = case _ of
  Nothing -> false
  Just (DownwardIndex steps) -> List.null steps

-- Recursion principles for handling indexing
type RecModule a
  = RecMetaContext.RecModule (UpwardIndex -> Cursor -> a)

type RecModule_Module a
  = RecMetaContext.RecModule_Module
      ( UpwardIndex -> -- module
        Boolean -> -- module
        (Int -> UpwardIndex) -> -- definition
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
            (\i -> Module_Definition i <: ix)
            (\i -> checkCursorStep (Module_Definition i) csr)
    }

type RecBlock a
  = RecMetaContext.RecBlock (UpwardIndex -> Cursor -> a)

type RecBlock_Block a
  = RecMetaContext.RecBlock_Block
      ( UpwardIndex -> -- block
        Boolean -> -- block
        (Int -> UpwardIndex) -> -- definition
        (Int -> Cursor) -> -- definition
        UpwardIndex -> -- term
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
            (\i -> Block_Definition i <: ix)
            (\i -> checkCursorStep (Block_Definition i) csr)
            -- term
            (Block_Term <: ix)
            (checkCursorStep Block_Term csr)
    }

type RecDefinitions a
  = RecMetaContext.RecDefinitions
      ( UpwardIndex -> -- module/block
        (Int -> UpwardIndex) -> -- definition
        (Int -> Cursor) -> -- definition
        a
      )

type RecDefinitions_Definitions a
  = RecMetaContext.RecDefinitions_Definitions
      ( UpwardIndex -> -- module/block
        (Int -> UpwardIndex) -> -- definition
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
      ( UpwardIndex -> -- module/block
        UpwardIndex -> -- definition
        Cursor -> -- definition
        a
      )

type RecDefinition_TermDefinition a
  = RecMetaContext.RecDefinition_TermDefinition
      ( UpwardIndex -> -- module/block
        UpwardIndex -> -- definition
        Boolean -> -- definition
        UpwardIndex -> -- termId
        Cursor -> -- termId
        UpwardIndex -> -- type
        Cursor -> -- type
        UpwardIndex -> -- term
        Cursor -> -- term
        a
      )

type RecDefinition_DataDefinition a
  = RecMetaContext.RecDefinition_DataDefinition
      ( UpwardIndex -> -- module/block
        UpwardIndex -> -- definition
        Boolean -> -- definition
        UpwardIndex -> -- typeBinding
        Cursor -> -- typeBinding
        (Int -> UpwardIndex) -> -- constructors
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
            (TermDefinition_TermBinding <: ix)
            (checkCursorStep TermDefinition_TermBinding csr)
            -- type
            (TermDefinition_Type <: ix)
            (checkCursorStep TermDefinition_Type csr)
            -- term
            (TermDefinition_Term <: ix)
            (checkCursorStep TermDefinition_Term csr)
    , data:
        \typeBinding constrs meta gamma metaGamma ix_parent ix csr ->
          rec.data typeBinding constrs meta gamma metaGamma
            ix_parent
            -- definition
            ix
            (checkCursorHere csr)
            -- typeBinding
            (DataDefinition_TypeBinding <: ix)
            (checkCursorStep DataDefinition_TypeBinding csr)
            -- constructors
            (\i -> DataDefinition_Constructor i <: ix)
            (\i -> checkCursorStep (DataDefinition_Constructor i) csr)
    }

type RecConstructor a
  = RecMetaContext.RecConstructor
      ( UpwardIndex -> -- module/block
        UpwardIndex -> -- definition
        UpwardIndex -> -- constructor
        Cursor -> -- constructor
        a
      )

type RecConstructor_Constructor a
  = RecMetaContext.RecConstructor_Constructor
      ( UpwardIndex -> -- module/block
        UpwardIndex -> -- definition
        UpwardIndex -> -- constructor
        Boolean -> -- constructor
        UpwardIndex -> -- termBinding
        Cursor -> -- termBinding
        (Int -> UpwardIndex) -> -- parameters
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
        \termBinding prms meta typeId gamma alpha metaGamma ix_parent ix_def ix csr ->
          rec.constructor termBinding prms meta typeId gamma alpha metaGamma
            ix_parent
            ix_def
            -- constructor
            ix
            (checkCursorHere csr)
            -- termBinding
            (Constructor_TermBinding <: ix)
            (checkCursorStep Constructor_TermBinding csr)
            -- parameters
            (\i -> Constructor_Parameter i <: ix)
            (\i -> checkCursorStep (Constructor_Parameter i) csr)
    }

type RecDefinitionBindings a
  = RecMetaContext.RecDefinitionBindings
      ( UpwardIndex -> -- definition
        UpwardIndex -> -- type
        Cursor -> -- type 
        UpwardIndex -> -- term
        Cursor -> -- term
        a
      )

type RecDefinitionBindings_ArrowLambda a
  = RecMetaContext.RecDefinitionBindings_ArrowLambda
      ( UpwardIndex -> -- definition
        UpwardIndex -> -- type
        Boolean -> -- type
        UpwardIndex -> -- term
        Boolean -> -- term
        UpwardIndex -> -- parameter
        Cursor -> -- parameter
        UpwardIndex -> -- type (sub)
        Cursor -> -- type (sub)
        UpwardIndex -> -- termId
        Cursor -> -- termId
        UpwardIndex -> -- block
        Cursor -> -- block
        a
      )

type RecDefinitionBindings_Wildcard a
  = RecMetaContext.RecDefinitionBindings_Wildcard
      ( UpwardIndex ->
        UpwardIndex ->
        Boolean ->
        UpwardIndex ->
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
  = RecMetaContext.RecType (UpwardIndex -> Cursor -> a)

type RecType_Arrow a
  = RecMetaContext.RecType_Arrow
      ( UpwardIndex -> -- type
        Boolean -> -- type
        UpwardIndex -> -- parameter
        Cursor -> -- parameter
        UpwardIndex -> -- type (sub)
        Cursor -> -- type (sub)
        a
      )

type RecType_Data a
  = RecMetaContext.RecType_Data
      ( UpwardIndex -> -- type
        Boolean -> -- type
        a
      )

type RecType_Hole a
  = RecMetaContext.RecType_Hole
      ( UpwardIndex -> -- type
        Boolean -> -- type
        a
      )

type RecType_ProxyHole a
  = RecMetaContext.RecType_ProxyHole
      ( UpwardIndex -> -- type
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
            (ArrowType_Parameter <: ix)
            (checkCursorStep ArrowType_Parameter csr)
            -- beta
            (ArrowType_Type <: ix)
            (checkCursorStep ArrowType_Type csr)
    , data: \typeId meta gamma metaGamma ix csr -> rec.data typeId meta gamma metaGamma ix (checkCursorHere csr)
    , hole: \holeID wkn meta gamma metaGamma ix csr -> rec.hole holeID wkn meta gamma metaGamma ix (checkCursorHere csr)
    , proxyHole: \holeID gamma metaGamma ix csr -> rec.proxyHole holeID gamma metaGamma ix (checkCursorHere csr)
    }

type RecTerm a
  = RecMetaContext.RecTerm (UpwardIndex -> Cursor -> a)

type RecTerm_Lambda a
  = RecMetaContext.RecTerm_Lambda
      ( UpwardIndex -> -- term
        Boolean -> -- term
        UpwardIndex -> -- termId
        Cursor -> -- termId
        UpwardIndex -> -- block
        Cursor -> -- block
        a
      )

type RecTerm_Neutral a
  = RecMetaContext.RecTerm_Neutral
      ( UpwardIndex -> -- term
        Boolean -> -- term
        UpwardIndex -> -- termId
        Cursor -> -- termId
        UpwardIndex -> -- args
        Cursor -> -- args
        a
      )

type RecTerm_Match a
  = RecMetaContext.RecTerm_Match
      ( UpwardIndex -> -- term
        Boolean -> -- term
        UpwardIndex -> -- term (sub)
        Cursor -> -- term (sub)
        (Int -> UpwardIndex) -> -- cases
        (Int -> Cursor) -> -- cases
        a
      )

type RecTerm_Hole a
  = RecMetaContext.RecTerm_Hole
      ( UpwardIndex -> -- term
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
            (LambdaTerm_TermId <: ix)
            (checkCursorStep LambdaTerm_TermId csr)
            -- block
            (LambdaTerm_Block <: ix)
            (checkCursorStep LambdaTerm_Block csr)
    , neutral:
        \termId args meta gamma alpha metaGamma ix csr ->
          rec.neutral termId args meta gamma alpha metaGamma
            -- term
            ix
            (checkCursorHere csr)
            -- termId
            (NeutralTerm_TermId <: ix)
            (checkCursorStep NeutralTerm_TermId csr)
            -- args
            (NeutralTerm_Args <: ix)
            (checkCursorStep NeutralTerm_Args csr)
    , match:
        \typeId a cases meta gamma alpha metaGamma constrIDs ix csr ->
          rec.match typeId a cases meta gamma alpha metaGamma constrIDs
            -- term
            ix
            (checkCursorHere csr)
            -- term (sub)
            (MatchTerm_Term <: ix)
            (checkCursorStep MatchTerm_Term csr)
            -- cases
            (\i -> MatchTerm_Case i <: ix)
            (\i -> checkCursorStep (MatchTerm_Case i) csr)
    , hole:
        \meta gamma alpha metaGamma ix csr ->
          rec.hole meta gamma alpha metaGamma ix (checkCursorHere csr)
    }

type RecArgs a
  = RecMetaContext.RecArgs (UpwardIndex -> Cursor -> a)

type RecArgs_None (a :: Prim.Type)
  = RecMetaContext.RecArgs_None a

type RecArgs_Cons a
  = RecMetaContext.RecArgs_Cons
      ( UpwardIndex -> -- args
        Boolean -> -- args
        UpwardIndex -> -- term
        Cursor -> -- term
        UpwardIndex -> -- args (sub)
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
            (ConsArgs_Term <: ix)
            (checkCursorStep ConsArgs_Term csr)
            -- args (sub)
            (ConsArgs_Args <: ix)
            (checkCursorStep ConsArgs_Args csr)
    }

type RecCase a
  = RecMetaContext.RecCase
      ( UpwardIndex -> -- match
        UpwardIndex -> -- case
        Cursor -> -- case
        a
      )

type RecCase_Case a
  = RecMetaContext.RecCase_Case
      ( UpwardIndex -> -- match
        UpwardIndex -> -- case
        Boolean -> -- case
        (Int -> UpwardIndex) -> -- termId
        (Int -> Cursor) -> -- termId
        UpwardIndex -> -- term
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
            (\i -> Case_TermId i <: ix)
            (\i -> checkCursorStep (Case_TermId i) csr)
            -- term
            (Case_Term <: ix)
            (checkCursorStep Case_Term csr)
    }

type RecParameter a
  = RecMetaContext.RecParameter (UpwardIndex -> Cursor -> a)

type RecParameter_Parameter a
  = RecMetaContext.RecParameter_Parameter
      ( UpwardIndex -> -- parameter
        Boolean -> -- parameter
        UpwardIndex -> -- type 
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
            (Parameter_Type <: ix)
            (checkCursorStep Parameter_Type csr)
    }

type RecTypeBinding a
  = TypeBinding -> Context -> MetaContext -> UpwardIndex -> Cursor -> a

type RecTypeBinding_TypeBinding a
  = TypeId -> TypeBindingMetadata -> Context -> MetaContext -> UpwardIndex -> Boolean -> a

recTypeBinding ::
  forall a.
  { typeBinding :: RecTypeBinding_TypeBinding a
  } ->
  RecTypeBinding a
recTypeBinding rec (TypeBinding typeId meta) gamma metaGamma ix csr = rec.typeBinding typeId meta gamma metaGamma ix (checkCursorHere csr)

type RecTermBinding a
  = TermBinding -> Context -> MetaContext -> UpwardIndex -> Cursor -> a

type RecTermBinding_TermBinding a
  = TermId -> TermBindingMetadata -> Context -> MetaContext -> UpwardIndex -> Boolean -> a

recTermBinding ::
  forall a.
  { termBinding :: RecTermBinding_TermBinding a
  } ->
  RecTermBinding a
recTermBinding rec (TermBinding termId meta) gamma metaGamma ix csr = rec.termBinding termId meta gamma metaGamma ix (checkCursorHere csr)

type RecTermId a
  = TermId -> Context -> MetaContext -> UpwardIndex -> Cursor -> a

type RecTermId_TermId a
  = TermId -> Context -> MetaContext -> UpwardIndex -> Boolean -> a

recTermId :: forall a. { termId :: RecTermId_TermId a } -> RecTermId a
recTermId rec termId gamma metaGamma ix csr = rec.termId termId gamma metaGamma ix (checkCursorHere csr)
