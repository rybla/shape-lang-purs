module Language.Shape.Stlc.Recursion.Index where

import Data.Either
import Data.List.Unsafe
import Data.Maybe
import Data.Tuple.Nested
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Typing
import Prelude
import Prim hiding (Type)
import Control.Monad.State (State, runState)
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Debug as Debug
import Language.Shape.Stlc.Changes as Ch
import Language.Shape.Stlc.Holes (HoleSub)
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

checkCursorSteps :: DownwardIndex -> Cursor -> Cursor
checkCursorSteps ix csr = case unconsDownwardIndex ix of
  Just { step, ix' } -> checkCursorSteps ix' $ checkCursorStep step csr
  Nothing -> csr

checkCursorHere :: Cursor -> Boolean
checkCursorHere = case _ of
  Nothing -> false
  Just (DownwardIndex steps) -> null steps

-- Recursion principles for handling indexing
type RecModule a
  = RecMetaContext.RecModule (UpwardIndex -> Cursor -> a)

type RecModule_Module a
  = RecMetaContext.RecModule_Module
      ( UpwardIndex -> -- module
        Boolean -> -- module
        -- (Int -> UpwardIndex) -> -- definition
        -- (Int -> Cursor) -> -- definition
        UpwardIndex -> -- definitionItems
        Cursor -> -- definitionItems
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
            -- definitionItems
            -- (\i -> ix <> UpwardIndex (singleton (IndexStep StepModule 0)) <> toUpwardIndex (fromListIndexToDownwardIndex i))
            -- (\i -> checkCursorSteps (toDownwardIndex $ UpwardIndex (singleton (IndexStep StepModule 0)) <> toUpwardIndex (fromListIndexToDownwardIndex i)) csr)
            (ix :- IndexStep StepModule 0)
            (checkCursorStep (IndexStep StepModule 0) csr)
    }

type RecBlock a
  = RecMetaContext.RecBlock (UpwardIndex -> Cursor -> a)

type RecBlock_Block a
  = RecMetaContext.RecBlock_Block
      ( UpwardIndex -> -- block
        Boolean -> -- block
        -- (Int -> UpwardIndex) -> -- definition
        -- (Int -> Cursor) -> -- definition
        UpwardIndex -> -- definitionItems
        Cursor -> -- definitionItems
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
            -- definitionItems
            -- (\i -> ix <> UpwardIndex (singleton (IndexStep StepBlock 0)) <> toUpwardIndex (fromListIndexToDownwardIndex i))
            -- (\i -> checkCursorSteps (toDownwardIndex $ UpwardIndex (singleton (IndexStep StepBlock 0)) <> toUpwardIndex (fromListIndexToDownwardIndex i)) csr)
            (ix :- IndexStep StepBlock 0)
            (checkCursorStep (IndexStep StepBlock 0) csr)
            -- term
            (ix :- IndexStep StepBlock 1)
            (checkCursorStep (IndexStep StepBlock 1) csr)
    }

type RecDefinitionItems a
  = RecMetaContext.RecDefinitionItems
      ( UpwardIndex -> -- module/block
        UpwardIndex -> Cursor -> a
      )

type RecDefinitionItems_DefinitionItems a
  = RecMetaContext.RecDefinitionItems_DefinitionItems
      ( UpwardIndex -> -- module/block
        UpwardIndex -> -- definitionItems
        Boolean -> -- definitionItems
        (Int -> UpwardIndex) -> -- definition
        (Int -> Cursor) -> -- definition
        a
      )

recDefinitionItems ::
  forall a.
  { definitionItems :: RecDefinitionItems_DefinitionItems a } ->
  RecDefinitionItems a
recDefinitionItems rec =
  RecMetaContext.recDefinitionItems
    { definitionItems:
        \defItems gamma metaGamma ix_parent ix csr ->
          rec.definitionItems defItems gamma metaGamma
            -- module/block
            ix_parent
            -- definitionItems
            ix
            (checkCursorHere csr)
            -- defItem
            (\i -> toUpwardIndex (fromListIndexToDownwardIndex i) <> ix)
            (\i -> checkCursorSteps (fromListIndexToDownwardIndex i) csr)
    }

--  RecMetaContext.recDefinitionItems
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
            -- module/block
            ix_parent
            -- definition
            ix
            (checkCursorHere csr)
            -- termBinding
            (ix :- IndexStep StepTermDefinition 0)
            (checkCursorStep (IndexStep StepTermDefinition 0) csr)
            -- type
            (ix :- IndexStep StepTermDefinition 1)
            (checkCursorStep (IndexStep StepTermDefinition 1) csr)
            -- term
            (ix :- IndexStep StepTermDefinition 2)
            (checkCursorStep (IndexStep StepTermDefinition 2) csr)
    , data:
        \typeBinding constrs meta gamma metaGamma ix_parent ix csr ->
          rec.data typeBinding constrs meta gamma metaGamma
            -- module/block
            ix_parent
            -- definition
            ix
            (checkCursorHere csr)
            -- typeBinding
            (ix :- IndexStep StepDataDefinition 0)
            (checkCursorStep (IndexStep StepDataDefinition 0) csr)
            -- constructors
            (\i -> ix <> UpwardIndex (singleton (IndexStep StepDataDefinition 1)) <> toUpwardIndex (fromListIndexToDownwardIndex i))
            (\i -> checkCursorSteps (toDownwardIndex $ UpwardIndex (singleton (IndexStep StepDataDefinition 1)) <> toUpwardIndex (fromListIndexToDownwardIndex i)) csr)
    }

type RecConstructor a
  = RecMetaContext.RecConstructor
      ( UpwardIndex -> -- definitionItems
        UpwardIndex -> -- definition
        UpwardIndex -> -- constructor
        Cursor -> -- constructor
        a
      )

type RecConstructor_Constructor a
  = RecMetaContext.RecConstructor_Constructor
      ( UpwardIndex -> -- definitionItems
        UpwardIndex -> -- definition
        UpwardIndex -> -- constructor
        Boolean -> -- constructor
        UpwardIndex -> -- termBinding
        Cursor -> -- termBinding
        (Int -> UpwardIndex) -> -- parameters
        (Int -> Cursor) -> -- parameters
        a
      )

-- registration already handled by recDefinitionItems
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
            (ix :- IndexStep StepConstructor 0)
            (checkCursorStep (IndexStep StepConstructor 0) csr)
            -- parameters
            (\i -> ix <> UpwardIndex (singleton (IndexStep StepConstructor 1)) <> toUpwardIndex (fromListIndexToDownwardIndex i))
            (\i -> checkCursorSteps (toDownwardIndex $ UpwardIndex (singleton (IndexStep StepConstructor 1)) <> toUpwardIndex (fromListIndexToDownwardIndex i)) csr)
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
            (ix_type :- ArrowType_Parameter)
            (checkCursorStep ArrowType_Parameter ?csr_type)
            -- beta
            (ix_type :- ArrowType_Type)
            (checkCursorStep ArrowType_Type ?csr_type)
            -- termId
            (ix_term :- LambdaTerm_TermId)
            (checkCursorStep LambdaTerm_TermId ?csr_term)
            -- block
            (ix_term :- LambdaTerm_Block)
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
            (ix :- IndexStep StepArrowType 0)
            (checkCursorStep (IndexStep StepArrowType 0) csr)
            -- beta
            (ix :- IndexStep StepArrowType 1)
            (checkCursorStep (IndexStep StepArrowType 1) csr)
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
        UpwardIndex -> -- argItems
        Cursor -> -- argItems
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
            (ix :- IndexStep StepLambdaTerm 0)
            (checkCursorStep (IndexStep StepLambdaTerm 0) csr)
            -- block
            (ix :- IndexStep StepLambdaTerm 1)
            (checkCursorStep (IndexStep StepLambdaTerm 1) csr)
    , neutral:
        \termId argItems meta gamma alpha metaGamma ix csr ->
          rec.neutral termId argItems meta gamma alpha metaGamma
            -- term
            ix
            (checkCursorHere csr)
            -- termId
            (ix :- IndexStep StepNeutralTerm 0)
            (checkCursorStep (IndexStep StepNeutralTerm 0) csr)
            -- argItems
            (ix :- IndexStep StepNeutralTerm 1)
            (checkCursorStep (IndexStep StepNeutralTerm 1) csr)
    , match:
        \typeId a cases meta gamma alpha metaGamma constrIDs ix csr ->
          rec.match typeId a cases meta gamma alpha metaGamma constrIDs
            -- term
            ix
            (checkCursorHere csr)
            -- term (sub)
            (ix :- IndexStep StepMatchTerm 0)
            (checkCursorStep (IndexStep StepMatchTerm 0) csr)
            -- cases
            (\i -> ix <> UpwardIndex (singleton (IndexStep StepMatchTerm 1)) <> toUpwardIndex (fromListIndexToDownwardIndex i))
            (\i -> checkCursorSteps (toDownwardIndex $ UpwardIndex (singleton (IndexStep StepMatchTerm 1)) <> toUpwardIndex (fromListIndexToDownwardIndex i)) csr)
    , hole:
        \meta gamma alpha metaGamma ix csr ->
          rec.hole meta gamma alpha metaGamma ix (checkCursorHere csr)
    }

type RecArgItems a
  = RecMetaContext.RecArgItems (UpwardIndex -> Cursor -> a)

type RecArgItems_Nil (a :: Prim.Type)
  = RecMetaContext.RecArgItems_Nil a

type RecArgItems_Cons a
  = RecMetaContext.RecArgItems_Cons
      ( UpwardIndex -> -- argItems
        Boolean -> -- argItems
        UpwardIndex -> -- term
        Cursor -> -- term
        UpwardIndex -> -- argItems (sub)
        Cursor -> -- argItems (sub)
        a
      )

recArgItems ::
  forall a.
  { nil :: RecArgItems_Nil a
  , cons :: RecArgItems_Cons a
  } ->
  RecArgItems a
recArgItems rec =
  RecMetaContext.recArgItems
    { nil: \gamma alpha metaGamma ix csr -> rec.nil gamma alpha metaGamma
    , cons:
        \argItem argItems gamma prm beta metaGamma ix csr ->
          rec.cons argItem argItems gamma prm beta metaGamma
            -- argItems
            ix
            (checkCursorHere csr)
            -- term
            (ix :- IndexStep StepCons 0)
            (checkCursorStep (IndexStep StepCons 0) csr)
            -- argItems (sub)
            (ix :- IndexStep StepCons 1)
            (checkCursorStep (IndexStep StepCons 1) csr)
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
            -- termIdItems
            (\i -> ix <> UpwardIndex (singleton (IndexStep StepCase 0)) <> toUpwardIndex (fromListIndexToDownwardIndex i))
            (\i -> checkCursorSteps (toDownwardIndex $ UpwardIndex (singleton (IndexStep StepCase 0)) <> toUpwardIndex (fromListIndexToDownwardIndex i)) csr)
            -- term
            (ix :- IndexStep StepCase 1)
            (checkCursorStep (IndexStep StepCase 1) csr)
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
            (ix :- IndexStep StepParameter 0)
            (checkCursorStep (IndexStep StepParameter 0) csr)
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
