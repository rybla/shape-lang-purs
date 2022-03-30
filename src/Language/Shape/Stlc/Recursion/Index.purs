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
  = RecMetaContext.RecModule ({ ix :: UpwardIndex, csr :: Cursor } -> a)

type RecModule_Module a
  = RecMetaContext.RecModule_Module
      ( { ix :: UpwardIndex -- module
        , isSelected :: Boolean -- module
        , ix_defItems :: UpwardIndex -- definitionItems
        , csr_defItems :: Cursor -- definitionItems
        } ->
        a
      )

recModule ::
  forall a.
  { module_ :: RecModule_Module a } ->
  RecModule a
recModule rec =
  RecMetaContext.recModule
    { module_:
        \defs meta gamma metaGamma { ix, csr } ->
          rec.module_ defs meta gamma metaGamma
            { ix
            , isSelected: checkCursorHere csr
            , ix_defItems: ix :- IndexStep StepModule 0
            , csr_defItems: checkCursorStep (IndexStep StepModule 0) csr
            }
    }

type RecBlock a
  = RecMetaContext.RecBlock ({ ix :: UpwardIndex, csr :: Cursor } -> a)

type RecBlock_Block a
  = RecMetaContext.RecBlock_Block
      ( { ix :: UpwardIndex
        , isSelected :: Boolean
        , ix_defItems :: UpwardIndex
        , csr_defItems :: Cursor
        , ix_term :: UpwardIndex
        , csr_term :: Cursor
        } ->
        a
      )

recBlock ::
  forall a.
  { block :: RecBlock_Block a } ->
  RecBlock a
recBlock rec =
  RecMetaContext.recBlock
    { block:
        \defs a meta gamma alpha metaGamma { ix, csr } ->
          rec.block defs a meta gamma alpha metaGamma
            { ix
            , isSelected: checkCursorHere csr
            , ix_defItems: ix :- IndexStep StepBlock 0
            , csr_defItems: checkCursorStep (IndexStep StepBlock 0) csr
            , ix_term: ix :- IndexStep StepBlock 1
            , csr_term: checkCursorStep (IndexStep StepBlock 1) csr
            }
    }

type RecDefinitionItems a
  = RecMetaContext.RecDefinitionItems
      ( { ix_parentBlock :: UpwardIndex
        , ix :: UpwardIndex
        , csr :: Cursor
        } ->
        a
      )

type RecDefinitionItems_DefinitionItems a
  = RecMetaContext.RecDefinitionItems_DefinitionItems
      ( { ix_parentBlock :: UpwardIndex
        , ix :: UpwardIndex
        , isSelected :: Boolean
        , ix_def_at :: Int -> UpwardIndex
        , csr_def_at :: Int -> Cursor
        , ix_defSep_at :: Int -> UpwardIndex
        , csr_defSep_at :: Int -> Cursor
        } ->
        a
      )

recDefinitionItems ::
  forall a.
  { definitionItems :: RecDefinitionItems_DefinitionItems a } ->
  RecDefinitionItems a
recDefinitionItems rec =
  RecMetaContext.recDefinitionItems
    { definitionItems:
        \defItems gamma metaGamma { ix_parentBlock, ix, csr } ->
          rec.definitionItems defItems gamma metaGamma
            { ix_parentBlock
            , ix
            , isSelected: checkCursorHere csr
            , ix_def_at: \i -> ix <> (fromListIndexToUpwardIndex i <> singletonUpwardIndex (IndexStep StepDefinitionItem 0))
            , csr_def_at: \i -> checkCursorSteps (fromListIndexToDownwardIndex i <> singletonDownwardIndex (IndexStep StepDefinitionItem 0)) csr
            , ix_defSep_at: \i -> ix <> fromSublistIndexToUpwardIndex i
            , csr_defSep_at: \i -> checkCursorSteps (fromSublistIndexToDownwardIndex i) csr
            }
    }

type RecDefinitionSeparator a
  = { ix_parentBlock :: UpwardIndex
    , ix :: UpwardIndex
    , csr :: Cursor
    } ->
    a

type RecDefinitionSeparator_Separator a
  = { ix_parentBlock :: UpwardIndex
    , ix :: UpwardIndex
    , isSelected :: Boolean
    } ->
    a

recDefinitionSeparator ::
  forall a.
  { separator :: RecDefinitionSeparator_Separator a } ->
  RecDefinitionSeparator a
recDefinitionSeparator rec { ix_parentBlock, ix, csr } =
  rec.separator
    { ix_parentBlock
    , ix
    , isSelected: checkCursorHere csr
    }

type RecDefinition a
  = RecMetaContext.RecDefinition
      ( { ix_parentBlock :: UpwardIndex
        , ix :: UpwardIndex
        , csr :: Cursor
        } ->
        a
      )

type RecDefinition_TermDefinition a
  = RecMetaContext.RecDefinition_TermDefinition
      ( { ix_parentBlock :: UpwardIndex
        , ix :: UpwardIndex
        , isSelected :: Boolean
        , ix_termBinding :: UpwardIndex
        , csr_termBinding :: Cursor
        , ix_type :: UpwardIndex
        , csr_type :: Cursor
        , ix_term :: UpwardIndex
        , csr_term :: Cursor
        } ->
        a
      )

type RecDefinition_DataDefinition a
  = RecMetaContext.RecDefinition_DataDefinition
      ( { ix_parentBlock :: UpwardIndex
        , ix :: UpwardIndex
        , isSelected :: Boolean
        , ix_typeBinding :: UpwardIndex
        , csr_typeBinding :: Cursor
        , ix_constr_at :: Int -> UpwardIndex
        , csr_constr_at :: Int -> Cursor
        , ix_constrSep_at :: Int -> UpwardIndex
        , csr_constrSep_at :: Int -> Cursor
        } ->
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
        \termBinding alpha a meta gamma metaGamma { ix_parentBlock, ix, csr } ->
          rec.term termBinding alpha a meta gamma metaGamma
            { ix_parentBlock
            , ix
            , isSelected: (checkCursorHere csr)
            , ix_termBinding: (ix :- IndexStep StepTermDefinition 0)
            , csr_termBinding: (checkCursorStep (IndexStep StepTermDefinition 0) csr)
            , ix_type: (ix :- IndexStep StepTermDefinition 1)
            , csr_type: (checkCursorStep (IndexStep StepTermDefinition 1) csr)
            , ix_term: (ix :- IndexStep StepTermDefinition 2)
            , csr_term: (checkCursorStep (IndexStep StepTermDefinition 2) csr)
            }
    , data:
        \typeBinding constrs meta gamma metaGamma { ix_parentBlock, ix, csr } ->
          let
            _ = unit -- if isJust csr then Debug.trace ("data" /\ ix /\ csr) identity else unit
          in
            rec.data typeBinding constrs meta gamma metaGamma
              { ix_parentBlock
              , ix
              , isSelected: (checkCursorHere csr)
              , ix_typeBinding: (ix :- IndexStep StepDataDefinition 0)
              , csr_typeBinding: (checkCursorStep (IndexStep StepDataDefinition 0) csr)
              , ix_constr_at: (\i -> ix <> singletonUpwardIndex (IndexStep StepDataDefinition 1) <> fromListIndexToUpwardIndex i <> singletonUpwardIndex (IndexStep StepConstructorItem 0))
              , csr_constr_at: (\i -> checkCursorSteps (singletonDownwardIndex (IndexStep StepDataDefinition 1) <> fromListIndexToDownwardIndex i <> singletonDownwardIndex (IndexStep StepConstructorItem 0)) csr)
              , ix_constrSep_at: (\i -> ix <> singletonUpwardIndex (IndexStep StepDataDefinition 1) <> fromSublistIndexToUpwardIndex i)
              , csr_constrSep_at: (\i -> checkCursorSteps (singletonDownwardIndex (IndexStep StepDataDefinition 1) <> fromSublistIndexToDownwardIndex i) csr)
              }
    }

type RecConstructorSeparator a
  = { ix_parentBlock :: UpwardIndex
    , ix_parentDef :: UpwardIndex
    , ix :: UpwardIndex
    , csr :: Cursor
    } ->
    a

type RecConstructorSeparator_Separator a
  = { ix_parentBlock :: UpwardIndex
    , ix_parentDef :: UpwardIndex
    , ix :: UpwardIndex
    , isSelected :: Boolean
    } ->
    a

recConstructorSeparator ::
  forall a.
  { separator :: RecConstructorSeparator_Separator a } ->
  RecConstructorSeparator a
recConstructorSeparator rec { ix_parentBlock, ix_parentDef, ix, csr } =
  rec.separator
    { ix_parentBlock
    , ix_parentDef
    , ix
    , isSelected: checkCursorHere csr
    }

type RecConstructor a
  = RecMetaContext.RecConstructor
      ( { ix_parentBlock :: UpwardIndex
        , ix_parentDef :: UpwardIndex
        , ix :: UpwardIndex
        , csr :: Cursor
        } ->
        a
      )

type RecConstructor_Constructor a
  = RecMetaContext.RecConstructor_Constructor
      ( { ix_parentBlock :: UpwardIndex
        , ix_parentDef :: UpwardIndex
        , ix :: UpwardIndex
        , isSelected :: Boolean
        , ix_termBinding :: UpwardIndex
        , csr_termBinding :: Cursor
        , ix_param_at :: (Int -> UpwardIndex)
        , csr_param_at :: (Int -> Cursor)
        , ix_paramSep_at :: (Int -> UpwardIndex)
        , csr_paramSep_at :: (Int -> Cursor)
        } ->
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
        \termBinding params meta typeId gamma alpha metaGamma metaGamma_param_at { ix_parentBlock, ix_parentDef, ix, csr } ->
          let
            _ = unit -- if isJust csr then Debug.trace ("constructor" /\ ix /\ csr) identity else unit
          in
            rec.constructor termBinding params meta typeId gamma alpha metaGamma metaGamma_param_at
              { ix_parentBlock
              , ix_parentDef
              , ix
              , isSelected: (checkCursorHere csr)
              , ix_termBinding: (ix :- IndexStep StepConstructor 0)
              , csr_termBinding: (checkCursorStep (IndexStep StepConstructor 0) csr)
              , ix_param_at: (\i -> ix <> singletonUpwardIndex (IndexStep StepConstructor 1) <> fromListIndexToUpwardIndex i <> singletonUpwardIndex (IndexStep StepParameterItem 0))
              , csr_param_at: (\i -> checkCursorSteps (singletonDownwardIndex (IndexStep StepConstructor 1) <> fromListIndexToDownwardIndex i <> singletonDownwardIndex (IndexStep StepParameterItem 0)) csr)
              , ix_paramSep_at: (\i -> ix <> singletonUpwardIndex (IndexStep StepConstructor 1) <> fromSublistIndexToUpwardIndex i)
              , csr_paramSep_at: (\i -> checkCursorSteps (singletonDownwardIndex (IndexStep StepConstructor 1) <> fromSublistIndexToDownwardIndex i) csr)
              }
    }

type RecParameterSeparator a
  = { ix_parentBlock :: UpwardIndex
    , ix_parentDef :: UpwardIndex
    , ix_parentConstr :: UpwardIndex
    , ix :: UpwardIndex
    , csr :: Cursor
    } ->
    a

type RecParameterSeparator_Separator a
  = { ix_parentBlock :: UpwardIndex
    , ix_parentDef :: UpwardIndex
    , ix_parentConstr :: UpwardIndex
    , ix :: UpwardIndex
    , isSelected :: Boolean
    } ->
    a

recParameterSeparator ::
  forall a.
  { separator :: RecParameterSeparator_Separator a } ->
  RecParameterSeparator a
recParameterSeparator rec { ix_parentBlock, ix_parentDef, ix_parentConstr, ix, csr } =
  rec.separator
    { ix_parentBlock
    , ix_parentDef
    , ix_parentConstr
    , ix
    , isSelected: checkCursorHere csr
    }

-- TODO: if necessary
{-
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

recDefinitionBindings ::
  forall a.
  { arrow_lambda :: RecDefinitionBindings_ArrowLambda a
  , wildcard :: RecDefinitionBindings_Wildcard a
  } ->
  RecDefinitionBindings a
recDefinitionBindings rec =
  RecMetaContext.recDefinitionBindings
    { arrow_lambda:
        \param beta termId block meta gamma metaGamma ix_def ix_type csr_type ix_term csr_term ->
          rec.arrow_lambda param beta termId block meta gamma metaGamma
            -- def
            ix_def
            -- type
            ix_type
            csr_type
            -- term
            ix_term
            csr_term
            -- param
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
  = RecMetaContext.RecType ({ ix :: UpwardIndex, csr :: Cursor } -> a)

type RecType_Arrow a
  = RecMetaContext.RecType_Arrow
      ( { ix :: UpwardIndex
        , isSelected :: Boolean
        , ix_param :: UpwardIndex
        , csr_param :: Cursor
        , ix_type :: UpwardIndex
        , csr_type :: Cursor
        } ->
        a
      )

type RecType_Data a
  = RecMetaContext.RecType_Data
      ( { ix :: UpwardIndex
        , isSelected :: Boolean
        } ->
        a
      )

type RecType_Hole a
  = RecMetaContext.RecType_Hole
      ( { ix :: UpwardIndex
        , isSelected :: Boolean
        } ->
        a
      )

type RecType_ProxyHole a
  = RecMetaContext.RecType_ProxyHole
      ( { ix :: UpwardIndex
        , isSelected :: Boolean
        } ->
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
        \param beta meta gamma metaGamma { ix, csr } ->
          rec.arrow param beta meta gamma metaGamma
            { ix
            , isSelected: (checkCursorHere csr)
            , ix_param: (ix :- IndexStep StepArrowType 0)
            , csr_param: (checkCursorStep (IndexStep StepArrowType 0) csr)
            , ix_type: (ix :- IndexStep StepArrowType 1)
            , csr_type: (checkCursorStep (IndexStep StepArrowType 1) csr)
            }
    , data: \typeId meta gamma metaGamma { ix, csr } -> rec.data typeId meta gamma metaGamma { ix, isSelected: checkCursorHere csr }
    , hole: \holeID wkn meta gamma metaGamma { ix, csr } -> rec.hole holeID wkn meta gamma metaGamma { ix, isSelected: checkCursorHere csr }
    , proxyHole: \holeID gamma metaGamma { ix, csr } -> rec.proxyHole holeID gamma metaGamma { ix, isSelected: checkCursorHere csr }
    }

type RecTerm a
  = RecMetaContext.RecTerm ({ ix :: UpwardIndex, csr :: Cursor } -> a)

type RecTerm_Lambda a
  = RecMetaContext.RecTerm_Lambda
      ( { ix :: UpwardIndex
        , isSelected :: Boolean
        , ix_termId :: UpwardIndex
        , csr_termId :: Cursor
        , ix_block :: UpwardIndex
        , csr_block :: Cursor
        } ->
        a
      )

type RecTerm_Neutral a
  = RecMetaContext.RecTerm_Neutral
      ( { ix :: UpwardIndex
        , isSelected :: Boolean
        , ix_termId :: UpwardIndex
        , csr_termId :: Cursor
        , ix_argItems :: UpwardIndex
        , csr_argItems :: Cursor
        } ->
        a
      )

type RecTerm_Match a
  = RecMetaContext.RecTerm_Match
      ( { ix :: UpwardIndex
        , isSelected :: Boolean
        , ix_term :: UpwardIndex
        , csr_term :: Cursor
        , ix_case_at :: (Int -> UpwardIndex)
        , csr_case_at :: (Int -> Cursor)
        } ->
        a
      )

type RecTerm_Hole a
  = RecMetaContext.RecTerm_Hole
      ( { ix :: UpwardIndex
        , isSelected :: Boolean
        } ->
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
        \termId block meta gamma param beta metaGamma { ix, csr } ->
          rec.lambda termId block meta gamma param beta metaGamma
            { ix
            , isSelected: (checkCursorHere csr)
            , ix_termId: (ix :- IndexStep StepLambdaTerm 0)
            , csr_termId: (checkCursorStep (IndexStep StepLambdaTerm 0) csr)
            , ix_block: (ix :- IndexStep StepLambdaTerm 1)
            , csr_block: (checkCursorStep (IndexStep StepLambdaTerm 1) csr)
            }
    , neutral:
        \termId argItems meta gamma alpha metaGamma { ix, csr } ->
          rec.neutral termId argItems meta gamma alpha metaGamma
            { ix
            , isSelected: (checkCursorHere csr)
            , ix_termId: (ix :- IndexStep StepNeutralTerm 0)
            , csr_termId: (checkCursorStep (IndexStep StepNeutralTerm 0) csr)
            , ix_argItems: (ix :- IndexStep StepNeutralTerm 1)
            , csr_argItems: (checkCursorStep (IndexStep StepNeutralTerm 1) csr)
            }
    , match:
        \typeId a cases meta gamma alpha metaGamma constrIDs { ix, csr } ->
          rec.match typeId a cases meta gamma alpha metaGamma constrIDs
            { ix
            , isSelected: (checkCursorHere csr)
            , ix_term: (ix :- IndexStep StepMatchTerm 0)
            , csr_term: (checkCursorStep (IndexStep StepMatchTerm 0) csr)
            , ix_case_at: (\i -> ix <> singletonUpwardIndex (IndexStep StepMatchTerm 1) <> fromListIndexToUpwardIndex i <> singletonUpwardIndex (IndexStep StepCaseItem 0))
            , csr_case_at: (\i -> checkCursorSteps (singletonDownwardIndex (IndexStep StepMatchTerm 1) <> fromListIndexToDownwardIndex i <> singletonDownwardIndex (IndexStep StepCaseItem 0)) csr)
            }
    , hole:
        \meta gamma alpha metaGamma { ix, csr } ->
          rec.hole meta gamma alpha metaGamma { ix, isSelected: checkCursorHere csr }
    }

type RecArgItems a
  = RecMetaContext.RecArgItems ({ ix :: UpwardIndex, csr :: Cursor } -> a)

type RecArgItems_Nil (a :: Prim.Type)
  = RecMetaContext.RecArgItems_Nil a

type RecArgItems_Cons a
  = RecMetaContext.RecArgItems_Cons
      ( { ix :: UpwardIndex
        , isSelected :: Boolean
        , ix_term :: UpwardIndex
        , csr_term :: Cursor
        , ix_argItems :: UpwardIndex
        , csr_argItems :: Cursor
        } ->
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
    { nil:
        \gamma alpha metaGamma { ix, csr } ->
          rec.nil gamma alpha metaGamma
    , cons:
        \argItem argItems gamma param beta metaGamma { ix, csr } ->
          rec.cons argItem argItems gamma param beta metaGamma
            { ix
            , isSelected: checkCursorHere csr
            , ix_term: (ix :- IndexStep StepCons 0)
            , csr_term: (checkCursorStep (IndexStep StepCons 0) csr)
            , ix_argItems: (ix :- IndexStep StepCons 1)
            , csr_argItems: (checkCursorStep (IndexStep StepCons 1) csr)
            }
    }

type RecCase a
  = RecMetaContext.RecCase
      ( { ix_parentMatch :: UpwardIndex
        , ix :: UpwardIndex
        , csr :: Cursor
        } ->
        a
      )

type RecCase_Case a
  = RecMetaContext.RecCase_Case
      ( { ix_parentMatch :: UpwardIndex
        , ix :: UpwardIndex
        , isSelected :: Boolean
        , ix_termId_at :: (Int -> UpwardIndex)
        , csr_termId_at :: (Int -> Cursor)
        , ix_block :: UpwardIndex
        , csr_block :: Cursor
        } ->
        a
      )

recCase ::
  forall a.
  { case_ :: RecCase_Case a } ->
  RecCase a
recCase rec =
  RecMetaContext.recCase
    { case_:
        \termIds block meta typeId constrId gamma alpha metaGamma { ix_parentMatch, ix, csr } ->
          rec.case_ termIds block meta typeId constrId gamma alpha metaGamma
            { ix_parentMatch
            , ix
            , isSelected: (checkCursorHere csr)
            , ix_termId_at: (\i -> ix <> singletonUpwardIndex (IndexStep StepCase 1) <> fromListIndexToUpwardIndex i <> singletonUpwardIndex (IndexStep StepTermIdItem 0))
            , csr_termId_at: (\i -> checkCursorSteps (singletonDownwardIndex (IndexStep StepCase 1) <> fromListIndexToDownwardIndex i <> singletonDownwardIndex (IndexStep StepTermIdItem 0)) csr)
            , ix_block: (ix :- IndexStep StepCase 1)
            , csr_block: (checkCursorStep (IndexStep StepCase 1) csr)
            }
    }

type RecParameter a
  = RecMetaContext.RecParameter ({ ix :: UpwardIndex, csr :: Cursor } -> a)

type RecParameter_Parameter a
  = RecMetaContext.RecParameter_Parameter
      ( { ix :: UpwardIndex
        , isSelected :: Boolean
        , ix_type :: UpwardIndex
        , csr_type :: Cursor
        } ->
        a
      )

recParameter ::
  forall a.
  { parameter :: RecParameter_Parameter a } ->
  RecParameter a
recParameter rec =
  RecMetaContext.recParameter
    { parameter:
        \alpha meta gamma metaGamma { ix, csr } ->
          rec.parameter alpha meta gamma metaGamma
            { ix
            , isSelected: (checkCursorHere csr)
            , ix_type: (ix :- IndexStep StepParameter 0)
            , csr_type: (checkCursorStep (IndexStep StepParameter 0) csr)
            }
    }

type RecTypeBinding a
  = TypeBinding -> Context -> MetaContext -> { ix :: UpwardIndex, csr :: Cursor } -> a

type RecTypeBinding_TypeBinding a
  = TypeId -> TypeBindingMetadata -> Context -> MetaContext -> { ix :: UpwardIndex, isSelected :: Boolean } -> a

recTypeBinding ::
  forall a.
  { typeBinding :: RecTypeBinding_TypeBinding a
  } ->
  RecTypeBinding a
recTypeBinding rec (TypeBinding typeId meta) gamma metaGamma { ix, csr } = rec.typeBinding typeId meta gamma metaGamma { ix, isSelected: checkCursorHere csr }

type RecTermBinding a
  = TermBinding -> Context -> MetaContext -> { ix :: UpwardIndex, csr :: Cursor } -> a

type RecTermBinding_TermBinding a
  = TermId -> TermBindingMetadata -> Context -> MetaContext -> { ix :: UpwardIndex, isSelected :: Boolean } -> a

recTermBinding ::
  forall a.
  { termBinding :: RecTermBinding_TermBinding a
  } ->
  RecTermBinding a
recTermBinding rec (TermBinding termId meta) gamma metaGamma { ix, csr } = rec.termBinding termId meta gamma metaGamma { ix, isSelected: checkCursorHere csr }

type RecTermId a
  = TermId -> Context -> MetaContext -> { ix :: UpwardIndex, csr :: Cursor } -> a

type RecTermId_TermId a
  = TermId -> Context -> MetaContext -> { ix :: UpwardIndex, isSelected :: Boolean } -> a

recTermId :: forall a. { termId :: RecTermId_TermId a } -> RecTermId a
recTermId rec termId gamma metaGamma { ix, csr } = rec.termId termId gamma metaGamma { ix, isSelected: checkCursorHere csr }
