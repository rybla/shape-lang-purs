module Language.Shape.Stlc.Recursion.Context where

import Data.Foldable
import Data.List
import Data.Tuple.Nested
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Typing
import Prelude
import Prim hiding (Type)
import Data.Map.Unsafe as Map
import Data.Tuple (fst)
import Debug as Debug
import Language.Shape.Stlc.Recursion.Base as RecBase
import Undefined (undefined)
import Unsafe as Unsafe

-- TODO: adds typeIds to context appropriately

-- Recursion principles for handling context & type
type RecModule a
  = RecBase.RecModule (Context -> a)

type RecModule_Module a
  = RecBase.RecModule_Module (Context -> a)

recModule ::
  forall a.
  { module_ :: RecModule_Module a } ->
  RecModule a
recModule rec =
  RecBase.recModule
    { module_:
        \defs meta gamma ->
          rec.module_ defs meta (addDefinitionsToContext (fromItem <$> defs) gamma)
    }

type RecBlock a
  = RecBase.RecBlock (Context -> Type -> a)

type RecBlock_Block a
  = RecBase.RecBlock_Block (Context -> Type -> a)

recBlock ::
  forall a.
  { block :: RecBlock_Block a } ->
  RecBlock a
recBlock rec =
  RecBase.recBlock
    { block:
        \defs a meta gamma alpha ->
          rec.block defs a meta (addDefinitionsToContext (fromItem <$> defs) gamma) alpha
    }

type RecDefinitionItems a
  = RecBase.RecDefinitionItems (Context -> a)

type RecDefinitionItems_DefinitionItems a
  = RecBase.RecDefinitionItems_DefinitionItems (Context -> a)

recDefinitionItems ::
  forall a.
  { definitionItems :: RecDefinitionItems_DefinitionItems a } ->
  RecDefinitionItems a
recDefinitionItems rec = rec.definitionItems

type RecDefinition a
  = RecBase.RecDefinition (Context -> a)

type RecDefinition_TermDefinition a
  = RecBase.RecDefinition_TermDefinition (Context -> a)

type RecDefinition_DataDefinition a
  = RecBase.RecDefinition_DataDefinition (Context -> a)

recDefinition ::
  forall a.
  { term :: RecDefinition_TermDefinition a
  , data :: RecDefinition_DataDefinition a
  } ->
  RecDefinition a
recDefinition = RecBase.recDefinition

type RecConstructor a
  = RecBase.RecConstructor (Context -> a)

type RecConstructor_Constructor a
  = RecBase.RecConstructor_Constructor (Context -> Type -> a)

recConstructor ::
  forall a.
  { constructor :: RecConstructor_Constructor a } ->
  RecConstructor a
recConstructor rec =
  RecBase.recConstructor
    { constructor:
        \termBinding params meta typeId gamma ->
          rec.constructor termBinding params meta typeId gamma
            (typeOfConstructor (fromItem <$> params) typeId)
    }

type RecType_ProxyHole a
  = RecBase.RecType_ProxyHole (Context -> a)

type RecDefinitionBindings a
  = RecBase.RecDefinitionBindings (Context -> a)

type RecDefinitionBindings_ArrowLambda a
  = RecBase.RecDefinitionBindings_ArrowLambda (Context -> a)

type RecDefinitionBindings_Wildcard a
  = RecBase.RecDefinitionBindings_Wildcard (Context -> a)

{-
recDefinitionBindings ::
  forall a.
  { arrow_lambda :: RecDefinitionBindings_ArrowLambda a
  , wildcard :: RecDefinitionBindings_Wildcard a
  } ->
  RecDefinitionBindings a
recDefinitionBindings rec =
  RecBase.recDefinitionBindings
    { arrow_lambda: \param@(Parameter alpha _) beta termId block meta gamma -> rec.arrow_lambda param beta termId block meta (Map.insert termId alpha gamma)
    , wildcard: rec.wildcard
    }
-}
type RecType a
  = RecBase.RecType (Context -> a)

type RecType_Arrow a
  = RecBase.RecType_Arrow (Context -> a)

type RecType_Data a
  = RecBase.RecType_Data (Context -> a)

type RecType_Hole a
  = RecBase.RecType_Hole (Context -> a)

recType ::
  forall a.
  { arrow :: RecType_Arrow a
  , data :: RecType_Data a
  , hole :: RecType_Hole a
  , proxyHole :: RecType_ProxyHole a
  } ->
  RecType a
recType = RecBase.recType

type RecTerm a
  = RecBase.RecTerm (Context -> Type -> a)

type RecTerm_Lambda a
  = RecBase.RecTerm_Lambda (Context -> Parameter -> Type -> a)

type RecTerm_Neutral a
  = RecBase.RecTerm_Neutral (Context -> Type -> a)

type RecTerm_Match a
  = RecBase.RecTerm_Match (Context -> Type -> a)

type RecTerm_Hole a
  = RecBase.RecTerm_Hole (Context -> Type -> a)

recTerm ::
  forall a.
  { lambda :: RecTerm_Lambda a
  , neutral :: RecTerm_Neutral a
  , match :: RecTerm_Match a
  , hole :: RecTerm_Hole a
  } ->
  RecTerm a
recTerm rec =
  RecBase.recTerm
    { lambda:
        \termId block meta gamma alpha -> case alpha of
          ArrowType param@(Parameter alpha _) beta _ -> rec.lambda termId block meta (insertTyping termId alpha gamma) param beta
          _ -> Unsafe.error $ "[Context.recTerm.lambda] impossible: the term " <> show (LambdaTerm termId block meta) <> " has type " <> show alpha
    , neutral:
        \termId argItems meta gamma alpha ->
          -- Debug.trace ("RecContext.recTerm:neutral" <> "\ntypes: " <> show gamma.types <> "\ntype: " <> show (lookupTyping termId gamma) <> "\nargs: " <> show (fromItem <$> argItems)) \_ ->
          rec.neutral termId argItems meta gamma (lookupTyping termId gamma)
    , hole:
        \meta gamma alpha ->
          rec.hole meta gamma alpha
    , match:
        \dataID a cases meta gamma alpha ->
          rec.match dataID a cases meta gamma alpha
    }

type RecArgItems a
  = RecBase.RecArgItems (Context -> Type -> a)

type RecArgItems_Nil a
  = RecBase.RecArgItems_Nil (Context -> Type -> a)

type RecArgItems_Cons a
  = RecBase.RecArgItems_Cons (Context -> Parameter -> Type -> a)

recArgItems ::
  forall a.
  { nil :: RecArgItems_Nil a
  , cons :: RecArgItems_Cons a
  } ->
  RecArgItems a
recArgItems rec =
  RecBase.recArgItems
    { nil: rec.nil
    , cons:
        \argItem argItems gamma alpha -> case alpha of
          ArrowType param beta _ -> rec.cons argItem argItems gamma param beta
          _ -> Unsafe.error "Context.recArgItems: impossible"
    }

type RecCase a
  = RecBase.RecCase (Context -> Type -> a)

type RecCase_Case a
  = RecBase.RecCase_Case (Context -> Type -> a)

recCase ::
  forall a.
  { case_ :: RecCase_Case a } ->
  RecCase a
recCase rec =
  RecBase.recCase
    { case_:
        \termIds block meta typeId constrId gamma alpha ->
          let
            params /\ _ = flattenArrowType $ lookupTyping constrId gamma
          in
            rec.case_ termIds block meta typeId constrId
              ( foldl
                  (\gamma' (termId /\ (Parameter alpha _)) -> insertTyping termId alpha gamma')
                  gamma
                  (zip (fromItem <$> termIds) params)
              )
              alpha
    }

type RecParameter a
  = RecBase.RecParameter (Context -> a)

type RecParameter_Parameter a
  = RecBase.RecParameter_Parameter (Context -> a)

recParameter ::
  forall a.
  { parameter :: RecParameter_Parameter a } ->
  RecParameter a
recParameter = RecBase.recParameter
