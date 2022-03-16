module Language.Shape.Stlc.Recursion.Base where

import Data.Tuple.Nested
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Data.List
import Undefined (undefined)
import Unsafe (error)

type RecModule a
  = Module -> a

type RecModule_Module a
  = List DefinitionItem -> ModuleMetadata -> a

recModule ::
  forall a.
  { module_ :: RecModule_Module a } ->
  RecModule a
recModule rec = case _ of
  Module defItems meta -> rec.module_ defItems meta

type RecBlock a
  = Block -> a

type RecBlock_Block a
  = List DefinitionItem -> Term -> BlockMetadata -> a

recBlock ::
  forall a.
  { block :: RecBlock_Block a } ->
  RecBlock a
recBlock rec = case _ of
  Block defItems term meta -> rec.block defItems term meta

type RecDefinitionItems a
  = List DefinitionItem -> a

type RecDefinitionItems_DefinitionItems a
  = List DefinitionItem -> a

recDefinitionItems ::
  forall a.
  { definitionItems :: RecDefinitionItems_DefinitionItems a } ->
  RecDefinitionItems a
recDefinitionItems rec = rec.definitionItems

type RecDefinition a
  = Definition -> a

type RecDefinition_TermDefinition a
  = TermBinding -> Type -> Term -> TermDefinitionMetadata -> a

type RecDefinition_DataDefinition a
  = TypeBinding -> List ConstructorItem -> DataDefinitionMetadata -> a

recDefinition ::
  forall a.
  { term :: RecDefinition_TermDefinition a
  , data :: RecDefinition_DataDefinition a
  } ->
  RecDefinition a
recDefinition rec = case _ of
  TermDefinition termBinding alpha a meta -> rec.term termBinding alpha a meta
  DataDefinition typeBinding constrs meta -> rec.data typeBinding constrs meta

type RecConstructor a
  = Constructor -> TypeId -> a

type RecConstructor_Constructor a
  = TermBinding -> List ParameterItem -> ConstructorMetadata -> TypeId -> a

recConstructor :: forall a. { constructor :: RecConstructor_Constructor a } -> RecConstructor a
recConstructor rec constr typeId = case constr of
  Constructor termBinding prms meta -> rec.constructor termBinding prms meta typeId

type RecDefinitionBindings a
  = Type -> Term -> a

type RecDefinitionBindings_ArrowLambda a
  = Parameter -> Type -> TermId -> Block -> LambdaTermMetadata -> a

type RecDefinitionBindings_Wildcard a
  = Type -> Term -> a

{-
recDefinitionBindings ::
  forall a.
  { arrow_lambda :: RecDefinitionBindings_ArrowLambda a
  , wildcard :: RecDefinitionBindings_Wildcard a
  } ->
  RecDefinitionBindings a
recDefinitionBindings rec alpha a = case alpha /\ a of
  ArrowType prm beta _ /\ LambdaTerm termId block meta -> rec.arrow_lambda prm beta termId block meta
  _ /\ _ -> rec.wildcard alpha a
-}
type RecType a
  = Type -> a

type RecType_Arrow a
  = Parameter -> Type -> ArrowTypeMetadata -> a

type RecType_Data a
  = TypeId -> DataTypeMetadata -> a

type RecType_Hole a
  = HoleId -> TypeWeakening -> HoleTypeMetadata -> a

type RecType_ProxyHole a
  = HoleId -> a

recType ::
  forall a.
  { arrow :: RecType_Arrow a
  , data :: RecType_Data a
  , hole :: RecType_Hole a
  , proxyHole :: RecType_ProxyHole a
  } ->
  RecType a
recType rec = case _ of
  ArrowType prm alpha meta -> rec.arrow prm alpha meta
  DataType typeId meta -> rec.data typeId meta
  HoleType holeId wkn meta -> rec.hole holeId wkn meta
  ProxyHoleType holeId -> rec.proxyHole holeId

type RecTerm a
  = Term -> a

type RecTerm_Lambda a
  = TermId -> Block -> LambdaTermMetadata -> a

type RecTerm_Neutral a
  = TermId -> List ArgItem -> NeutralTermMetadata -> a

type RecTerm_Match a
  = TypeId -> Term -> List CaseItem -> MatchTermMetadata -> a

type RecTerm_Hole a
  = HoleTermMetadata -> a

recTerm ::
  forall a.
  { lambda :: RecTerm_Lambda a
  , neutral :: RecTerm_Neutral a
  , match :: RecTerm_Match a
  , hole :: RecTerm_Hole a
  } ->
  RecTerm a
recTerm rec = case _ of
  LambdaTerm termId block meta -> rec.lambda termId block meta
  NeutralTerm termId argItems meta -> rec.neutral termId argItems meta
  MatchTerm typeId a caseItems meta -> rec.match typeId a caseItems meta
  HoleTerm meta -> rec.hole meta

type RecArgItems a
  = List ArgItem -> a

type RecArgItems_Nil :: forall k. k -> k
type RecArgItems_Nil a
  = a

type RecArgItems_Cons a
  = ArgItem -> List ArgItem -> a

recArgItems ::
  forall a.
  { nil :: RecArgItems_Nil a
  , cons :: RecArgItems_Cons a
  } ->
  RecArgItems a
recArgItems rec argItems = case argItems of
  Nil -> rec.nil
  Cons argItem argItems -> rec.cons argItem argItems

type RecCase a
  = Case -> TypeId -> TermId -> a

type RecCase_Case a
  = List TermIdItem -> Term -> CaseMetadata -> TypeId -> TermId -> a

recCase ::
  forall a.
  { case_ :: RecCase_Case a } ->
  RecCase a
recCase rec case_ typeId constrId = case case_ of
  Case termIdItems a meta -> rec.case_ termIdItems a meta typeId constrId

type RecParameter a
  = Parameter -> a

type RecParameter_Parameter a
  = Type -> ParameterMetadata -> a

recParameter ::
  forall a.
  { parameter :: RecParameter_Parameter a } ->
  RecParameter a
recParameter rec = case _ of
  Parameter alpha meta -> rec.parameter alpha meta
