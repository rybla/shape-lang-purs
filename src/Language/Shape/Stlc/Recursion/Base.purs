module Language.Shape.Stlc.Recursion.Base where

import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Data.List (List)
import Undefined (undefined)

type RecModule a
  = Module -> a

type RecModule_Module a
  = List Definition -> ModuleMetadata -> a

recModule ::
  forall a.
  { module_ :: RecModule_Module a } ->
  RecModule a
recModule rec = case _ of
  Module defs meta -> rec.module_ defs meta

type RecBlock a
  = Block -> a

type RecBlock_Block a
  = List Definition -> Term -> BlockMetadata -> a

recBlock ::
  forall a.
  { block :: RecBlock_Block a } ->
  RecBlock a
recBlock rec = case _ of
  Block defs term meta -> rec.block defs term meta

type RecDefinitions a
  = List Definition -> a

type RecDefinitions_Definitions a
  = List Definition -> a

recDefinitions ::
  forall a.
  { definitions :: RecDefinitions_Definitions a } ->
  RecDefinitions a
recDefinitions rec = rec.definitions

recDefinition = undefined

recConstructor = undefined

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
  = TermId -> Args -> NeutralTermMetadata -> a

type RecTerm_Match a
  = TypeId -> Term -> List Case -> MatchTermMetadata -> a

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
  NeutralTerm termId args meta -> rec.neutral termId args meta
  MatchTerm typeId a cases meta -> rec.match typeId a cases meta
  HoleTerm meta -> rec.hole meta

type RecArgs a
  = Args -> a

type RecArgs_None (a :: Prim.Type)
  = a

type RecArgs_Cons a
  = Term -> Args -> ArgConsMetaData -> a

recArgs ::
  forall a.
  { none :: RecArgs_None a
  , cons :: RecArgs_Cons a
  } ->
  RecArgs a
recArgs rec = case _ of
  NoneArgs -> rec.none
  ConsArgs a args meta -> rec.cons a args meta

type RecCase a
  = Case -> a

type RecCase_Case a
  = List TermId -> Term -> CaseMetadata -> a

recCase ::
  forall a.
  { case_ :: RecCase_Case a } ->
  RecCase a
recCase rec = case _ of
  Case termIds a meta -> rec.case_ termIds a meta

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
