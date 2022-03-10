module Language.Shape.Stlc.Recursion.Base where

import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Data.List (List)
import Undefined (undefined)

recModule ::
  forall a.
  { module_ :: List Definition -> ModuleMetadata -> a
  } ->
  Module ->
  a
recModule rec = case _ of
  Module defs meta -> rec.module_ defs meta

recBlock ::
  forall a.
  { block :: List Definition -> Term -> BlockMetadata -> a
  } ->
  Block -> a
recBlock rec = case _ of
  Block defs term meta -> rec.block defs term meta

recDefinitions ::
  forall a.
  { definitions :: List Definition -> a
  } ->
  List Definition -> a
recDefinitions rec = rec.definitions

recConstructor ::
  forall a.
  { constructor :: TermBinding -> List Parameter -> ConstructorMetadata -> a
  } ->
  Constructor -> a
recConstructor rec = case _ of
  Constructor termBnd prms meta -> rec.constructor termBnd prms meta

recType ::
  forall a.
  { arrow :: Parameter -> Type -> ArrowTypeMetadata -> a
  , data :: TypeId -> DataTypeMetadata -> a
  , hole :: HoleID -> TypeWeakening -> HoleTypeMetadata -> a
  , proxyHole :: HoleID -> a
  } ->
  Type -> a
recType rec = case _ of
  ArrowType prm alpha meta -> rec.arrow prm alpha meta
  DataType typeId meta -> rec.data typeId meta
  HoleType holeID wkn meta -> rec.hole holeID wkn meta
  ProxyHoleType holeID -> rec.proxyHole holeID

recTerm ::
  forall a.
  { lambda :: TermId -> Block -> LambdaTermMetadata -> a
  , neutral :: TermId -> Args -> NeutralTermMetadata -> a
  , match :: TypeId -> Term -> List Case -> MatchTermMetadata -> a
  , hole :: HoleTermMetadata -> a
  } ->
  Term -> a
recTerm rec = case _ of
  LambdaTerm termId block meta -> rec.lambda termId block meta
  NeutralTerm termId args meta -> rec.neutral termId args meta
  MatchTerm typeId a cases meta -> rec.match typeId a cases meta
  HoleTerm meta -> rec.hole meta

recArgs ::
  forall a.
  { none :: a
  , cons :: Term -> Args -> ArgConsMetaData -> a
  } ->
  Args -> a
recArgs rec = case _ of
  NoneArgs -> rec.none
  ConsArgs a args meta -> rec.cons a args meta

recCase ::
  forall a.
  { case_ :: List TermId -> Term -> CaseMetadata -> a } ->
  Case -> a
recCase rec = case _ of
  Case termIds a meta -> rec.case_ termIds a meta

recParameter ::
  forall a.
  { parameter :: Type -> ParameterMetadata -> a } ->
  Parameter -> a
recParameter rec = case _ of
  Parameter alpha meta -> rec.parameter alpha meta
