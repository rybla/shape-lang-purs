module Language.Shape.Stlc.Recursion.Base where

import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Data.List (List)

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

recDefinition ::
  forall a.
  { term :: TermBinding -> Type -> Term -> TermDefinitionMetadata -> a
  , data :: TypeBinding -> List Constructor -> DataDefinitionMetadata -> a
  } ->
  Definition -> a
recDefinition rec = case _ of
  TermDefinition termBnd alpha a meta -> rec.term termBnd alpha a meta
  DataDefinition typeBnd constrs meta -> rec.data typeBnd constrs meta

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
  , data :: TypeID -> DataTypeMetadata -> a
  , hole :: HoleID -> TypeWeakening -> HoleTypeMetadata -> a
  , proxyHole :: HoleID -> a
  } ->
  Type -> a
recType rec = case _ of
  ArrowType prm alpha meta -> rec.arrow prm alpha meta
  DataType typeID meta -> rec.data typeID meta
  HoleType holeID wkn meta -> rec.hole holeID wkn meta
  ProxyHoleType holeID -> rec.proxyHole holeID

recTerm ::
  forall a.
  { lambda :: TermID -> Block -> LambdaTermMetadata -> a
  , neutral :: NeutralTerm -> NeutralTermMetadata -> a
  , match :: TypeID -> Term -> List Case -> MatchTermMetadata -> a
  , hole :: HoleTermMetadata -> a
  } ->
  Term -> a
recTerm rec = case _ of
  LambdaTerm termID block meta -> rec.lambda termID block meta
  NeutralTerm neu meta -> rec.neutral neu meta
  MatchTerm typeID a cases meta -> rec.match typeID a cases meta
  HoleTerm meta -> rec.hole meta

recNeutralTerm ::
  forall a.
  { variable :: TermID -> VariableTermMetadata -> a
  , application :: NeutralTerm -> Term -> ApplicationTermMetadata -> a
  } ->
  NeutralTerm -> a
recNeutralTerm rec = case _ of
  VariableTerm termID meta -> rec.variable termID meta
  ApplicationTerm neu a meta -> rec.application neu a meta

recCase ::
  forall a.
  { case_ :: List TermID -> Term -> CaseMetadata -> a } ->
  Case -> a
recCase rec = case _ of
  Case termIDs a meta -> rec.case_ termIDs a meta

recParameter ::
  forall a.
  { parameter :: Type -> ParameterMetadata -> a } ->
  Parameter -> a
recParameter rec = case _ of
  Parameter alpha meta -> rec.parameter alpha meta
