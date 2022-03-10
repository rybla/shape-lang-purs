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

-- recDefinition ::
--   forall a.
--   { term :: TermBinding -> Type -> Term -> TermDefinitionMetadata -> a
--   , data :: TypeBinding -> List Constructor -> DataDefinitionMetadata -> a
--   } ->
--   Definition -> a
-- recDefinition rec = case _ of
--   TermDefinition termBnd alpha a meta -> rec.term termBnd alpha a meta
--   DataDefinition typeBnd constrs meta -> rec.data typeBnd constrs meta
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
  , neutral :: TermID -> Args -> NeutralTermMetadata -> a
  , match :: TypeID -> Term -> List Case -> MatchTermMetadata -> a
  , hole :: HoleTermMetadata -> a
  } ->
  Term -> a
recTerm rec = case _ of
  LambdaTerm termID block meta -> rec.lambda termID block meta
  NeutralTerm termID args meta -> rec.neutral termID args meta
  MatchTerm typeID a cases meta -> rec.match typeID a cases meta
  HoleTerm meta -> rec.hole meta

recArgs ::
  forall a.
  { none :: a
  , cons :: Term -> Args -> ArgConsMetaData -> a
  } ->
  Args -> a
recArgs = undefined

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
