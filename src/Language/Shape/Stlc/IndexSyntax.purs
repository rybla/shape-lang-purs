module Language.Shape.Stlc.IndexSyntax where

import Data.List
import Data.Tuple
import Data.Tuple.Nested
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Syntax
import Prelude
import Data.Maybe
import Partial.Unsafe (unsafeCrashWith)

stepSyntax :: IndexStep -> Syntax -> Syntax
stepSyntax step syn = case syn /\ step of
  SyntaxModule (Module defItems meta) /\ IndexStep StepModule 0 -> SyntaxList $ SyntaxDefinitionItem <$> defItems
  SyntaxBlock (Block defItems a meta) /\ IndexStep StepBlock 0 -> SyntaxList $ SyntaxDefinitionItem <$> defItems
  SyntaxBlock (Block defItems a meta) /\ IndexStep StepBlock 1 -> SyntaxTerm a
  SyntaxDefinition (TermDefinition x alpha a meta) /\ IndexStep StepTermDefinition 0 -> SyntaxTermBinding x
  SyntaxDefinition (TermDefinition x alpha a meta) /\ IndexStep StepTermDefinition 1 -> SyntaxType alpha
  SyntaxDefinition (TermDefinition x alpha a meta) /\ IndexStep StepTermDefinition 2 -> SyntaxTerm a
  SyntaxDefinition (DataDefinition t constrItems meta) /\ IndexStep StepDataDefinition 0 -> SyntaxTypeBinding t
  SyntaxDefinition (DataDefinition t constrItems meta) /\ IndexStep StepDataDefinition 1 -> SyntaxList $ SyntaxConstructorItem <$> constrItems
  SyntaxConstructor (Constructor x paramItems meta) /\ IndexStep StepConstructor 0 -> SyntaxTermBinding x
  SyntaxConstructor (Constructor x paramItems meta) /\ IndexStep StepConstructor 1 -> SyntaxList $ SyntaxParameterItem <$> paramItems
  SyntaxType (ArrowType param beta meta) /\ IndexStep StepArrowType 0 -> SyntaxParameter param
  SyntaxType (ArrowType param beta meta) /\ IndexStep StepArrowType 1 -> SyntaxType beta
  SyntaxTerm (LambdaTerm x block meta) /\ IndexStep StepLambdaTerm 0 -> SyntaxTermId x
  SyntaxTerm (LambdaTerm x block meta) /\ IndexStep StepLambdaTerm 1 -> SyntaxBlock block
  SyntaxTerm (NeutralTerm x argItems meta) /\ IndexStep StepNeutralTerm 0 -> SyntaxTermId x
  SyntaxTerm (NeutralTerm x argItems meta) /\ IndexStep StepNeutralTerm 1 -> SyntaxList $ SyntaxArgItem <$> argItems
  SyntaxTerm (MatchTerm typeId a caseItems meta) /\ IndexStep StepMatchTerm 0 -> SyntaxTerm a
  SyntaxTerm (MatchTerm typeId a caseItems meta) /\ IndexStep StepMatchTerm 1 -> SyntaxList $ SyntaxCaseItem <$> caseItems
  SyntaxCase (Case xItems block meta) /\ IndexStep StepCase 0 -> SyntaxList $ SyntaxTermIdItem <$> xItems
  SyntaxCase (Case xItems block meta) /\ IndexStep StepCase 1 -> SyntaxBlock block
  SyntaxParameter (Parameter alpha meta) /\ IndexStep StepParameter 0 -> SyntaxType alpha
  -- items
  SyntaxDefinitionItem (def /\ meta) /\ IndexStep StepDefinitionItem 0 -> SyntaxDefinition def
  SyntaxConstructorItem (constr /\ meta) /\ IndexStep StepConstructorItem 0 -> SyntaxConstructor constr
  SyntaxCaseItem (case_ /\ meta) /\ IndexStep StepCaseItem 0 -> SyntaxCase case_
  SyntaxParameterItem (param /\ meta) /\ IndexStep StepParameterItem 0 -> SyntaxParameter param
  SyntaxArgItem (arg /\ meta) /\ IndexStep StepArgItem 0 -> SyntaxTerm arg
  SyntaxTermIdItem (termId /\ meta) /\ IndexStep StepTermIdItem 0 -> SyntaxTermId termId
  -- list
  SyntaxList (Cons h t) /\ IndexStep StepCons 0 -> h
  SyntaxList (Cons h t) /\ IndexStep StepCons 1 -> SyntaxList t
  _ -> unsafeCrashWith $ "stepSyntax: impossible: " <> show (step /\ syn)

wrapStepSyntax :: IndexStep -> Syntax -> (Syntax -> Syntax)
wrapStepSyntax step syn synSub = case syn /\ step /\ synSub of
  SyntaxModule (Module defItems meta) /\ IndexStep StepModule 0 /\ SyntaxList defs' -> SyntaxModule (Module (toDefinitionItem <$> defs') meta)
  SyntaxBlock (Block defItems a meta) /\ IndexStep StepBlock 0 /\ SyntaxList defs' -> SyntaxBlock (Block (toDefinitionItem <$> defs') a meta)
  SyntaxBlock (Block defItems a meta) /\ IndexStep StepBlock 1 /\ SyntaxTerm a' -> SyntaxBlock (Block defItems a' meta)
  SyntaxDefinition (TermDefinition x alpha a meta) /\ IndexStep StepTermDefinition 0 /\ SyntaxTermBinding x' -> SyntaxDefinition (TermDefinition x' alpha a meta)
  SyntaxDefinition (TermDefinition x alpha a meta) /\ IndexStep StepTermDefinition 1 /\ SyntaxType alpha' -> SyntaxDefinition (TermDefinition x alpha' a meta)
  SyntaxDefinition (TermDefinition x alpha a meta) /\ IndexStep StepTermDefinition 2 /\ SyntaxTerm a' -> SyntaxDefinition (TermDefinition x alpha a' meta)
  SyntaxDefinition (DataDefinition t constrItems meta) /\ IndexStep StepDataDefinition 0 /\ SyntaxTypeBinding t' -> SyntaxDefinition (DataDefinition t' constrItems meta)
  SyntaxDefinition (DataDefinition t constrItems meta) /\ IndexStep StepDataDefinition 1 /\ SyntaxList constrs' -> SyntaxDefinition (DataDefinition t (toConstructorItem <$> constrs') meta)
  SyntaxConstructor (Constructor x paramItems meta) /\ IndexStep StepConstructor 0 /\ SyntaxTermBinding x' -> SyntaxConstructor (Constructor x' paramItems meta)
  SyntaxConstructor (Constructor x paramItems meta) /\ IndexStep StepConstructor 1 /\ SyntaxList params' -> SyntaxConstructor (Constructor x (toParameterItem <$> params') meta)
  SyntaxType (ArrowType param beta meta) /\ IndexStep StepArrowType 0 /\ SyntaxParameter param' -> SyntaxType (ArrowType param' beta meta)
  SyntaxType (ArrowType param beta meta) /\ IndexStep StepArrowType 1 /\ SyntaxType beta' -> SyntaxType (ArrowType param beta' meta)
  SyntaxTerm (LambdaTerm x block meta) /\ IndexStep StepLambdaTerm 0 /\ SyntaxTermId x' -> SyntaxTerm (LambdaTerm x' block meta)
  SyntaxTerm (LambdaTerm x block meta) /\ IndexStep StepLambdaTerm 1 /\ SyntaxBlock block' -> SyntaxTerm (LambdaTerm x block' meta)
  SyntaxTerm (NeutralTerm x argItems meta) /\ IndexStep StepNeutralTerm 0 /\ SyntaxTermId x' -> SyntaxTerm (NeutralTerm x' argItems meta)
  SyntaxTerm (NeutralTerm x argItems meta) /\ IndexStep StepNeutralTerm 1 /\ SyntaxList args' -> SyntaxTerm (NeutralTerm x (toArgItem <$> args') meta)
  SyntaxTerm (MatchTerm typeId a caseItems meta) /\ IndexStep StepMatchTerm 0 /\ SyntaxTerm a' -> SyntaxTerm (MatchTerm typeId a' caseItems meta)
  SyntaxTerm (MatchTerm typeId a caseItems meta) /\ IndexStep StepMatchTerm 1 /\ SyntaxList cases' -> SyntaxTerm (MatchTerm typeId a (toCaseItem <$> cases') meta)
  SyntaxCase (Case xItems block meta) /\ IndexStep StepCase 0 /\ SyntaxList xs -> SyntaxCase (Case (toTermIdItem <$> xs) block meta)
  SyntaxCase (Case xItems block meta) /\ IndexStep StepCase 1 /\ SyntaxBlock block' -> SyntaxCase (Case xItems block' meta)
  SyntaxParameter (Parameter alpha meta) /\ IndexStep StepParameter 0 /\ SyntaxType alpha' -> SyntaxParameter (Parameter alpha' meta)
  -- items
  SyntaxDefinitionItem (def /\ meta) /\ IndexStep StepDefinitionItem 0 /\ SyntaxDefinition def' -> SyntaxDefinitionItem (def' /\ meta)
  SyntaxConstructorItem (constr /\ meta) /\ IndexStep StepConstructorItem 0 /\ SyntaxConstructor constr' -> SyntaxConstructorItem (constr' /\ meta)
  SyntaxCaseItem (case_ /\ meta) /\ IndexStep StepCaseItem 0 /\ SyntaxCase case' -> SyntaxCaseItem (case' /\ meta)
  SyntaxParameterItem (param /\ meta) /\ IndexStep StepParameterItem 0 /\ SyntaxParameter param' -> SyntaxParameterItem (param' /\ meta)
  SyntaxArgItem (arg /\ meta) /\ IndexStep StepArgItem 0 /\ SyntaxTerm arg' -> SyntaxArgItem (arg' /\ meta)
  SyntaxTermIdItem (termId /\ meta) /\ IndexStep StepTermIdItem 0 /\ SyntaxTermId termId' -> SyntaxTermIdItem (termId' /\ meta)
  -- list
  SyntaxList (Cons h t) /\ IndexStep StepCons 0 /\ h' -> SyntaxList (Cons h' t)
  SyntaxList (Cons h t) /\ IndexStep StepCons 1 /\ SyntaxList t' -> SyntaxList (Cons h t')
  _ -> unsafeCrashWith $ "wrapStepSyntax: impossible: \nstep: " <> show step <> "\nsyn: " <> show syn <> "\nsynSub: " <> show synSub

lookupSyntaxAt :: DownwardIndex -> Syntax -> Syntax
lookupSyntaxAt ix syn = case unconsDownwardIndex ix of
  Nothing -> syn
  Just { step, ix' } -> lookupSyntaxAt ix' (stepSyntax step syn)

modifySyntaxAt :: DownwardIndex -> (Syntax -> Syntax) -> Syntax -> Syntax
modifySyntaxAt ix f syn = case unconsDownwardIndex ix of
  Nothing -> f syn
  Just { step, ix' } -> wrapStepSyntax step syn $ modifySyntaxAt ix' f (stepSyntax step syn)

modifySyntaxAtM :: forall m. Monad m => DownwardIndex -> (Syntax -> m Syntax) -> Syntax -> m Syntax
modifySyntaxAtM ix f syn = case unconsDownwardIndex ix of
  Nothing -> f syn
  Just { step, ix' } -> wrapStepSyntax step syn <$> modifySyntaxAtM ix' f (stepSyntax step syn)
