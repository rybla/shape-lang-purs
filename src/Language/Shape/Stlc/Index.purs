module Language.Shape.Stlc.Index where

import Data.List.Unsafe
import Data.Tuple.Nested
import Language.Shape.Stlc.Syntax
import Prelude

import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol)
import Data.Unfoldable (replicate)
import Debug as Debug
import Language.Shape.Stlc.Metadata as Meta
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe as Unsafe

-- with this new index that has a constant number of children for each StepLabel, going up is same, left/right is much easier now, and going down still requires a case-by-case check to see if the current node supports going downward (always to the 0 child) i.e. it is not a terminal node of the AST

newtype UpwardIndex
  = UpwardIndex (List IndexStep)

newtype DownwardIndex
  = DownwardIndex (List IndexStep)

instance Semigroup UpwardIndex where 
  append (UpwardIndex steps1) (UpwardIndex steps2) = UpwardIndex (steps2 <> steps1)

derive newtype instance Semigroup DownwardIndex

instance showUpwardIndex :: Show UpwardIndex where show (UpwardIndex steps) = show steps
instance showDownwardIndex :: Show DownwardIndex where show (DownwardIndex steps) = show steps

derive instance Generic UpwardIndex _ 
derive instance Generic DownwardIndex _ 

pushDownwardIndex :: IndexStep -> DownwardIndex -> DownwardIndex
pushDownwardIndex step (DownwardIndex steps) = DownwardIndex $ Cons step steps

infixr 6 pushDownwardIndex as -:

snocDownwardIndex :: DownwardIndex -> IndexStep -> DownwardIndex
snocDownwardIndex (DownwardIndex steps) step = DownwardIndex $ snoc steps step 

pushUpwardIndex :: UpwardIndex -> IndexStep -> UpwardIndex
pushUpwardIndex (UpwardIndex steps) step = UpwardIndex $ Cons step steps

infixl 6 pushUpwardIndex as :-

toUpwardIndex :: DownwardIndex -> UpwardIndex
toUpwardIndex (DownwardIndex steps) = UpwardIndex $ reverse steps

toDownwardIndex :: UpwardIndex -> DownwardIndex
toDownwardIndex (UpwardIndex steps) = DownwardIndex $ reverse steps

emptyDownwardIndex :: DownwardIndex
emptyDownwardIndex = DownwardIndex Nil

unconsDownwardIndex :: DownwardIndex -> Maybe { step :: IndexStep, ix' :: DownwardIndex }
unconsDownwardIndex (DownwardIndex steps) = do
  { head: step, tail: steps' } <- uncons steps
  Just { step, ix': DownwardIndex steps' }

unsnocDownwardIndex :: DownwardIndex -> Maybe { ix' :: DownwardIndex, step :: IndexStep }
unsnocDownwardIndex (DownwardIndex steps) = do 
  { init: steps', last: step } <- unsnoc steps 
  Just $ { ix': DownwardIndex steps' , step }

unconsUpwardIndex :: UpwardIndex -> Maybe { step :: IndexStep, ix' :: UpwardIndex }
unconsUpwardIndex (UpwardIndex ix) = do
  { head: step, tail: steps' } <- uncons ix
  Just { step, ix': UpwardIndex steps' }

data StepLabel
  = StepModule
  | StepBlock
  | StepTermDefinition
  | StepDataDefinition
  | StepConstructor
  | StepArrowType
  | StepDataType
  | StepHoleType
  | StepLambdaTerm
  | StepNeutralTerm
  | StepMatchTerm
  | StepCase
  | StepParameter
  -- item
  | StepDefinitionItem
  | StepConstructorItem
  | StepParameterItem
  | StepCaseItem
  | StepArgItem
  | StepTermIdItem
  -- list
  | StepCons
  | StepNil


derive instance Generic StepLabel _ 
instance Eq StepLabel where eq l1 l2 = genericEq l1 l2
instance Show StepLabel where show l = genericShow l

data IndexStep
  = IndexStep StepLabel Int

derive instance Generic IndexStep _ 
instance Eq IndexStep where eq ix1 ix2 = genericEq ix1 ix2
instance Show IndexStep where show (IndexStep l i) = show l <> "(" <> show i <> ")"

-- i^th element of a cons-list
fromListIndexToDownwardIndex :: Int -> DownwardIndex 
fromListIndexToDownwardIndex i = 
  DownwardIndex $ replicate i (IndexStep StepCons 1) `snoc` IndexStep StepCons 0

-- i^th element of a cons-list
fromListIndexToUpwardIndex :: Int -> UpwardIndex
fromListIndexToUpwardIndex i = 
  UpwardIndex (replicate i (IndexStep StepCons 1)) :- IndexStep StepCons 0

-- i^th sublist of a cons-list
fromSublistIndexToDownwardIndex :: Int -> DownwardIndex 
fromSublistIndexToDownwardIndex i = 
  DownwardIndex $ replicate i (IndexStep StepCons 1)

-- i^th sublist of a cons-list
fromSublistIndexToUpwardIndex :: Int -> UpwardIndex 
fromSublistIndexToUpwardIndex i = 
  UpwardIndex $ replicate i (IndexStep StepCons 1)

singletonDownwardIndex :: IndexStep -> DownwardIndex 
singletonDownwardIndex = DownwardIndex <<< singleton

singletonUpwardIndex :: IndexStep -> UpwardIndex 
singletonUpwardIndex = UpwardIndex <<< singleton

childrenCount :: StepLabel -> Int
childrenCount = case _ of 
  StepModule -> 1
  StepBlock -> 2
  StepTermDefinition -> 3
  StepDataDefinition -> 2
  StepConstructor -> 2
  StepArrowType -> 2
  StepDataType -> 0
  StepHoleType -> 0
  StepLambdaTerm -> 2
  StepNeutralTerm -> 2
  StepMatchTerm -> 2
  StepCase -> 2
  StepParameter -> 1
  -- item
  StepDefinitionItem -> 1
  StepConstructorItem -> 1
  StepParameterItem -> 1
  StepCaseItem -> 1
  StepArgItem -> 1
  StepTermIdItem -> 1
  -- list 
  StepCons -> 2
  StepNil -> 0

stepSyntax :: IndexStep -> Syntax -> Syntax
stepSyntax step syn = case syn /\ step of 
  SyntaxModule (Module defItems meta) /\ IndexStep StepModule 0 ->  SyntaxList $ SyntaxDefinitionItem <$> defItems
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
  SyntaxDefinition (TermDefinition x alpha a meta) /\ IndexStep StepTermDefinition 1 /\ SyntaxType alpha'  -> SyntaxDefinition (TermDefinition x alpha' a meta)
  SyntaxDefinition (TermDefinition x alpha a meta) /\ IndexStep StepTermDefinition 2 /\ SyntaxTerm a' -> SyntaxDefinition (TermDefinition x alpha a' meta)
  SyntaxDefinition (DataDefinition t constrItems meta) /\ IndexStep StepDataDefinition 0 /\ SyntaxTypeBinding t' -> SyntaxDefinition (DataDefinition t' constrItems meta)
  SyntaxDefinition (DataDefinition t constrItems meta) /\ IndexStep StepDataDefinition 1 /\ SyntaxList constrs' -> SyntaxDefinition (DataDefinition t (toConstructorItem <$> constrs') meta)
  SyntaxConstructor (Constructor x paramItems meta) /\ IndexStep StepConstructor 0 /\ SyntaxTermBinding x' -> SyntaxConstructor (Constructor x' paramItems meta)
  SyntaxConstructor (Constructor x paramItems meta) /\ IndexStep StepConstructor 1 /\ SyntaxList params' -> SyntaxConstructor (Constructor x (toParameterItem <$> params') meta)
  SyntaxType (ArrowType param beta meta) /\ IndexStep StepArrowType 0 /\ SyntaxParameter param' -> SyntaxType (ArrowType param' beta meta)
  SyntaxType (ArrowType param beta meta) /\ IndexStep StepArrowType 1 /\SyntaxType beta' -> SyntaxType (ArrowType param beta' meta)
  SyntaxTerm (LambdaTerm x block meta) /\ IndexStep StepLambdaTerm 0 /\ SyntaxTermId x' -> SyntaxTerm (LambdaTerm x' block meta)
  SyntaxTerm (LambdaTerm x block meta) /\ IndexStep StepLambdaTerm 1 /\ SyntaxBlock block'-> SyntaxTerm (LambdaTerm x block' meta)
  SyntaxTerm (NeutralTerm x argItems meta) /\ IndexStep StepNeutralTerm 0 /\ SyntaxTermId x' -> SyntaxTerm (NeutralTerm x' argItems meta)
  SyntaxTerm (NeutralTerm x argItems meta) /\ IndexStep StepNeutralTerm 1 /\ SyntaxList args' -> SyntaxTerm (NeutralTerm x (toArgItem <$> args') meta)
  SyntaxTerm (MatchTerm typeId a caseItems meta) /\ IndexStep StepMatchTerm 0 /\ SyntaxTerm a' -> SyntaxTerm (MatchTerm typeId a' caseItems meta)
  SyntaxTerm (MatchTerm typeId a caseItems meta) /\ IndexStep StepMatchTerm 1 /\ SyntaxList cases' -> SyntaxTerm (MatchTerm typeId a (toCaseItem <$> cases') meta)
  SyntaxCase (Case xItems block meta) /\ IndexStep StepCase 0 /\ SyntaxList xs -> SyntaxCase (Case (toTermIdItem <$> xs) block meta)
  SyntaxCase (Case xItems block meta) /\ IndexStep StepCase 1  /\ SyntaxBlock block' -> SyntaxCase (Case xItems block' meta)
  SyntaxParameter (Parameter alpha meta) /\ IndexStep StepParameter 0 /\ SyntaxType alpha' -> SyntaxParameter (Parameter alpha' meta)
  -- items
  SyntaxDefinitionItem (def /\ meta) /\ IndexStep StepDefinitionItem 0 /\ SyntaxDefinition def' -> SyntaxDefinitionItem (def' /\ meta )
  SyntaxConstructorItem (constr /\ meta) /\ IndexStep StepConstructorItem 0 /\ SyntaxConstructor constr' -> SyntaxConstructorItem (constr' /\ meta )
  SyntaxCaseItem (case_ /\ meta) /\ IndexStep StepCaseItem 0 /\ SyntaxCase case'  -> SyntaxCaseItem (case' /\ meta )
  SyntaxParameterItem (param /\ meta) /\ IndexStep StepParameterItem 0 /\ SyntaxParameter param' -> SyntaxParameterItem (param' /\ meta)
  SyntaxArgItem (arg /\ meta) /\ IndexStep StepArgItem 0 /\ SyntaxTerm arg' -> SyntaxArgItem (arg' /\ meta)
  SyntaxTermIdItem (termId /\ meta) /\ IndexStep StepTermIdItem 0 /\ SyntaxTermId termId' -> SyntaxTermIdItem (termId' /\ meta)
  -- list
  SyntaxList (Cons h t) /\ IndexStep StepCons 0 /\ h' -> SyntaxList (Cons h' t)
  SyntaxList (Cons h t) /\ IndexStep StepCons 1 /\ SyntaxList t' -> SyntaxList (Cons h t')
  _ -> unsafeCrashWith $ "wrapStepSyntax: impossible: \nstep: " <> show step <> "\nsyn: " <> show syn <> "\nsynSub: " <> show synSub

lookupSyntaxAt :: DownwardIndex -> Syntax -> Syntax
lookupSyntaxAt ix syn = 
  case unconsDownwardIndex ix of 
    Nothing -> 
      syn
    Just {step, ix'} ->
      lookupSyntaxAt ix' (stepSyntax step syn)

modifySyntaxAt :: DownwardIndex -> (Syntax -> Syntax) -> Syntax -> Syntax
modifySyntaxAt ix f syn = 
  case unconsDownwardIndex ix of 
    Nothing -> f syn
    Just {step, ix'} -> wrapStepSyntax step syn $ modifySyntaxAt ix' f (stepSyntax step syn)

modifySyntaxAtM :: forall m. Monad m => DownwardIndex -> (Syntax -> m Syntax) -> Syntax -> m Syntax
modifySyntaxAtM ix f syn = 
  case unconsDownwardIndex ix of 
    Nothing -> f syn
    Just {step, ix'} -> wrapStepSyntax step syn <$> modifySyntaxAtM ix' f (stepSyntax step syn)
