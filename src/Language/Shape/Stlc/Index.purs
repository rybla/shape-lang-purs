module Language.Shape.Stlc.Index where

import Data.List.Unsafe
import Data.Tuple.Nested
import Language.Shape.Stlc.Syntax
import Prelude
import Prim.Row as Row
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol)
import Data.Unfoldable (replicate)
import Debug as Debug
import Language.Shape.Stlc.Metadata as Meta
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

instance Show UpwardIndex where show (UpwardIndex steps) = show steps
instance Show DownwardIndex where show (DownwardIndex steps) = show steps

pushDownwardIndex :: IndexStep -> DownwardIndex -> DownwardIndex
pushDownwardIndex step (DownwardIndex steps) = DownwardIndex $ Cons step steps

infixr 6 pushDownwardIndex as -:

pushUpwardIndex :: UpwardIndex -> IndexStep -> UpwardIndex
pushUpwardIndex (UpwardIndex steps) step = UpwardIndex $ Cons step steps

infixr 6 pushUpwardIndex as :-

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
  SyntaxTermIdItem (termId /\ meta) /\ IndexStep StepTermIdItem 0 -> SyntaxTermId termId
  -- list
  SyntaxList (Cons h t) /\ IndexStep StepCons 0 -> h
  SyntaxList (Cons h t) /\ IndexStep StepCons 1 -> SyntaxList t
  _ -> Unsafe.error $ "stepSyntax: impossible: " <> show (step /\ syn)

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
  SyntaxTermIdItem (termId /\ meta) /\ IndexStep StepTermIdItem 0 /\ SyntaxTermId termId' -> SyntaxTermIdItem (termId' /\ meta)
  -- list
  SyntaxList (Cons h t) /\ IndexStep StepCons 0 /\ h' -> SyntaxList (Cons h' t)
  SyntaxList (Cons h t) /\ IndexStep StepCons 1 /\ SyntaxList t' -> SyntaxList (Cons h t')
  _ -> Unsafe.error $ "wrapStepSyntax: impossible: " <> show (step /\ syn /\ synSub)

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

-- data Direction
--   = Up
--   | Down
--   | Left
--   | Right

-- moveDownwardIndex :: Direction -> Module -> DownwardIndex -> DownwardIndex
-- moveDownwardIndex dir mod ixDownward = 
--   let ixUpward = toUpwardIndex ixDownward in
--   case dir of 
--     Up ->
--       case unconsUpwardIndex ixUpward of 
--         Nothing -> ixDownward 
--         Just {ix', step} -> toDownwardIndex ix'
--     Down ->
--       case 
--     Left  -> undefined
--     Right -> undefined

{-
inListBounds :: forall a. Int -> List a -> Boolean
inListBounds i ls = 0 <= i && i < Conslength ls

inBounds :: Int -> Int -> Int -> Boolean
inBounds i min max = min <= i && i <= max

lookupSyntaxAt :: DownwardIndex -> Syntax -> Syntax
lookupSyntaxAt ix syn =
  case unconsDownwardIndex ix of
    Nothing -> syn
    Just { ix', step } -> case syn /\ step of
      SyntaxModule (Module defs meta) /\ IndexStep StepModule i
        | inListBounds i defs -> lookupSyntaxAt ix' $ SyntaxDefinition (Consindex' defs i)
      SyntaxBlock (Block defs a meta) /\ IndexStep StepBlock i
        | inListBounds i defs -> lookupSyntaxAt ix' $ SyntaxDefinition (Consindex' defs i)
        | i == Conslength defs -> lookupSyntaxAt ix' $ SyntaxTerm a
      SyntaxDefinition (TermDefinition termBinding alpha a meta) /\ IndexStep StepTermDefinition i
        | i == 0 -> lookupSyntaxAt ix' $ SyntaxTermBinding termBinding
        | i == 1 -> lookupSyntaxAt ix' $ SyntaxType alpha
        | i == 2 -> lookupSyntaxAt ix' $ SyntaxTerm a
      SyntaxDefinition (DataDefinition typeBinding constrs meta) /\ IndexStep StepDataDefinition i
        | i == 0 -> lookupSyntaxAt ix' $ SyntaxTypeBinding typeBinding
        | inListBounds (i - 1) constrs -> lookupSyntaxAt ix' $ SyntaxConstructor $ Consindex' constrs (i - 1)
      SyntaxConstructor (Constructor termBinding params meta) /\ IndexStep StepConstructor i
        | i == 0 -> lookupSyntaxAt ix' $ SyntaxTermBinding termBinding
        | inListBounds (i - 1) params -> lookupSyntaxAt ix' $ SyntaxParameter $ Consindex' params i
      SyntaxTerm (LambdaTerm termId block meta) /\ IndexStep StepLambdaTerm i
        | i == 0 -> lookupSyntaxAt ix' $ SyntaxTermId termId
        | i == 1 -> lookupSyntaxAt ix' $ SyntaxBlock block
      SyntaxTerm (MatchTerm typeId term cases meta) /\ IndexStep StepMatchTerm i
        | i == 0 -> lookupSyntaxAt ix' $ SyntaxTerm term
        | inListBounds (i - 1) cases -> lookupSyntaxAt ix' $ SyntaxCase $ Consindex' cases (i - 1)
      SyntaxTerm (NeutralTerm termId args meta) /\ IndexStep StepNeutralTerm i
        | i == 0 -> lookupSyntaxAt ix' $ SyntaxTermId termId
        | i == 1 -> lookupSyntaxAt ix' $ SyntaxArgs args
      SyntaxArgs (ConsArgs a args meta) /\ IndexStep StepConsArgs i
        | i == 0 -> lookupSyntaxAt ix' $ SyntaxTerm a
        | i == 1 -> lookupSyntaxAt ix' $ SyntaxArgs args
      SyntaxCase (Case termIds term meta) /\ IndexStep StepCase i
        | inListBounds i termIds -> lookupSyntaxAt ix' $ SyntaxTermId (Consindex' termIds i)
        | i == Conslength termIds -> lookupSyntaxAt ix' $ SyntaxTerm term
      SyntaxType (ArrowType param beta meta) /\ IndexStep StepArrowType i
        | i == 0 -> lookupSyntaxAt ix' $ SyntaxParameter param
        | i == 1 -> lookupSyntaxAt ix' $ SyntaxType beta
      SyntaxParameter (Parameter alpha meta) /\ IndexStep StepParameter i
        | i == 0 -> lookupSyntaxAt ix' $ SyntaxType alpha
      _ -> Unsafe.error "lookupSyntaxAt: impossible"

modifyIndexAt :: DownwardIndex -> (Syntax -> Syntax) -> Syntax -> Syntax
modifyIndexAt ix f syn = case unconsDownwardIndex ix of
  Nothing -> f syn
  Just { step, ix' } -> case syn /\ step of
    SyntaxModule (Module defs meta) /\ IndexStep StepModule i
      | inListBounds i defs -> SyntaxModule $ Module (ConsmodifyAt' i (toDefinition <<< modifyIndexAt ix' f <<< SyntaxDefinition) defs) meta
    SyntaxBlock (Block defs a meta) /\ IndexStep StepBlock i
      | inListBounds i defs -> SyntaxBlock $ Block (ConsmodifyAt' i (toDefinition <<< modifyIndexAt ix' f <<< SyntaxDefinition) defs) a meta
      | i == Conslength defs -> SyntaxBlock $ Block defs (toTerm $ modifyIndexAt ix' f $ SyntaxTerm a) meta
    SyntaxDefinition (TermDefinition termBinding alpha a meta) /\ IndexStep StepTermDefinition i
      | i == 0 -> SyntaxDefinition $ TermDefinition (toTermBinding $ modifyIndexAt ix' f $ SyntaxTermBinding termBinding) alpha a meta
      | i == 1 -> SyntaxDefinition $ TermDefinition termBinding (toType $ modifyIndexAt ix' f $ SyntaxType alpha) a meta
      | i == 2 -> SyntaxDefinition $ TermDefinition termBinding alpha (toTerm $ modifyIndexAt ix' f $ SyntaxTerm a) meta
    SyntaxDefinition (DataDefinition typeBinding constrs meta) /\ IndexStep StepDataDefinition i
      | i == 0 -> SyntaxDefinition (DataDefinition (toTypeBinding $ modifyIndexAt ix' f $ SyntaxTypeBinding typeBinding) constrs meta)
      | inListBounds (i - 1) constrs -> SyntaxDefinition (DataDefinition typeBinding (ConsmodifyAt' (i - 1) (toConstructor <<< modifyIndexAt ix' f <<< SyntaxConstructor) constrs) meta)
    SyntaxConstructor (Constructor termBinding params meta) /\ IndexStep StepConstructor i
      | i == 0 -> SyntaxConstructor (Constructor (toTermBinding $ modifyIndexAt ix' f $ SyntaxTermBinding termBinding) params meta)
      | inListBounds (i - 1) params -> SyntaxConstructor (Constructor termBinding (ConsmodifyAt' (i - 1) (toParameter <<< modifyIndexAt ix' f <<< SyntaxParameter) params) meta)
    SyntaxTerm (LambdaTerm termId block meta) /\ IndexStep StepLambdaTerm i
      | i == 0 -> SyntaxTerm (LambdaTerm (toTermId $ modifyIndexAt ix' f $ SyntaxTermId termId) block meta)
      | i == 1 -> SyntaxTerm (LambdaTerm termId (toBlock $ modifyIndexAt ix' f $ SyntaxBlock block) meta)
    SyntaxTerm (MatchTerm typeId term cases meta) /\ IndexStep StepMatchTerm i
      | i == 0 -> SyntaxTerm (MatchTerm typeId (toTerm $ modifyIndexAt ix' f $ SyntaxTerm term) cases meta)
      | inListBounds (i - 1) cases -> SyntaxTerm (MatchTerm typeId term (ConsmodifyAt' (i - 1) (toCase <<< modifyIndexAt ix' f <<< SyntaxCase) cases) meta)
    SyntaxTerm (NeutralTerm termId args meta) /\ IndexStep StepNeutralTerm i
      | i == 0 -> SyntaxTerm (NeutralTerm (toTermId $ modifyIndexAt ix' f $ SyntaxTermId termId) args meta)
      | i == 1 -> SyntaxTerm (NeutralTerm termId (toArgs $ modifyIndexAt ix' f $ SyntaxArgs args) meta)
    SyntaxArgs (ConsArgs a args meta) /\ IndexStep StepConsArgs i
      | i == 0 -> SyntaxArgs (ConsArgs (toTerm $ modifyIndexAt ix' f $ SyntaxTerm a) args meta)
      | i == 1 -> SyntaxArgs (ConsArgs a (toArgs $ modifyIndexAt ix' f $ SyntaxArgs args) meta)
    SyntaxCase (Case termIds term meta) /\ IndexStep StepCase i
      | inListBounds i termIds -> SyntaxCase (Case (ConsmodifyAt' i (toTermId <<< modifyIndexAt ix' f <<< SyntaxTermId) termIds) term meta)
      | i == Conslength termIds -> SyntaxCase (Case termIds (toTerm $ modifyIndexAt ix' f $ SyntaxTerm term) meta)
    SyntaxType (ArrowType param beta meta) /\ IndexStep StepArrowType i
      | i == 0 -> SyntaxType (ArrowType (toParameter $ modifyIndexAt ix' f $ SyntaxParameter param) beta meta)
      | i == 1 -> SyntaxType (ArrowType param (toType $ modifyIndexAt ix' f $ SyntaxType beta) meta)
    SyntaxParameter (Parameter alpha meta) /\ IndexStep StepParameter i
      | i == 0 -> SyntaxParameter (Parameter (toType $ modifyIndexAt ix' f $ SyntaxType alpha) meta)
    _ -> Unsafe.error "modifyIndexAt: impossible"


data Direction
  = Up
  | Down
  | Left
  | Right

moveDownwardIndex :: Direction -> Module -> DownwardIndex -> DownwardIndex
moveDownwardIndex dir mod ix = 
  let
    ixUpward = toUpwardIndex ix
  in
    case dir of
      Up -> case unconsUpwardIndex ixUpward of
        Nothing -> ix
        Just { ix' } -> toDownwardIndex ix'
      Down ->
        case lookupSyntaxAt ix (SyntaxModule mod) of
          SyntaxModule (Module defs _) -> if Conslength defs > 0 then toDownwardIndex $ ixUpward :- IndexStep StepModule 0 else ix
          SyntaxBlock (Block defs _ _) -> toDownwardIndex $ ixUpward :- IndexStep StepBlock 0
          SyntaxDefinition (TermDefinition _ _ _ _) -> toDownwardIndex $ ixUpward :- IndexStep StepTermDefinition 0
          SyntaxDefinition (DataDefinition _ _ _) -> toDownwardIndex $ ixUpward :- IndexStep StepDataDefinition 0
          SyntaxConstructor (Constructor _ _ _) -> toDownwardIndex $ ixUpward :- IndexStep StepConstructor 0
          SyntaxTerm (LambdaTerm _ _ _) -> toDownwardIndex $ ixUpward :- IndexStep StepLambdaTerm 0
          SyntaxTerm (HoleTerm _) -> ix
          SyntaxTerm (MatchTerm _ _ _ _) -> toDownwardIndex $ ixUpward :- IndexStep StepMatchTerm 0
          SyntaxTerm (NeutralTerm _ _ _) -> toDownwardIndex $ ixUpward :- IndexStep StepNeutralTerm 0
          SyntaxArgs NoneArgs -> ix
          SyntaxArgs (ConsArgs _ _ _) -> toDownwardIndex $ ixUpward :- IndexStep StepConsArgs 0
          SyntaxCase (Case _ _ _) -> toDownwardIndex $ ixUpward :- IndexStep StepCase 0
          SyntaxType (ArrowType _ _ _) -> toDownwardIndex $ ixUpward :- IndexStep StepArrowType 0
          SyntaxType (DataType _ _) -> ix 
          SyntaxType (HoleType _ _ _) -> ix 
          SyntaxType (ProxyHoleType _) -> ix 
          SyntaxParameter (Parameter _ _) -> toDownwardIndex $ ixUpward :- IndexStep StepParameter 0
          SyntaxTermBinding _ -> ix 
          SyntaxTypeBinding _ -> ix 
          SyntaxTermId _ -> ix 
      Left ->
        case unconsUpwardIndex ixUpward of
          Nothing -> ix
          Just { ix', step } ->
            let
              syn = lookupSyntaxAt (toDownwardIndex ix') (SyntaxModule mod)
            in
              case syn /\ step of
                SyntaxModule (Module defs _) /\ IndexStep StepModule i
                  | inBounds (i - 1) 0 (Conslength defs - 1) -> toDownwardIndex $ ix' :- IndexStep StepModule (i - 1)
                  | otherwise -> ix
                SyntaxBlock (Block defs _ _) /\ IndexStep StepBlock i
                  | inBounds (i - 1) 0 (Conslength defs) -> toDownwardIndex $ ix' :- IndexStep StepBlock (i - 1)
                  | otherwise -> ix
                SyntaxDefinition (TermDefinition _ _ _ _) /\ IndexStep StepTermDefinition i
                  | inBounds (i - 1) 0 2 -> toDownwardIndex $ ix' :- IndexStep StepTermDefinition (i - 1)
                  | otherwise -> ix
                SyntaxDefinition (DataDefinition _ constrs _) /\ IndexStep StepDataDefinition i
                  | inBounds (i - 1) 0 (Conslength constrs) -> toDownwardIndex $ ix' :- IndexStep StepDataDefinition (i - 1)
                  | otherwise -> ix
                SyntaxConstructor (Constructor _ params _) /\ IndexStep StepConstructor i
                  | inBounds (i - 1) 0 (Conslength params) -> toDownwardIndex $ ix' :- IndexStep StepConstructor (i - 1)
                  | otherwise -> ix
                SyntaxTerm (LambdaTerm _ _ _) /\ IndexStep StepLambdaTerm i
                  | inBounds (i - 1) 0 1 -> toDownwardIndex $ ix' :- IndexStep StepLambdaTerm (i - 1)
                  | otherwise -> ix
                SyntaxTerm (MatchTerm _ _ cases _) /\ IndexStep StepMatchTerm i
                  | inBounds (i - 1) 0 (Conslength cases) -> toDownwardIndex $ ix' :- IndexStep StepMatchTerm (i - 1)
                  | otherwise -> ix
                SyntaxTerm (NeutralTerm _ _ _) /\ IndexStep StepNeutralTerm i
                  | inBounds (i - 1) 0 1 -> toDownwardIndex $ ix' :- IndexStep StepNeutralTerm (i - 1)
                  | otherwise -> ix
                SyntaxArgs (ConsArgs _ _ _) /\ IndexStep StepConsArgs i
                  | inBounds (i - 1) 0 1 -> toDownwardIndex $ ix' :- IndexStep StepConsArgs (i - 1)
                  | otherwise -> ix
                SyntaxCase (Case termIds _ _) /\ IndexStep StepCase i
                  | inBounds (i - 1) 0 (Conslength termIds) -> toDownwardIndex $ ix' :- IndexStep StepCase (i - 1)
                  | otherwise -> ix
                SyntaxType (ArrowType _ _ _) /\ IndexStep StepArrowType i
                  | inBounds (i - 1) 0 1 -> toDownwardIndex $ ix' :- IndexStep StepArrowType (i - 1)
                  | otherwise -> ix
                SyntaxParameter (Parameter _ _) /\ IndexStep StepParameter i
                  | otherwise -> ix
                _ -> Unsafe.error "moveDownwardIndex.Right: impossible"
      Right ->
        case unconsUpwardIndex ixUpward of
          Nothing -> ix
          Just { ix', step } ->
            let
              syn = lookupSyntaxAt (toDownwardIndex ix') (SyntaxModule mod)
            in
              case syn /\ step of
                SyntaxModule (Module defs _) /\ IndexStep StepModule i
                  | inBounds (i + 1) 0 (Conslength defs - 1) -> toDownwardIndex $ ix' :- IndexStep StepModule (i + 1)
                  | otherwise -> ix
                SyntaxBlock (Block defs _ _) /\ IndexStep StepBlock i
                  | inBounds (i + 1) 0 (Conslength defs) -> toDownwardIndex $ ix' :- IndexStep StepBlock (i + 1)
                  | otherwise -> ix
                SyntaxDefinition (TermDefinition _ _ _ _) /\ IndexStep StepTermDefinition i
                  | inBounds (i + 1) 0 2 -> toDownwardIndex $ ix' :- IndexStep StepTermDefinition (i + 1)
                  | otherwise -> ix
                SyntaxDefinition (DataDefinition _ constrs _) /\ IndexStep StepDataDefinition i
                  | inBounds (i + 1) 0 (Conslength constrs) -> toDownwardIndex $ ix' :- IndexStep StepDataDefinition (i + 1)
                  | otherwise -> ix 
                SyntaxConstructor (Constructor _ params _) /\ IndexStep StepConstructor i
                  | inBounds (i + 1) 0 (Conslength params) -> toDownwardIndex $ ix' :- IndexStep StepConstructor (i + 1)
                  | otherwise -> ix
                SyntaxTerm (LambdaTerm _ _ _) /\ IndexStep StepLambdaTerm i
                  | inBounds (i + 1) 0 1  -> toDownwardIndex $ ix' :- IndexStep StepLambdaTerm (i + 1)
                  | otherwise -> ix
                SyntaxTerm (MatchTerm _ _ cases _) /\ IndexStep StepMatchTerm i
                  | inBounds (i + 1) 0 (Conslength cases) -> toDownwardIndex $ ix' :- IndexStep StepMatchTerm (i + 1)
                  | otherwise -> ix
                SyntaxTerm (NeutralTerm _ _ _) /\ IndexStep StepNeutralTerm i
                  | inBounds (i + 1) 0 1 -> toDownwardIndex $ ix' :- IndexStep StepNeutralTerm (i + 1)
                  | otherwise -> ix
                SyntaxArgs (ConsArgs _ _ _) /\ IndexStep StepConsArgs i
                  | inBounds (i + 1) 0 1  -> toDownwardIndex $ ix' :- IndexStep StepConsArgs (i + 1)
                  | otherwise -> ix
                SyntaxCase (Case termIds _ _) /\ IndexStep StepCase i
                  | inBounds (i + 1) 0 (Conslength termIds) -> toDownwardIndex $ ix' :- IndexStep StepCase (i + 1)
                  | otherwise -> ix
                SyntaxType (ArrowType _ _ _) /\ IndexStep StepArrowType i
                  | inBounds (i + 1) 0 1 -> toDownwardIndex $ ix' :- IndexStep StepArrowType (i + 1)
                  | otherwise -> ix
                SyntaxParameter (Parameter _ _) /\ IndexStep StepParameter i
                  | otherwise -> ix
                _ -> Unsafe.error "moveDownwardIndex.Left: impossible"
-}

toggleIndentedMetadataAt :: forall label. DownwardIndex -> Syntax -> Syntax 
toggleIndentedMetadataAt ix = case unsnocDownwardIndex ix of 
  Nothing -> identity -- cannot indent at top level
  Just { ix', step } ->
    modifySyntaxAt ix' \synParent -> case synParent /\ step of
      SyntaxModule (Module defItems meta) /\ IndexStep StepModule 0 -> SyntaxModule (Module defItems meta)
      SyntaxBlock (Block defItems a meta) /\ IndexStep StepBlock 0 -> SyntaxBlock (Block defItems a (Meta.toggle Meta._indented meta))
      SyntaxBlock (Block defItems a meta) /\ IndexStep StepBlock 1 -> SyntaxBlock (Block defItems a (Meta.toggle Meta._indented meta))
      SyntaxDefinition (TermDefinition x alpha a meta) /\ IndexStep StepTermDefinition 0 -> SyntaxDefinition (TermDefinition x alpha a (Meta.toggle Meta._indented meta))
      SyntaxDefinition (TermDefinition x alpha a meta) /\ IndexStep StepTermDefinition 1 -> SyntaxDefinition (TermDefinition x alpha a (Meta.toggle Meta._indented meta))
      SyntaxDefinition (TermDefinition x alpha a meta) /\ IndexStep StepTermDefinition 2 -> SyntaxDefinition (TermDefinition x alpha a (Meta.toggle Meta._indented meta))
      SyntaxDefinition (DataDefinition t constrItems meta) /\ IndexStep StepDataDefinition 0 -> SyntaxDefinition (DataDefinition t constrItems meta)
      SyntaxDefinition (DataDefinition t constrItems meta) /\ IndexStep StepDataDefinition 1 -> SyntaxDefinition (DataDefinition t constrItems meta)
      SyntaxConstructor (Constructor x paramItems meta) /\ IndexStep StepConstructor 0 -> SyntaxConstructor (Constructor x paramItems meta)
      SyntaxConstructor (Constructor x paramItems meta) /\ IndexStep StepConstructor 1 -> SyntaxConstructor (Constructor x paramItems meta)
      SyntaxType (ArrowType param beta meta) /\ IndexStep StepArrowType 0 -> SyntaxType (ArrowType param beta meta )
      SyntaxType (ArrowType param beta meta) /\ IndexStep StepArrowType 1 -> SyntaxType (ArrowType param beta meta)
      SyntaxTerm (LambdaTerm x block meta) /\ IndexStep StepLambdaTerm 0 -> SyntaxTerm (LambdaTerm x block (Meta.toggle Meta._indented meta))
      SyntaxTerm (LambdaTerm x block meta) /\ IndexStep StepLambdaTerm 1 -> SyntaxTerm (LambdaTerm x block (Meta.toggle Meta._indented meta))
      SyntaxTerm (NeutralTerm x argItems meta) /\ IndexStep StepNeutralTerm 0 -> SyntaxTerm (NeutralTerm x argItems meta)
      SyntaxTerm (NeutralTerm x argItems meta) /\ IndexStep StepNeutralTerm 1 -> SyntaxTerm (NeutralTerm x argItems meta)
      SyntaxTerm (MatchTerm typeId a caseItems meta) /\ IndexStep StepMatchTerm 0 -> SyntaxTerm (MatchTerm typeId a caseItems (Meta.toggle Meta._indented meta))
      SyntaxTerm (MatchTerm typeId a caseItems meta) /\ IndexStep StepMatchTerm 1 -> SyntaxTerm (MatchTerm typeId a caseItems (Meta.toggle Meta._indented meta))
      SyntaxCase (Case xItems block meta) /\ IndexStep StepCase 0 -> SyntaxCase (Case xItems block (Meta.toggle Meta._indented meta))
      SyntaxCase (Case xItems block meta) /\ IndexStep StepCase 1 -> SyntaxCase (Case xItems block (Meta.toggle Meta._indented meta))
      SyntaxParameter (Parameter alpha meta) /\ IndexStep StepParameter 0 -> SyntaxParameter (Parameter alpha meta)
      -- items
      SyntaxDefinitionItem (def /\ meta) /\ IndexStep StepDefinitionItem 0 -> SyntaxDefinitionItem (def /\ (Meta.toggle Meta._indented meta))
      SyntaxConstructorItem (constr /\ meta) /\ IndexStep StepConstructorItem 0 -> SyntaxConstructorItem (constr /\ (Meta.toggle Meta._indented meta))
      SyntaxCaseItem (case_ /\ meta) /\ IndexStep StepCaseItem 0 -> SyntaxCaseItem (case_ /\ (Meta.toggle Meta._indented meta))
      SyntaxParameterItem (param /\ meta) /\ IndexStep StepParameterItem 0 -> SyntaxParameterItem (param /\ (Meta.toggle Meta._indented meta))
      SyntaxTermIdItem (termId /\ meta) /\ IndexStep StepTermIdItem 0 -> SyntaxTermIdItem (termId /\ (Meta.toggle Meta._indented meta))
      -- list
      SyntaxList (Cons h t) /\ IndexStep StepCons 0 -> SyntaxList (Cons h t)
      SyntaxList (Cons h t) /\ IndexStep StepCons 1 -> SyntaxList (Cons h t)
      syn /\ step -> Unsafe.error $ "toggleIndentedMetadataAt: impossible: " <> show (step /\ syn)
