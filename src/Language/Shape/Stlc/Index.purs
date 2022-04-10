module Language.Shape.Stlc.Index where

import Data.List.Unsafe
import Data.Tuple.Nested
import Prelude
import Data.Array as Array
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String as String
import Data.Symbol (class IsSymbol)
import Data.Unfoldable (replicate)
import Debug as Debug
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

instance semigroupUpwardIndex :: Semigroup UpwardIndex where
  append (UpwardIndex steps1) (UpwardIndex steps2) = UpwardIndex (steps2 <> steps1)

derive newtype instance semigroupDownwardIndex :: Semigroup DownwardIndex

instance showUpwardIndex :: Show UpwardIndex where
  show x = genericShow x

instance showDownwardIndex :: Show DownwardIndex where
  show x = genericShow x

derive instance genericUpwardIndex :: Generic UpwardIndex _

derive instance genericDownwardIndex :: Generic DownwardIndex _

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
  Just $ { ix': DownwardIndex steps', step }

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

derive instance genericStepLabel :: Generic StepLabel _

instance eqStepLabel :: Eq StepLabel where
  eq l1 l2 = genericEq l1 l2

instance showStepLabel :: Show StepLabel where
  show l = genericShow l

data IndexStep
  = IndexStep StepLabel Int

derive instance genericStepIndex :: Generic IndexStep _

instance eqStepIndex :: Eq IndexStep where
  eq ix1 ix2 = genericEq ix1 ix2

instance showStepIndex :: Show IndexStep where
  show step = genericShow step

-- i^th element of a cons-list
fromListIndexToDownwardIndex :: Int -> DownwardIndex
fromListIndexToDownwardIndex i = DownwardIndex $ replicate i (IndexStep StepCons 1) `snoc` IndexStep StepCons 0

-- i^th element of a cons-list
fromListIndexToUpwardIndex :: Int -> UpwardIndex
fromListIndexToUpwardIndex i = UpwardIndex (replicate i (IndexStep StepCons 1)) :- IndexStep StepCons 0

-- i^th sublist of a cons-list
fromSublistIndexToDownwardIndex :: Int -> DownwardIndex
fromSublistIndexToDownwardIndex i = DownwardIndex $ replicate i (IndexStep StepCons 1)

-- i^th sublist of a cons-list
fromSublistIndexToUpwardIndex :: Int -> UpwardIndex
fromSublistIndexToUpwardIndex i = UpwardIndex $ replicate i (IndexStep StepCons 1)

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
