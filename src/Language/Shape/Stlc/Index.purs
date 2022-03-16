module Language.Shape.Stlc.Index where

import Data.Tuple.Nested
import Language.Shape.Stlc.Syntax
import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List.Unsafe (List)
import Data.List.Unsafe as List
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Debug as Debug
import Undefined (undefined)
import Unsafe as Unsafe

newtype UpwardIndex
  = UpwardIndex (List IndexStep)

newtype DownwardIndex
  = DownwardIndex (List IndexStep)

instance Show UpwardIndex where  show (UpwardIndex steps) = show steps
instance Show DownwardIndex where  show (DownwardIndex steps) = show steps

pushDownwardIndex :: IndexStep -> DownwardIndex -> DownwardIndex
pushDownwardIndex step (DownwardIndex steps) = DownwardIndex $ List.Cons step steps

infix 5 pushDownwardIndex as <:

pushUpwardIndex :: UpwardIndex -> IndexStep -> UpwardIndex
pushUpwardIndex (UpwardIndex steps) step = UpwardIndex $ List.Cons step steps

infix 5 pushUpwardIndex as :>

toUpwardIndex :: DownwardIndex -> UpwardIndex
toUpwardIndex (DownwardIndex steps) = UpwardIndex $ List.reverse steps

toDownwardIndex :: UpwardIndex -> DownwardIndex
toDownwardIndex (UpwardIndex steps) = DownwardIndex $ List.reverse steps

unconsDownwardIndex :: DownwardIndex -> Maybe { step :: IndexStep, ix' :: DownwardIndex }
unconsDownwardIndex (DownwardIndex steps) = do
  { head: step, tail: steps' } <- List.uncons steps
  Just { step, ix': DownwardIndex steps' }

unconsUpwardIndex :: UpwardIndex -> Maybe { step :: IndexStep, ix' :: UpwardIndex }
unconsUpwardIndex (UpwardIndex ix) = do
  { head: step, tail: steps' } <- List.uncons ix
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
  | StepHoleTerm
  | StepCase
  | StepConsArgs
  | StepParameter

derive instance Generic StepLabel _ 
instance Eq StepLabel where eq l1 l2 = genericEq l1 l2
instance Show StepLabel where show l = genericShow l

data IndexStep
  = IndexStep StepLabel Int

derive instance Generic IndexStep _ 
instance Eq IndexStep where eq ix1 ix2 = genericEq ix1 ix2
instance Show IndexStep where show (IndexStep l i) = show l <> "(" <> show i <> ")"

inListBounds :: forall a. Int -> List a -> Boolean
inListBounds i ls = 0 <= i && i < List.length ls

inBounds :: Int -> Int -> Int -> Boolean
inBounds i min max = min <= i && i <= max

lookupSyntaxAt :: DownwardIndex -> Syntax -> Syntax
lookupSyntaxAt ix syn =
  case unconsDownwardIndex ix of
    Nothing -> syn
    Just { ix', step } -> case syn /\ step of
      SyntaxModule (Module defs meta) /\ IndexStep StepModule i
        | inListBounds i defs -> lookupSyntaxAt ix' $ SyntaxDefinition (List.index' defs i)
      SyntaxBlock (Block defs a meta) /\ IndexStep StepBlock i
        | inListBounds i defs -> lookupSyntaxAt ix' $ SyntaxDefinition (List.index' defs i)
        | i == List.length defs -> lookupSyntaxAt ix' $ SyntaxTerm a
      SyntaxDefinition (TermDefinition termBinding alpha a meta) /\ IndexStep StepTermDefinition i
        | i == 0 -> lookupSyntaxAt ix' $ SyntaxTermBinding termBinding
        | i == 1 -> lookupSyntaxAt ix' $ SyntaxType alpha
        | i == 2 -> lookupSyntaxAt ix' $ SyntaxTerm a
      SyntaxDefinition (DataDefinition typeBinding constrs meta) /\ IndexStep StepDataDefinition i
        | i == 0 -> lookupSyntaxAt ix' $ SyntaxTypeBinding typeBinding
        | inListBounds (i - 1) constrs -> lookupSyntaxAt ix' $ SyntaxConstructor $ List.index' constrs (i - 1)
      SyntaxConstructor (Constructor termBinding prms meta) /\ IndexStep StepConstructor i
        | i == 0 -> lookupSyntaxAt ix' $ SyntaxTermBinding termBinding
        | inListBounds (i - 1) prms -> lookupSyntaxAt ix' $ SyntaxParameter $ List.index' prms i
      SyntaxTerm (LambdaTerm termId block meta) /\ IndexStep StepLambdaTerm i
        | i == 0 -> lookupSyntaxAt ix' $ SyntaxTermId termId
        | i == 1 -> lookupSyntaxAt ix' $ SyntaxBlock block
      SyntaxTerm (MatchTerm typeId term cases meta) /\ IndexStep StepMatchTerm i
        | i == 0 -> lookupSyntaxAt ix' $ SyntaxTerm term
        | inListBounds (i - 1) cases -> lookupSyntaxAt ix' $ SyntaxCase $ List.index' cases (i - 1)
      SyntaxTerm (NeutralTerm termId args meta) /\ IndexStep StepNeutralTerm i
        | i == 0 -> lookupSyntaxAt ix' $ SyntaxTermId termId
        | i == 1 -> lookupSyntaxAt ix' $ SyntaxArgs args
      SyntaxArgs (ConsArgs a args meta) /\ IndexStep StepConsArgs i
        | i == 0 -> lookupSyntaxAt ix' $ SyntaxTerm a
        | i == 1 -> lookupSyntaxAt ix' $ SyntaxArgs args
      SyntaxCase (Case termIds term meta) /\ IndexStep StepCase i
        | inListBounds i termIds -> lookupSyntaxAt ix' $ SyntaxTermId (List.index' termIds i)
        | i == List.length termIds -> lookupSyntaxAt ix' $ SyntaxTerm term
      SyntaxType (ArrowType prm beta meta) /\ IndexStep StepArrowType i
        | i == 0 -> lookupSyntaxAt ix' $ SyntaxParameter prm
        | i == 1 -> lookupSyntaxAt ix' $ SyntaxType beta
      SyntaxParameter (Parameter alpha meta) /\ IndexStep StepParameter i
        | i == 0 -> lookupSyntaxAt ix' $ SyntaxType alpha
      _ -> Unsafe.error "lookupSyntaxAt: impossible"

modifyIndexAt :: DownwardIndex -> (Syntax -> Syntax) -> Syntax -> Syntax
modifyIndexAt ix f syn = case unconsDownwardIndex ix of
  Nothing -> f syn
  Just { step, ix' } -> case syn /\ step of
    SyntaxModule (Module defs meta) /\ IndexStep StepModule i
      | inListBounds i defs -> SyntaxModule $ Module (List.modifyAt' i (toDefinition <<< modifyIndexAt ix' f <<< SyntaxDefinition) defs) meta
    SyntaxBlock (Block defs a meta) /\ IndexStep StepBlock i
      | inListBounds i defs -> SyntaxBlock $ Block (List.modifyAt' i (toDefinition <<< modifyIndexAt ix' f <<< SyntaxDefinition) defs) a meta
      | i == List.length defs -> SyntaxBlock $ Block defs (toTerm $ modifyIndexAt ix' f $ SyntaxTerm a) meta
    SyntaxDefinition (TermDefinition termBinding alpha a meta) /\ IndexStep StepTermDefinition i
      | i == 0 -> SyntaxDefinition $ TermDefinition (toTermBinding $ modifyIndexAt ix' f $ SyntaxTermBinding termBinding) alpha a meta
      | i == 1 -> SyntaxDefinition $ TermDefinition termBinding (toType $ modifyIndexAt ix' f $ SyntaxType alpha) a meta
      | i == 2 -> SyntaxDefinition $ TermDefinition termBinding alpha (toTerm $ modifyIndexAt ix' f $ SyntaxTerm a) meta
    SyntaxDefinition (DataDefinition typeBinding constrs meta) /\ IndexStep StepDataDefinition i
      | i == 0 -> SyntaxDefinition (DataDefinition (toTypeBinding $ modifyIndexAt ix' f $ SyntaxTypeBinding typeBinding) constrs meta)
      | inListBounds (i - 1) constrs -> SyntaxDefinition (DataDefinition typeBinding (List.modifyAt' (i - 1) (toConstructor <<< modifyIndexAt ix' f <<< SyntaxConstructor) constrs) meta)
    SyntaxConstructor (Constructor termBinding prms meta) /\ IndexStep StepConstructor i
      | i == 0 -> SyntaxConstructor (Constructor (toTermBinding $ modifyIndexAt ix' f $ SyntaxTermBinding termBinding) prms meta)
      | inListBounds (i - 1) prms -> SyntaxConstructor (Constructor termBinding (List.modifyAt' (i - 1) (toParameter <<< modifyIndexAt ix' f <<< SyntaxParameter) prms) meta)
    SyntaxTerm (LambdaTerm termId block meta) /\ IndexStep StepLambdaTerm i
      | i == 0 -> SyntaxTerm (LambdaTerm (toTermId $ modifyIndexAt ix' f $ SyntaxTermId termId) block meta)
      | i == 1 -> SyntaxTerm (LambdaTerm termId (toBlock $ modifyIndexAt ix' f $ SyntaxBlock block) meta)
    SyntaxTerm (MatchTerm typeId term cases meta) /\ IndexStep StepMatchTerm i
      | i == 0 -> SyntaxTerm (MatchTerm typeId (toTerm $ modifyIndexAt ix' f $ SyntaxTerm term) cases meta)
      | inListBounds (i - 1) cases -> SyntaxTerm (MatchTerm typeId term (List.modifyAt' (i - 1) (toCase <<< modifyIndexAt ix' f <<< SyntaxCase) cases) meta)
    SyntaxTerm (NeutralTerm termId args meta) /\ IndexStep StepNeutralTerm i
      | i == 0 -> SyntaxTerm (NeutralTerm (toTermId $ modifyIndexAt ix' f $ SyntaxTermId termId) args meta)
      | i == 1 -> SyntaxTerm (NeutralTerm termId (toArgs $ modifyIndexAt ix' f $ SyntaxArgs args) meta)
    SyntaxArgs (ConsArgs a args meta) /\ IndexStep StepConsArgs i
      | i == 0 -> SyntaxArgs (ConsArgs (toTerm $ modifyIndexAt ix' f $ SyntaxTerm a) args meta)
      | i == 1 -> SyntaxArgs (ConsArgs a (toArgs $ modifyIndexAt ix' f $ SyntaxArgs args) meta)
    SyntaxCase (Case termIds term meta) /\ IndexStep StepCase i
      | inListBounds i termIds -> SyntaxCase (Case (List.modifyAt' i (toTermId <<< modifyIndexAt ix' f <<< SyntaxTermId) termIds) term meta)
      | i == List.length termIds -> SyntaxCase (Case termIds (toTerm $ modifyIndexAt ix' f $ SyntaxTerm term) meta)
    SyntaxType (ArrowType prm beta meta) /\ IndexStep StepArrowType i
      | i == 0 -> SyntaxType (ArrowType (toParameter $ modifyIndexAt ix' f $ SyntaxParameter prm) beta meta)
      | i == 1 -> SyntaxType (ArrowType prm (toType $ modifyIndexAt ix' f $ SyntaxType beta) meta)
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
          SyntaxModule (Module defs _) -> if List.length defs > 0 then toDownwardIndex $ ixUpward :> IndexStep StepModule 0 else ix
          SyntaxBlock (Block defs _ _) -> toDownwardIndex $ ixUpward :> IndexStep StepBlock 0
          SyntaxDefinition (TermDefinition _ _ _ _) -> toDownwardIndex $ ixUpward :> IndexStep StepTermDefinition 0
          SyntaxDefinition (DataDefinition _ _ _) -> toDownwardIndex $ ixUpward :> IndexStep StepDataDefinition 0
          SyntaxConstructor (Constructor _ _ _) -> toDownwardIndex $ ixUpward :> IndexStep StepConstructor 0
          SyntaxTerm (LambdaTerm _ _ _) -> toDownwardIndex $ ixUpward :> IndexStep StepLambdaTerm 0
          SyntaxTerm (HoleTerm _) -> ix
          SyntaxTerm (MatchTerm _ _ _ _) -> toDownwardIndex $ ixUpward :> IndexStep StepMatchTerm 0
          SyntaxTerm (NeutralTerm _ _ _) -> toDownwardIndex $ ixUpward :> IndexStep StepNeutralTerm 0
          SyntaxArgs NoneArgs -> ix
          SyntaxArgs (ConsArgs _ _ _) -> toDownwardIndex $ ixUpward :> IndexStep StepConsArgs 0
          SyntaxCase (Case _ _ _) -> toDownwardIndex $ ixUpward :> IndexStep StepCase 0
          SyntaxType (ArrowType _ _ _) -> toDownwardIndex $ ixUpward :> IndexStep StepArrowType 0
          SyntaxType (DataType _ _) -> ix 
          SyntaxType (HoleType _ _ _) -> ix 
          SyntaxType (ProxyHoleType _) -> ix 
          SyntaxParameter (Parameter _ _) -> toDownwardIndex $ ixUpward :> IndexStep StepParameter 0
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
                  | inBounds (i - 1) 0 (List.length defs - 1) -> toDownwardIndex $ ix' :> IndexStep StepModule (i - 1)
                  | otherwise -> ix
                SyntaxBlock (Block defs _ _) /\ IndexStep StepBlock i
                  | inBounds (i - 1) 0 (List.length defs) -> toDownwardIndex $ ix' :> IndexStep StepBlock (i - 1)
                  | otherwise -> ix
                SyntaxDefinition (TermDefinition _ _ _ _) /\ IndexStep StepTermDefinition i
                  | inBounds (i - 1) 0 2 -> toDownwardIndex $ ix' :> IndexStep StepTermDefinition (i - 1)
                  | otherwise -> ix
                SyntaxDefinition (DataDefinition _ constrs _) /\ IndexStep StepDataDefinition i
                  | inBounds (i - 1) 0 (List.length constrs) -> toDownwardIndex $ ix' :> IndexStep StepDataDefinition (i - 1)
                  | otherwise -> ix
                SyntaxConstructor (Constructor _ prms _) /\ IndexStep StepConstructor i
                  | inBounds (i - 1) 0 (List.length prms) -> toDownwardIndex $ ix' :> IndexStep StepConstructor (i - 1)
                  | otherwise -> ix
                SyntaxTerm (LambdaTerm _ _ _) /\ IndexStep StepLambdaTerm i
                  | inBounds (i - 1) 0 1 -> toDownwardIndex $ ix' :> IndexStep StepLambdaTerm (i - 1)
                  | otherwise -> ix
                SyntaxTerm (MatchTerm _ _ cases _) /\ IndexStep StepMatchTerm i
                  | inBounds (i - 1) 0 (List.length cases) -> toDownwardIndex $ ix' :> IndexStep StepMatchTerm (i - 1)
                  | otherwise -> ix
                SyntaxTerm (NeutralTerm _ _ _) /\ IndexStep StepNeutralTerm i
                  | inBounds (i - 1) 0 1 -> toDownwardIndex $ ix' :> IndexStep StepNeutralTerm (i - 1)
                  | otherwise -> ix
                SyntaxArgs (ConsArgs _ _ _) /\ IndexStep StepConsArgs i
                  | inBounds (i - 1) 0 1 -> toDownwardIndex $ ix' :> IndexStep StepConsArgs (i - 1)
                  | otherwise -> ix
                SyntaxCase (Case termIds _ _) /\ IndexStep StepCase i
                  | inBounds (i - 1) 0 (List.length termIds) -> toDownwardIndex $ ix' :> IndexStep StepCase (i - 1)
                  | otherwise -> ix
                SyntaxType (ArrowType _ _ _) /\ IndexStep StepArrowType i
                  | inBounds (i - 1) 0 1 -> toDownwardIndex $ ix' :> IndexStep StepArrowType (i - 1)
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
              Debug.trace (syn /\ step) \_ -> case syn /\ step of
                SyntaxModule (Module defs _) /\ IndexStep StepModule i
                  | inBounds (i + 1) 0 (List.length defs - 1) -> toDownwardIndex $ ix' :> IndexStep StepModule (i + 1)
                  | otherwise -> ix
                SyntaxBlock (Block defs _ _) /\ IndexStep StepBlock i
                  | inBounds (i + 1) 0 (List.length defs) -> toDownwardIndex $ ix' :> IndexStep StepBlock (i + 1)
                  | otherwise -> ix
                SyntaxDefinition (TermDefinition _ _ _ _) /\ IndexStep StepTermDefinition i
                  | inBounds (i + 1) 0 2 -> toDownwardIndex $ ix' :> IndexStep StepTermDefinition (i + 1)
                  | otherwise -> ix
                SyntaxDefinition (DataDefinition _ constrs _) /\ IndexStep StepDataDefinition i
                  | inBounds (i + 1) 0 (List.length constrs) -> toDownwardIndex $ ix' :> IndexStep StepDataDefinition (i + 1)
                  | otherwise -> ix 
                SyntaxConstructor (Constructor _ prms _) /\ IndexStep StepConstructor i
                  | inBounds (i + 1) 0 (List.length prms) -> toDownwardIndex $ ix' :> IndexStep StepConstructor (i + 1)
                  | otherwise -> ix
                SyntaxTerm (LambdaTerm _ _ _) /\ IndexStep StepLambdaTerm i
                  | inBounds (i + 1) 0 1  -> toDownwardIndex $ ix' :> IndexStep StepLambdaTerm (i + 1)
                  | otherwise -> ix
                SyntaxTerm (MatchTerm _ _ cases _) /\ IndexStep StepMatchTerm i
                  | inBounds (i + 1) 0 (List.length cases) -> toDownwardIndex $ ix' :> IndexStep StepMatchTerm (i + 1)
                  | otherwise -> ix
                SyntaxTerm (NeutralTerm _ _ _) /\ IndexStep StepNeutralTerm i
                  | inBounds (i + 1) 0 1 -> toDownwardIndex $ ix' :> IndexStep StepNeutralTerm (i + 1)
                  | otherwise -> ix
                SyntaxArgs (ConsArgs _ _ _) /\ IndexStep StepConsArgs i
                  | inBounds (i + 1) 0 1  -> toDownwardIndex $ ix' :> IndexStep StepConsArgs (i + 1)
                  | otherwise -> ix
                SyntaxCase (Case termIds _ _) /\ IndexStep StepCase i
                  | inBounds (i + 1) 0 (List.length termIds) -> toDownwardIndex $ ix' :> IndexStep StepCase (i + 1)
                  | otherwise -> ix
                SyntaxType (ArrowType _ _ _) /\ IndexStep StepArrowType i
                  | inBounds (i + 1) 0 1 -> toDownwardIndex $ ix' :> IndexStep StepArrowType (i + 1)
                  | otherwise -> ix
                SyntaxParameter (Parameter _ _) /\ IndexStep StepParameter i
                  | otherwise -> ix
                _ -> Unsafe.error "moveDownwardIndex.Left: impossible"
