module Language.Shape.Stlc.Index where

import Data.Tuple.Nested
import Language.Shape.Stlc.Syntax
import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List.Unsafe (List)
import Data.List.Unsafe as List
import Data.Maybe (Maybe(..))
import Debug as Debug
import Undefined (undefined)
import Unsafe as Unsafe

newtype UpwardIndex
  = UpwardIndex (List IndexStep)

newtype DownwardIndex
  = DownwardIndex (List IndexStep)

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

data IndexStepLabel
  = StepLabelModule
  | StepLabelBlock
  | StepLabelTermDefinition
  | StepLabelDataDefinition
  | StepLabelConstructor
  | StepLabelArrowType
  | StepLabelDataType
  | StepLabelHoleType
  | StepLabelLambdaTerm
  | StepLabelNeutralTerm
  | StepLabelMatchTerm
  | StepLabelHoleTerm
  | StepLabelCase
  | StepLabelConsArgs
  | StepLabelParameter

derive instance Generic IndexStepLabel _ 
instance Eq IndexStepLabel where eq l1 l2 = genericEq l1 l2

data IndexStep
  = IndexStep IndexStepLabel Int

derive instance Generic IndexStep _ 
instance Eq IndexStep where eq ix1 ix2 = genericEq ix1 ix2

inBounds :: forall a. Int -> List a -> Boolean
inBounds i ls = 0 <= i && i < List.length ls

lookupSyntaxAt :: DownwardIndex -> Syntax -> Syntax
lookupSyntaxAt ix syn = case unconsDownwardIndex ix of
  Nothing -> syn
  Just { ix', step } -> case syn /\ step of
    SyntaxModule (Module defs meta) /\ IndexStep StepLabelModule i
      | inBounds i defs -> lookupSyntaxAt ix $ SyntaxDefinition (List.index' defs i)
    SyntaxBlock (Block defs a meta) /\ IndexStep StepLabelBlock i
      | inBounds i defs -> lookupSyntaxAt ix $ SyntaxDefinition (List.index' defs i)
      | i == List.length defs -> lookupSyntaxAt ix $ SyntaxTerm a
    SyntaxDefinition (TermDefinition termBinding alpha a meta) /\ IndexStep StepLabelTermDefinition i
      | i == 0 -> lookupSyntaxAt ix $ SyntaxTermBinding termBinding
      | i == 1 -> lookupSyntaxAt ix $ SyntaxType alpha
      | i == 2 -> lookupSyntaxAt ix $ SyntaxTerm a
    SyntaxDefinition (DataDefinition typeBinding constrs meta) /\ IndexStep StepLabelDataDefinition i
      | i == 0 -> lookupSyntaxAt ix $ SyntaxTypeBinding typeBinding
      | inBounds (i - 1) constrs -> lookupSyntaxAt ix $ SyntaxConstructor $ List.index' constrs (i - 1)
    SyntaxConstructor (Constructor termBinding prms meta) /\ IndexStep StepLabelConstructor i
      | i == 0 -> lookupSyntaxAt ix $ SyntaxTermBinding termBinding
      | inBounds (i - 1) prms -> lookupSyntaxAt ix $ SyntaxParameter $ List.index' prms i
    SyntaxTerm (LambdaTerm termId block meta) /\ IndexStep StepLabelLambdaTerm i
      | i == 0 -> lookupSyntaxAt ix $ SyntaxTermId termId
      | i == 1 -> lookupSyntaxAt ix $ SyntaxBlock block
    SyntaxTerm (MatchTerm typeId term cases meta) /\ IndexStep StepLabelMatchTerm i
      | i == 0 -> lookupSyntaxAt ix $ SyntaxTerm term
      | inBounds (i - 1) cases -> lookupSyntaxAt ix $ SyntaxCase $ List.index' cases (i - 1)
    SyntaxTerm (NeutralTerm termId args meta) /\ IndexStep StepLabelNeutralTerm i
      | i == 0 -> lookupSyntaxAt ix $ SyntaxTermId termId
      | i == 1 -> lookupSyntaxAt ix $ SyntaxArgs args
    SyntaxArgs (ConsArgs a args meta) /\ IndexStep StepLabelConsArgs i
      | i == 0 -> lookupSyntaxAt ix $ SyntaxTerm a
      | i == 1 -> lookupSyntaxAt ix $ SyntaxArgs args
    SyntaxCase (Case termIds term meta) /\ IndexStep StepLabelCase i
      | inBounds i termIds -> lookupSyntaxAt ix $ SyntaxTermId (List.index' termIds i)
      | i == List.length termIds -> lookupSyntaxAt ix $ SyntaxTerm term
    SyntaxType (ArrowType prm beta meta) /\ IndexStep StepLabelArrowType i
      | i == 0 -> lookupSyntaxAt ix $ SyntaxParameter prm
      | i == 1 -> lookupSyntaxAt ix $ SyntaxType beta
    SyntaxParameter (Parameter alpha meta) /\ IndexStep StepLabelParameter i
      | i == 0 -> lookupSyntaxAt ix $ SyntaxType alpha
    _ -> Debug.trace (ix /\ syn) \_ -> Unsafe.error "lookupSyntaxAt: impossible"

modifyIndexAt :: DownwardIndex -> (Syntax -> Syntax) -> Syntax -> Syntax
modifyIndexAt ix f syn = case unconsDownwardIndex ix of
  Nothing -> f syn
  Just { step, ix' } -> case syn /\ step of
    SyntaxModule (Module defs meta) /\ IndexStep StepLabelModule i
      | inBounds i defs -> SyntaxModule $ Module (List.modifyAt' i (toDefinition <<< modifyIndexAt ix f <<< SyntaxDefinition) defs) meta
    SyntaxBlock (Block defs a meta) /\ IndexStep StepLabelBlock i
      | inBounds i defs -> SyntaxBlock $ Block (List.modifyAt' i (toDefinition <<< modifyIndexAt ix f <<< SyntaxDefinition) defs) a meta
      | i == List.length defs -> SyntaxBlock $ Block defs (toTerm $ modifyIndexAt ix f $ SyntaxTerm a) meta
    SyntaxDefinition (TermDefinition termBinding alpha a meta) /\ IndexStep StepLabelTermDefinition i
      | i == 0 -> SyntaxDefinition $ TermDefinition (toTermBinding $ modifyIndexAt ix f $ SyntaxTermBinding termBinding) alpha a meta
      | i == 1 -> SyntaxDefinition $ TermDefinition termBinding (toType $ modifyIndexAt ix f $ SyntaxType alpha) a meta
      | i == 2 -> SyntaxDefinition $ TermDefinition termBinding alpha (toTerm $ modifyIndexAt ix f $ SyntaxTerm a) meta
    SyntaxDefinition (DataDefinition typeBinding constrs meta) /\ IndexStep StepLabelDataDefinition i
      | i == 0 -> SyntaxDefinition (DataDefinition (toTypeBinding $ modifyIndexAt ix f $ SyntaxTypeBinding typeBinding) constrs meta)
      | inBounds (i - 1) constrs -> SyntaxDefinition (DataDefinition typeBinding (List.modifyAt' (i - 1) (toConstructor <<< modifyIndexAt ix f <<< SyntaxConstructor) constrs) meta)
    SyntaxConstructor (Constructor termBinding prms meta) /\ IndexStep StepLabelConstructor i
      | i == 0 -> SyntaxConstructor (Constructor (toTermBinding $ modifyIndexAt ix f $ SyntaxTermBinding termBinding) prms meta)
      | inBounds (i - 1) prms -> SyntaxConstructor (Constructor termBinding (List.modifyAt' (i - 1) (toParameter <<< modifyIndexAt ix f <<< SyntaxParameter) prms) meta)
    SyntaxTerm (LambdaTerm termId block meta) /\ IndexStep StepLabelLambdaTerm i
      | i == 0 -> SyntaxTerm (LambdaTerm (toTermId $ modifyIndexAt ix f $ SyntaxTermId termId) block meta)
      | i == 1 -> SyntaxTerm (LambdaTerm termId (toBlock $ modifyIndexAt ix f $ SyntaxBlock block) meta)
    SyntaxTerm (MatchTerm typeId term cases meta) /\ IndexStep StepLabelMatchTerm i
      | i == 0 -> SyntaxTerm (MatchTerm typeId (toTerm $ modifyIndexAt ix f $ SyntaxTerm term) cases meta)
      | inBounds (i - 1) cases -> SyntaxTerm (MatchTerm typeId term (List.modifyAt' (i - 1) (toCase <<< modifyIndexAt ix f <<< SyntaxCase) cases) meta)
    SyntaxTerm (NeutralTerm termId args meta) /\ IndexStep StepLabelNeutralTerm i
      | i == 0 -> SyntaxTerm (NeutralTerm (toTermId $ modifyIndexAt ix f $ SyntaxTermId termId) args meta)
      | i == 1 -> SyntaxTerm (NeutralTerm termId (toArgs $ modifyIndexAt ix f $ SyntaxArgs args) meta)
    SyntaxArgs (ConsArgs a args meta) /\ IndexStep StepLabelConsArgs i
      | i == 0 -> SyntaxArgs (ConsArgs (toTerm $ modifyIndexAt ix f $ SyntaxTerm a) args meta)
      | i == 1 -> SyntaxArgs (ConsArgs a (toArgs $ modifyIndexAt ix f $ SyntaxArgs args) meta)
    SyntaxCase (Case termIds term meta) /\ IndexStep StepLabelCase i
      | inBounds i termIds -> SyntaxCase (Case (List.modifyAt' i (toTermId <<< modifyIndexAt ix f <<< SyntaxTermId) termIds) term meta)
      | i == List.length termIds -> SyntaxCase (Case termIds (toTerm $ modifyIndexAt ix f $ SyntaxTerm term) meta)
    SyntaxType (ArrowType prm beta meta) /\ IndexStep StepLabelArrowType i
      | i == 0 -> SyntaxType (ArrowType (toParameter $ modifyIndexAt ix f $ SyntaxParameter prm) beta meta)
      | i == 1 -> SyntaxType (ArrowType prm (toType $ modifyIndexAt ix f $ SyntaxType beta) meta)
    SyntaxParameter (Parameter alpha meta) /\ IndexStep StepLabelParameter i
      | i == 0 -> SyntaxParameter (Parameter (toType $ modifyIndexAt ix f $ SyntaxType alpha) meta)
    _ -> Debug.trace (ix /\ syn) \_ -> Unsafe.error "modifyIndexAt: impossible"

data Direction
  = Up
  | Down
  | Left
  | Right

moveDownwardIndex :: Direction -> Module -> DownwardIndex -> DownwardIndex
moveDownwardIndex dir mod ix = case dir of
  Up -> case unconsDownwardIndex ix of
    Nothing -> ix
    Just { ix' } -> ix'
  Down ->
    let
      ixUpward = toUpwardIndex ix
    in
      toDownwardIndex case lookupSyntaxAt ix (SyntaxModule mod) of
        SyntaxModule (Module defs _) -> if List.length defs > 0 then ixUpward :> IndexStep StepLabelModule 0 else ixUpward
        SyntaxBlock (Block defs _ _) -> if List.length defs > 0 then ixUpward :> IndexStep StepLabelBlock 0 else ixUpward
        SyntaxDefinition (TermDefinition _ _ _ _) -> ixUpward :> IndexStep StepLabelTermDefinition 0
        SyntaxDefinition (DataDefinition _ _ _) -> ixUpward :> IndexStep StepLabelDataDefinition 0
        SyntaxConstructor (Constructor _ _ _) -> ixUpward :> IndexStep StepLabelConstructor 0
        SyntaxTerm (LambdaTerm _ _ _) -> ixUpward :> IndexStep StepLabelLambdaTerm 0
        SyntaxTerm (HoleTerm _) -> ixUpward
        SyntaxTerm (MatchTerm _ _ _ _) -> ixUpward :> IndexStep StepLabelMatchTerm 0
        SyntaxTerm (NeutralTerm _ _ _) -> ixUpward :> IndexStep StepLabelNeutralTerm 0
        SyntaxArgs NoneArgs -> ixUpward
        SyntaxArgs (ConsArgs _ _ _) -> ixUpward :> IndexStep StepLabelConsArgs 0
        SyntaxCase (Case _ _ _) -> ixUpward :> IndexStep StepLabelCase 0
        SyntaxType (ArrowType _ _ _) -> ixUpward :> IndexStep StepLabelArrowType 0
        SyntaxType (DataType _ _) -> ixUpward
        SyntaxType (HoleType _ _ _) -> ixUpward
        SyntaxType (ProxyHoleType _) -> ixUpward
        SyntaxParameter (Parameter _ _) -> ixUpward :> IndexStep StepLabelParameter 0
        SyntaxTermBinding _ -> ixUpward
        SyntaxTypeBinding _ -> ixUpward
        SyntaxTermId _ -> ixUpward
  Left ->
    let
      ixUpward = toUpwardIndex ix
    in
      toDownwardIndex case unconsUpwardIndex (toUpwardIndex ix) of
        Nothing -> ixUpward
        Just { ix', step } ->
          let
            syn = lookupSyntaxAt ix (SyntaxModule mod)
          in
            case syn /\ step of
              SyntaxModule (Module defs _) /\ IndexStep StepLabelModule i
                | 0 <= i + 1 && i + 1 < List.length defs -> ix' :> IndexStep StepLabelModule (i + 1)
                | otherwise -> ixUpward
              SyntaxBlock (Block defs _ _) /\ IndexStep StepLabelBlock i
                | 0 <= i && i < List.length defs -> ix' :> IndexStep StepLabelBlock (i + 1)
                | i + 1 == List.length defs -> ix' :> IndexStep StepLabelBlock (i + 1)
                | otherwise -> ixUpward
              SyntaxDefinition (TermDefinition _ _ _ _) /\ IndexStep StepLabelTermDefinition i
                | 0 <= i + 1 && i <= 2 -> ix' :> IndexStep StepLabelTermDefinition (i + 1)
                | otherwise -> ixUpward
              SyntaxDefinition (DataDefinition _ constrs _) /\ IndexStep StepLabelDataDefinition i
                | (i + 1) - 1 <= List.length constrs -> ix' :> IndexStep StepLabelDataDefinition (i + 1)
                | otherwise -> ixUpward
              SyntaxConstructor (Constructor _ prms _) /\ IndexStep StepLabelConstructor i
                | (i + 1) - 1 <= List.length prms -> ix' :> IndexStep StepLabelConstructor (i + 1)
                | otherwise -> ixUpward
              SyntaxTerm (LambdaTerm _ _ _) /\ IndexStep StepLabelLambdaTerm i
                | i + 1 <= 1 -> ix' :> IndexStep StepLabelLambdaTerm (i + 1)
                | otherwise -> ixUpward
              SyntaxTerm (MatchTerm _ _ cases _) /\ IndexStep StepLabelMatchTerm i
                | (i + 1) - 1 <= List.length cases -> ix' :> IndexStep StepLabelMatchTerm (i + 1)
                | otherwise -> ixUpward
              SyntaxTerm (NeutralTerm _ _ _) /\ IndexStep StepLabelNeutralTerm i
                | i + 1 <= 1 -> ix' :> IndexStep StepLabelNeutralTerm (i + 1)
                | otherwise -> ixUpward
              SyntaxArgs (ConsArgs _ _ _) /\ IndexStep StepLabelConsArgs i
                | i + 1 <= 1 -> ix' :> IndexStep StepLabelConsArgs (i + 1)
                | otherwise -> ixUpward
              SyntaxCase (Case termIds _ _) /\ IndexStep StepLabelCase i
                | 0 <= i + 1 && i + 1 < List.length termIds -> ix' :> IndexStep StepLabelCase (i + 1)
                | i + 1 == List.length termIds -> ix' :> IndexStep StepLabelCase (i + 1)
                | otherwise -> ixUpward
              SyntaxType (ArrowType _ _ _) /\ IndexStep StepLabelArrowType i
                | i + 1 <= 1 -> ix' :> IndexStep StepLabelArrowType (i + 1)
                | otherwise -> ixUpward
              SyntaxParameter (Parameter _ _) /\ IndexStep StepLabelParameter i
                | otherwise -> ixUpward
              _ -> Debug.trace (ix /\ syn) \_ -> Unsafe.error "moveDownwardIndex.Left: impossible"
  Right ->
    let
      ixUpward = toUpwardIndex ix
    in
      toDownwardIndex case unconsUpwardIndex (toUpwardIndex ix) of
        Nothing -> ixUpward
        Just { ix', step } ->
          let
            syn = lookupSyntaxAt ix (SyntaxModule mod)
          in
            case syn /\ step of
              SyntaxModule (Module defs _) /\ IndexStep StepLabelModule i
                | 0 <= i - 1 && i - 1 < List.length defs -> ix' :> IndexStep StepLabelModule (i - 1)
                | otherwise -> ixUpward
              SyntaxBlock (Block defs _ _) /\ IndexStep StepLabelBlock i
                | 0 <= i && i < List.length defs -> ix' :> IndexStep StepLabelBlock (i - 1)
                | i - 1 == List.length defs -> ix' :> IndexStep StepLabelBlock (i - 1)
                | otherwise -> ixUpward
              SyntaxDefinition (TermDefinition _ _ _ _) /\ IndexStep StepLabelTermDefinition i
                | 0 <= i - 1 && i <= 2 -> ix' :> IndexStep StepLabelTermDefinition (i - 1)
                | otherwise -> ixUpward
              SyntaxDefinition (DataDefinition _ constrs _) /\ IndexStep StepLabelDataDefinition i
                | 0 <= (i - 1) - 1 && (i - 1) - 1 <= List.length constrs -> ix' :> IndexStep StepLabelDataDefinition (i - 1)
                | otherwise -> ixUpward
              SyntaxConstructor (Constructor _ prms _) /\ IndexStep StepLabelConstructor i
                | 0 <= (i - 1) - 1 && (i - 1) - 1 <= List.length prms -> ix' :> IndexStep StepLabelConstructor (i - 1)
                | otherwise -> ixUpward
              SyntaxTerm (LambdaTerm _ _ _) /\ IndexStep StepLabelLambdaTerm i
                | 0 <= i - 1 && i - 1 <= 1 -> ix' :> IndexStep StepLabelLambdaTerm (i - 1)
                | otherwise -> ixUpward
              SyntaxTerm (MatchTerm _ _ cases _) /\ IndexStep StepLabelMatchTerm i
                | 0 <= (i - 1) - 1 && (i - 1) - 1 <= List.length cases -> ix' :> IndexStep StepLabelMatchTerm (i - 1)
                | otherwise -> ixUpward
              SyntaxTerm (NeutralTerm _ _ _) /\ IndexStep StepLabelNeutralTerm i
                | 0 <= i - 1 && i - 1 <= 1 -> ix' :> IndexStep StepLabelNeutralTerm (i - 1)
                | otherwise -> ixUpward
              SyntaxArgs (ConsArgs _ _ _) /\ IndexStep StepLabelConsArgs i
                | 0 <= i - 1 && i - 1 <= 1 -> ix' :> IndexStep StepLabelConsArgs (i - 1)
                | otherwise -> ixUpward
              SyntaxCase (Case termIds _ _) /\ IndexStep StepLabelCase i
                | 0 <= i - 1 && i - 1 < List.length termIds -> ix' :> IndexStep StepLabelCase (i - 1)
                | otherwise -> ixUpward
              SyntaxType (ArrowType _ _ _) /\ IndexStep StepLabelArrowType i
                | 0 <= i - 1 && i - 1 <= 1 -> ix' :> IndexStep StepLabelArrowType (i - 1)
                | otherwise -> ixUpward
              SyntaxParameter (Parameter _ _) /\ IndexStep StepLabelParameter i
                | otherwise -> ixUpward
              _ -> Debug.trace (ix /\ syn) \_ -> Unsafe.error "moveDownwardIndex.Right: impossible"
