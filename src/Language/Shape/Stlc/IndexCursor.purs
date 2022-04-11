module Language.Shape.Stlc.IndexCursor where

import Language.Shape.Stlc.IndexSyntax
import Data.List.Unsafe
import Data.Tuple.Nested
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Syntax
import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Unfoldable (replicate)
import Debug as Debug
import Partial.Unsafe (unsafeCrashWith)
import Undefined (undefined)
import Unsafe as Unsafe

data Direction
  = Up
  | Down
  | Left
  | Right

derive instance genericDirection :: Generic Direction _

instance showDirection :: Show Direction where
  show x = genericShow x

moveDownwardIndex :: Direction -> Syntax -> DownwardIndex -> DownwardIndex
moveDownwardIndex dir syn ix =
  Debug.trace ("moveDownwardIndex:" <> "\ndir: " <> show dir <> "\nsyn: " <> show syn <> "\nix: ") \_ ->
    moveDownwardIndex' dir syn ix

moveDownwardIndex' :: Direction -> Syntax -> DownwardIndex -> DownwardIndex
moveDownwardIndex' Up syn ix = case unsnocDownwardIndex ix of
  Just { ix' } -> ix'
  Nothing -> ix

moveDownwardIndex' Down syn ix = case lookupSyntaxAt ix syn of
  SyntaxModule (Module defItems meta) -> if not (null defItems) then snocDownwardIndex ix (IndexStep StepModule 0) else ix
  SyntaxBlock (Block defItems a meta) -> snocDownwardIndex ix $ IndexStep StepBlock if not (null defItems) then 0 else 1
  SyntaxDefinition (TermDefinition x alpha a meta) -> snocDownwardIndex ix $ IndexStep StepTermDefinition 0
  SyntaxDefinition (DataDefinition t constrItems meta) -> snocDownwardIndex ix $ IndexStep StepDataDefinition 0
  SyntaxConstructor (Constructor x paramItems meta) -> snocDownwardIndex ix $ IndexStep StepConstructor 0
  SyntaxType (ArrowType param beta meta) -> snocDownwardIndex ix $ IndexStep StepArrowType 0
  SyntaxTerm (LambdaTerm x block meta) -> snocDownwardIndex ix $ IndexStep StepLambdaTerm 0
  SyntaxTerm (NeutralTerm x argItems meta) -> snocDownwardIndex ix $ IndexStep StepNeutralTerm 0
  SyntaxTerm (MatchTerm typeId a caseItems meta) -> snocDownwardIndex ix $ IndexStep StepMatchTerm 0
  SyntaxCase (Case xItems block meta) -> snocDownwardIndex ix $ IndexStep StepCase 0
  SyntaxParameter (Parameter alpha meta) -> snocDownwardIndex ix $ IndexStep StepParameter 0
  -- items
  SyntaxDefinitionItem (def /\ meta) -> snocDownwardIndex ix $ IndexStep StepDefinitionItem 0
  SyntaxConstructorItem (constr /\ meta) -> snocDownwardIndex ix $ IndexStep StepConstructorItem 0
  SyntaxCaseItem (case_ /\ meta) -> snocDownwardIndex ix $ IndexStep StepCaseItem 0
  SyntaxParameterItem (param /\ meta) -> snocDownwardIndex ix $ IndexStep StepParameterItem 0
  SyntaxArgItem (arg /\ meta) -> snocDownwardIndex ix $ IndexStep StepArgItem 0
  SyntaxTermIdItem (termId /\ meta) -> snocDownwardIndex ix $ IndexStep StepTermIdItem 0
  -- list
  SyntaxList (Cons h t) -> snocDownwardIndex ix $ IndexStep StepCons 0
  -- otherwise can't go down
  _ -> ix

moveDownwardIndex' Left syn ix = case unsnocDownwardIndex ix of
  Just { ix', step: IndexStep stepLabel i } -> snocDownwardIndex ix' (IndexStep stepLabel (max 0 (i - 1)))
  Nothing -> ix

moveDownwardIndex' Right syn ix = case unsnocDownwardIndex ix of
  Just { ix', step: IndexStep stepLabel i } -> snocDownwardIndex ix' (IndexStep stepLabel (min (childrenCount stepLabel - 1) (i + 1)))
  Nothing -> ix
