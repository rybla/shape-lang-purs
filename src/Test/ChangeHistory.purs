module Test.ChangeHistory where

import Data.Tuple.Nested
import Language.Shape.Stlc.ChangeAtIndex
import Language.Shape.Stlc.Changes
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Syntax
import Prelude
import Data.List (List(..), (:), foldM)
import Data.Map (Map)
import Data.Map as Map
import Data.Set (fromFoldable)
import Data.Tuple (Tuple(..))
import Data.UUID as UUID
import Debug as Debug
import Effect (Effect)
import Effect.Console (log)
import Language.Shape.Stlc.Initial as Initial
import Data.Maybe (Maybe(..))
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.RenderingTypes (ChangeHistory)
import Language.Shape.Stlc.Typing (emptyContext)
import Unsafe (fromJust)

runChangeHistory :: (Module /\ ChangeHistory) -> Effect Unit
runChangeHistory (m /\ ch) = void $ foldM f m ch
  where
  f :: Module -> (Syntax /\ Change /\ DownwardIndex) -> Effect Module
  f m (syn /\ c /\ ix) = do
    Debug.traceM "TODO: do some logging in runChangeHistory"
    let
      m' /\ _ /\ holeSub = fromJust $ chAtModule m emptyContext syn c ix
    -- TODO: use holeSub
    Debug.traceM (show m')
    pure m'

changeHistories :: Map String (Module /\ ChangeHistory)
changeHistories =
  Map.fromFoldable
    [
    -- test a enArrow which should work
    "test1" /\ Initial.test1 /\ ((Tuple (SyntaxType (ArrowType (Parameter (HoleType (HoleId (fromJust (UUID.parseUUID "fc0fdd77-899d-4373-a86b-5d4c34f6cf05"))) (fromFoldable []) {}) { name: (TermName "_") }) (DataType (TypeId (fromJust (UUID.parseUUID "f9d58f7c-631a-4dd5-a0bf-5ba7cecf1a16"))) {}) { indented: false })) (Tuple (ChangeTypeChange (InsertArg (HoleType (HoleId (fromJust (UUID.parseUUID "fc0fdd77-899d-4373-a86b-5d4c34f6cf05"))) (fromFoldable []) {}))) (DownwardIndex ((IndexStep StepModule 0) : (IndexStep StepCons 1) : (IndexStep StepCons 0) : (IndexStep StepDefinitionItem 0) : (IndexStep StepTermDefinition 1) : (IndexStep StepArrowType 1) : (IndexStep StepArrowType 0) : (IndexStep StepParameter 0) : Nil)))) : Nil)
    -- test a dig which is broken
    , "testdigfirstnat" /\ Initial.test1 /\ ((Tuple (SyntaxType (HoleType (HoleId (fromJust (UUID.parseUUID "d2f96907-ca87-4172-9619-f82da7d1ab53"))) (fromFoldable []) {})) (Tuple (ChangeTypeChange (Dig (HoleId (fromJust (UUID.parseUUID "d2f96907-ca87-4172-9619-f82da7d1ab53"))))) (DownwardIndex ((IndexStep StepModule 0) : (IndexStep StepCons 1) : (IndexStep StepCons 0) : (IndexStep StepDefinitionItem 0) : (IndexStep StepTermDefinition 1) : (IndexStep StepArrowType 0) : (IndexStep StepParameter 0) : Nil)))) : Nil)
    , "test2" /\ Initial.test1 /\ ((Tuple (SyntaxType (HoleType (HoleId (fromJust (UUID.parseUUID "68fdf3da-b361-48b2-9bb2-36b659c31b36"))) (fromFoldable []) {})) (Tuple (ChangeTypeChange (Dig (HoleId (fromJust (UUID.parseUUID "68fdf3da-b361-48b2-9bb2-36b659c31b36"))))) (DownwardIndex ((IndexStep StepModule 0) : (IndexStep StepCons 1) : (IndexStep StepCons 0) : (IndexStep StepDefinitionItem 0) : (IndexStep StepTermDefinition 1) : (IndexStep StepArrowType 0) : (IndexStep StepParameter 0) : Nil)))) : Nil)
    , "removeArg1" /\ Initial.test1 /\ ((Tuple (SyntaxType (DataType (TypeId (fromJust (UUID.parseUUID "83627817-184a-40cb-bd5d-595097fa8558"))) {})) (Tuple (ChangeTypeChange RemoveArg) (DownwardIndex ((IndexStep StepModule 0) : (IndexStep StepCons 1) : (IndexStep StepCons 0) : (IndexStep StepDefinitionItem 0) : (IndexStep StepTermDefinition 1) : (IndexStep StepArrowType 1) : Nil)))) : Nil)
    , "digLambda1" /\ Initial.test1 /\ ((Tuple (SyntaxTerm (HoleTerm {})) (Tuple (ChangeTypeChange (Dig (HoleId (fromJust (UUID.parseUUID "9b3d9c9d-0a07-4c2e-93a9-acbb07e0304e"))))) (DownwardIndex ((IndexStep StepModule 0) : (IndexStep StepCons 1) : (IndexStep StepCons 0) : (IndexStep StepDefinitionItem 0) : (IndexStep StepTermDefinition 2) : (IndexStep StepLambdaTerm 1) : (IndexStep StepBlock 1) : Nil)))) : Nil)
    , "digLambda2" /\ Initial.test1 /\ ((Tuple (SyntaxTerm (HoleTerm {})) (Tuple (ChangeTypeChange (Dig (HoleId (fromJust (UUID.parseUUID "fe7e1ea5-192b-4ef2-bab4-6b402fa0a03e"))))) (DownwardIndex ((IndexStep StepModule 0) : (IndexStep StepCons 1) : (IndexStep StepCons 0) : (IndexStep StepDefinitionItem 0) : (IndexStep StepTermDefinition 2) : (IndexStep StepLambdaTerm 1) : (IndexStep StepBlock 1) : Nil)))) : Nil)
    ]
