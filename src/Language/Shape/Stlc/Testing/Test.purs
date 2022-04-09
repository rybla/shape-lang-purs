module Language.Shape.Stlc.Testing.Test where

import Data.Tuple.Nested
import Prelude

import Data.List (List(..), (:))
import Data.Tuple (uncurry)
import Effect (Effect)
import Effect.Console (log)
import Language.Shape.Stlc.ChangeAtIndex (Change(..), chAtModule)
import Language.Shape.Stlc.Changes (TypeChange(..))
import Language.Shape.Stlc.Index (DownwardIndex(..), IndexStep(..), StepLabel(..))
import Language.Shape.Stlc.Initial (module_)
import Language.Shape.Stlc.Syntax (Syntax(..), freshHoleId, mkHoleTerm)
import Language.Shape.Stlc.Typing (emptyContext)
import Undefined (undefined)

test :: Effect Unit
test = let p = module_
    --    in let i = (DownwardIndex ((StepModule 0) : StepCons(1) : StepCons(0) : StepDefinitionItem(0) : StepTermDefinition(1) : StepArrowType(1) : StepArrowType(0) : StepParameter(0) : Nil))
       in let steps = (StepModule /\ 0) : (StepCons /\ 1) : (StepCons /\ 0) : (StepDefinitionItem /\ 0) : (StepTermDefinition /\ 1) : (StepArrowType /\ 1) : (StepArrowType /\ 0) : (StepParameter /\ 0) : Nil
       in let i = (DownwardIndex (map (uncurry IndexStep) steps))
       in let term = SyntaxTerm mkHoleTerm
       in let change = ChangeTypeChange (Dig (freshHoleId unit))
       in let result = chAtModule p emptyContext term change i
       in log (show result)