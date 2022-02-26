module Language.Typing where

import Data.Maybe
import Language.Shape.Stlc.Context
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Data.List as List
import Data.Map as Map
import Partial (crashWith)

fromJust' :: forall a. Partial => Maybe a -> String -> a
fromJust' (Just a) _ = a

fromJust' Nothing msg = crashWith msg

typeOfNeutralTerm :: Partial => NeutralTerm -> Context -> Type
typeOfNeutralTerm (ApplicationTerm id _) gamma = fromJust' (Map.lookup id gamma.termIdType) "typeOfNeutralTerm"

typeOfNeutralTerm (MatchTerm _ type_ _) gamma = BaseType type_

typeOfNeutralTerm HoleTerm gamma = BaseType (HoleType (freshHoleId unit) List.Nil)
