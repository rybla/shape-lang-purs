module Language.Shape.Stlc.Typing where

import Data.Maybe
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.UUID as UUID
import Undefined (undefined)
import Unsafe as Unsafe

-- Context
type Context
  = Map TermId Type

typeOfNeutralTerm :: Context -> NeutralTerm -> BaseType
typeOfNeutralTerm = undefined
-- typeOfNeutralTerm gamma (ApplicationTerm (TermReference id _) Nil _) = case Unsafe.fromJust (Map.lookup id gamma) of
--   ArrowType prms beta -> beta
--   _ -> Unsafe.error "impossible"

-- typeOfNeutralTerm gamma (NeutralTerm a) = case Unsafe.fromJust (Map.lookup undefined gamma) of
--   BaseType alpha -> alpha
--   _ -> Unsafe.error "impossible"

addParametersToContext :: Context -> List Parameter -> Context
addParametersToContext = undefined

addDefinitionsToContext :: Context -> List Definition -> Context
addDefinitionsToContext gamma defs =
  List.foldl
    ( \gamma' -> case _ of
        TermDefinition (TermUniqueBinding id _) alpha _ _ -> Map.insert id alpha gamma'
        DataDefinition (TypeUniqueBinding idType _) constrs _ -> List.foldl (\gamma'' (Constructor (TermUniqueBinding idConstr _) prms _) -> Map.insert idConstr (makeArrowType prms (makeDataType idType)) gamma'') gamma' constrs
    )
    Map.empty
    defs
