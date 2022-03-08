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
  = Map TermID Type

addDefinitionToContext :: Definition -> Context -> Context
addDefinitionToContext = undefined

addDefinitionsToContext :: List Definition -> Context -> Context
addDefinitionsToContext = undefined

-- data Term
--   = LambdaTerm TermBinding Type Block LambdaTermMetadata
--   | HoleTerm Type HoleTermMetadata
--   | MatchTerm TypeID Term (List Term) MatchTermMetadata
--   | NeutralTerm NeutralTerm NeutralTermMetadata
-- data NeutralTerm
--   = VariableTerm TermID VariableTermMetadata
--   | ApplicationTerm NeutralTerm Term ApplicationTermMetadata
inferTerm :: Context -> Term -> Type
inferTerm ctx (LambdaTerm bi a b md) = undefined

inferTerm ctx (HoleTerm t md) = undefined

inferTerm ctx (MatchTerm i t cases md) = undefined

inferTerm ctx t = undefined

inferBlock :: Context -> Block -> Type
inferBlock ctx b = undefined

inferNeutral :: Context -> NeutralTerm -> Type
inferNeutral ctx t = undefined

-- typeOfNeutralTerm :: Context -> NeutralTerm -> BaseType
-- typeOfNeutralTerm = undefined
-- -- typeOfNeutralTerm gamma (ApplicationTerm (TermReference id _) Nil _) = case Unsafe.fromJust (Map.lookup id gamma) of
-- --   ArrowType prms beta -> beta
-- --   _ -> Unsafe.error "impossible"
-- -- typeOfNeutralTerm gamma (NeutralTerm a) = case Unsafe.fromJust (Map.lookup undefined gamma) of
-- --   BaseType alpha -> alpha
-- --   _ -> Unsafe.error "impossible"
-- addParametersToContext :: Context -> List Parameter -> Context
-- addParametersToContext = undefined
-- addDefinitionsToContext :: Context -> List Definition -> Context
-- addDefinitionsToContext gamma defs =
--   List.foldl
--     ( \gamma' -> case _ of
--         TermDefinition (TermUniqueBinding id _) alpha _ _ -> Map.insert id alpha gamma'
--         DataDefinition (TypeUniqueBinding idType _) constrs _ -> List.foldl (\gamma'' (Constructor (TermUniqueBinding idConstr _) prms _) -> Map.insert idConstr (makeArrowType prms (makeDataType idType)) gamma'') gamma' constrs
--     )
--     Map.empty
--     defs
