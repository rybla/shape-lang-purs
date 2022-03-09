module Language.Shape.Stlc.Typing where

import Data.Foldable
import Data.Maybe
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Data.List (List(..))
import Data.List as List
import Data.Map.Unsafe (Map)
import Data.Map.Unsafe as Map
import Data.UUID as UUID
import Language.Shape.Stlc.Metadata
import Unsafe as Unsafe

type Context
  = Map TermID Type

addDefinitionToContext :: Definition -> Context -> Context
addDefinitionToContext = case _ of
  TermDefinition (TermBinding id _) alpha a meta -> Map.insert id alpha
  DataDefinition (TypeBinding typeID _) constrs meta -> flip (foldl (flip f)) constrs
    where
    f :: Constructor -> Map TermID Type -> Map TermID Type
    f (Constructor (TermBinding id _) prms _) = Map.insert id (typeOfConstructor prms typeID)

addDefinitionsToContext :: List Definition -> Context -> Context
addDefinitionsToContext = flip $ foldl (flip addDefinitionToContext)

typeOfConstructor :: List Parameter -> TypeID -> Type
typeOfConstructor prms typeID =
  -- TODO: this folds the right way, right?? do a test or two
  foldr
    (\prm beta -> ArrowType prm beta defaultArrowTypeMetadata)
    (DataType typeID defaultDataTypeMetadata)
    prms
