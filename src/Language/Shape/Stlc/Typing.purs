module Language.Shape.Stlc.Typing where

import Data.Foldable
import Data.Maybe
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)

import Data.List (List(..))
import Data.List as List
import Data.Map.Unsafe (Map)
import Data.Map.Unsafe as Map
import Data.UUID as UUID
import Unsafe (error)
import Unsafe as Unsafe

type Context
  = Map TermID Type

replaceHolesWithProxy :: Type -> Type
replaceHolesWithProxy (ArrowType (Parameter a md1) b md2)
  = ArrowType (Parameter (replaceHolesWithProxy a) md1) (replaceHolesWithProxy b) md2
replaceHolesWithProxy (DataType i md) = DataType i md
replaceHolesWithProxy (ProxyHoleType i) = error "This probably shouldn't happen"
replaceHolesWithProxy (HoleType id wea md) = ProxyHoleType id

addDefinitionToContext :: Definition -> Context -> Context
addDefinitionToContext = case _ of
  TermDefinition (TermBinding id _) alpha a meta -> Map.insert id (replaceHolesWithProxy alpha)
  DataDefinition (TypeBinding typeID _) constrs meta -> flip (foldl (flip f)) constrs
    where
    f :: Constructor -> Map TermID Type -> Map TermID Type
    f (Constructor (TermBinding id _) prms _) = Map.insert id (typeOfConstructor prms' typeID)
      where prms' = map (\(Parameter a md) -> (Parameter (replaceHolesWithProxy a) md)) prms

addDefinitionsToContext :: List Definition -> Context -> Context
addDefinitionsToContext = flip $ foldl (flip addDefinitionToContext)

typeOfConstructor :: List Parameter -> TypeID -> Type
typeOfConstructor prms typeID =
  -- TODO: this folds the right way, right?? do a test or two
  foldr
    (\prm beta -> ArrowType prm beta defaultArrowTypeMetadata)
    (DataType typeID defaultDataTypeMetadata)
    prms
