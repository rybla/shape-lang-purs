module Language.Shape.Stlc.Typing where

import Data.Foldable
import Data.Maybe
import Data.Tuple.Nested
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Data.List (List(..))
import Data.List as List
import Data.Map.Unsafe (Map)
import Data.Map.Unsafe as Map
import Data.UUID as UUID
import Undefined (undefined)
import Unsafe (error)
import Unsafe as Unsafe

type Context
  = Map TermId Type

replaceHolesWithProxy :: Type -> Type
replaceHolesWithProxy (ArrowType (Parameter a md1) b md2) = ArrowType (Parameter (replaceHolesWithProxy a) md1) (replaceHolesWithProxy b) md2

replaceHolesWithProxy (DataType i md) = DataType i md

replaceHolesWithProxy (ProxyHoleType i) = error "This probably shouldn't happen"

replaceHolesWithProxy (HoleType id wkn md) = ProxyHoleType id

addDefinitionToContext :: Definition -> Context -> Context
addDefinitionToContext = case _ of
  TermDefinition (TermBinding id _) alpha a meta -> Map.insert id (replaceHolesWithProxy alpha)
  DataDefinition (TypeBinding typeId _) constrs meta -> flip (foldl (flip f)) constrs
    where
    f :: Constructor -> Map TermId Type -> Map TermId Type
    f (Constructor (TermBinding id _) prms _) = Map.insert id (typeOfConstructor prms' typeId)
      where
      prms' = map (\(Parameter a md) -> (Parameter (replaceHolesWithProxy a) md)) prms

addDefinitionsToContext :: List Definition -> Context -> Context
addDefinitionsToContext = flip $ foldl (flip addDefinitionToContext)

typeOfConstructor :: List Parameter -> TypeId -> Type
typeOfConstructor prms typeId =
  -- TODO: this folds the right way, right?? do a test or two
  foldr
    (\prm beta -> ArrowType prm beta defaultArrowTypeMetadata)
    (DataType typeId defaultDataTypeMetadata)
    prms

flattenArrowType :: Type -> List Parameter /\ Type
flattenArrowType (ArrowType prm beta _) = let prms /\ delta = flattenArrowType beta in List.Cons prm prms /\ delta

flattenArrowType type_ = List.Nil /\ type_
