module Language.Shape.Stlc.Typing where

import Data.Foldable
import Data.Maybe
import Data.Tuple.Nested
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Record
import Data.List (List(..))
import Data.List as List
import Data.Map.Unsafe (Map, member)
import Data.Map.Unsafe as Map
import Data.Set as Set
import Data.UUID as UUID
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe (error)
import Unsafe as Unsafe

type Context
  = { typeIds :: List TypeId
    , types :: Map TermId Type
    , constructors :: Map TypeId (List TermId)
    }

emptyContext :: Context
emptyContext = { typeIds: Nil, types: Map.empty, constructors: Map.empty }

_types = Proxy :: Proxy "types"

_typeIds = Proxy :: Proxy "typeIds"

_constructors = Proxy :: Proxy "constructors"

insertTyping :: TermId -> Type -> Context -> Context
insertTyping id type_ = modify _types (Map.insert id type_)

lookupTyping :: TermId -> Context -> Type
lookupTyping id context = Map.lookup' id context.types

insertTypeId :: TypeId -> Context -> Context
insertTypeId typeId = modify _typeIds (Cons typeId)

insertConstructorIds :: TypeId -> List TermId -> Context -> Context
insertConstructorIds id constrIds = modify _constructors (Map.insert id constrIds)

lookupConstructorIds :: TypeId -> Context -> List TermId
lookupConstructorIds id context = Map.lookup' id context.constructors

replaceHolesWithProxy :: Type -> Type
replaceHolesWithProxy (ArrowType (Parameter a md1) b md2) = ArrowType (Parameter (replaceHolesWithProxy a) md1) (replaceHolesWithProxy b) md2

replaceHolesWithProxy (DataType i md) = DataType i md

replaceHolesWithProxy (ProxyHoleType i) = error "This probably shouldn't happen"

replaceHolesWithProxy (HoleType id wkn md) = ProxyHoleType id

addDefinitionToContext :: Definition -> Context -> Context
addDefinitionToContext = case _ of
  TermDefinition (TermBinding id _) alpha a meta -> insertTyping id (replaceHolesWithProxy alpha)
  DataDefinition (TypeBinding typeId _) constrItems meta ->
    foldl (<<<) identity
      [ flip (foldl (flip insertConstructorTyping)) (fromItem <$> constrItems)
      , insertConstructorIds typeId constrIds
      , insertTypeId typeId
      ]
    where
    insertConstructorTyping :: Constructor -> Context -> Context
    insertConstructorTyping (Constructor (TermBinding id _) params _) = insertTyping id (typeOfConstructor params' typeId)
      where
      params' = map (\(Parameter a md) -> (Parameter (replaceHolesWithProxy a) md)) (fromItem <$> params)

    constrIds :: List TermId
    constrIds = fromItem >>> (\(Constructor (TermBinding id _) _ _) -> id) <$> constrItems

addDefinitionsToContext :: List Definition -> Context -> Context
addDefinitionsToContext = flip $ foldl (flip addDefinitionToContext)

typeOfConstructor :: List Parameter -> TypeId -> Type
typeOfConstructor params typeId =
  -- TODO: this folds the right way, right?? do a test or two
  foldr
    (\param beta -> ArrowType param beta defaultArrowTypeMetadata)
    (DataType typeId defaultDataTypeMetadata)
    params

flattenArrowType :: Type -> List Parameter /\ Type
flattenArrowType (ArrowType param beta _) = let params /\ delta = flattenArrowType beta in List.Cons param params /\ delta

flattenArrowType type_ = List.Nil /\ type_

unflattenArrowType :: (List Parameter /\ Type) -> Type
unflattenArrowType (Nil /\ beta) = beta

unflattenArrowType ((Cons prm alphas) /\ beta) = mkArrow prm (unflattenArrowType (alphas /\ beta))

recontextType :: Type -> Context -> TypeWeakening -> Type
recontextType alpha gamma wkn = case alpha of
  ArrowType (Parameter alpha meta) beta meta' -> ArrowType (Parameter (recontextType alpha gamma wkn) meta) (recontextType beta gamma wkn) meta'
  DataType typeId meta -> if List.elem typeId gamma.typeIds && not (Set.member typeId wkn) then DataType typeId meta else mkHoleType (freshHoleId unit) Set.empty
  HoleType holeId wkn' meta -> HoleType holeId wkn' meta
  ProxyHoleType holeId -> ProxyHoleType holeId -- dont think about it too hard
