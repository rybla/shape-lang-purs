module Language.Shape.Stlc.Context where

import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Unsafe as Unsafe

-- Context
type Context
  = { typeScope :: TypeScope
    , termScope :: TermScope
    , indentation :: Int
    }

type TypeScope
  = Scope TypeId TypeName (List TermId) -- typeId => constructorIds

type TermScope
  = Scope TermId TermName Type -- termId => type

emptyContext :: Context
emptyContext =
  { typeScope: emptyScope
  , termScope: emptyScope
  , indentation: 0
  }

-- Scope
type Scope id name value
  = { names :: Map id name
    , clashes :: Map name (List id) -- name => ids that have name (in reverse order of binding)
    , values :: Map id value
    }

emptyScope :: forall id name value. Scope id name value
emptyScope =
  { names: Map.empty
  , clashes: Map.empty
  , values: Map.empty
  }

-- -- change
-- changeName :: forall id name value. Ord id => Ord name => id -> name -> name -> Scope id name value -> Scope id name value
-- changeName id name name' scope =
--   scope
--     { names = Map.insert id name' scope.names
--     , clashes =
--       Map.alter ()
--       Map.update (Just <<< List.filter (_ /= id)) name scope.clashes
--     }
-- add
addBinding :: forall id name value. Ord id => Ord name => id -> name -> value -> Scope id name value -> Scope id name value
addBinding id name value scope =
  scope
    { names = Map.insert id name scope.names
    , clashes =
      Map.alter
        ( case _ of
            Just ids -> Just $ id List.: ids
            Nothing -> Just $ List.singleton id
        )
        name
        scope.clashes
    , values = Map.insert id value scope.values
    }

addTermId :: forall r. TermId -> { name :: TermName | r } -> Type -> Context -> Context
addTermId id { name } alpha gamma = gamma { termScope = addBinding id name alpha gamma.termScope }

addTermBinding :: forall r. TermBinding -> { name :: TermName | r } -> Type -> Context -> Context
addTermBinding (TermBinding id _) = addTermId id

addTermUniqueBinding :: TermUniqueBinding -> Type -> Context -> Context
addTermUniqueBinding (TermUniqueBinding id meta) = addTermId id meta

addDefinitionBinding :: Definition -> Context -> Context
addDefinitionBinding (TermDefinition (TermUniqueBinding id { name }) alpha _ _) gamma = gamma { termScope = addBinding id name alpha gamma.termScope }

addDefinitionBinding (DataDefinition (TypeUniqueBinding id { name }) constrs _) gamma = gamma { typeScope = addBinding id name (map (\(Constructor (TermUniqueBinding idConstr _) _ _) -> idConstr) constrs) gamma.typeScope }

addDefinitionBindings :: List Definition -> Context -> Context
addDefinitionBindings defs gamma = List.foldl (\gamma def -> addDefinitionBinding def gamma) gamma defs

addParameterBinding :: TermId -> Parameter -> Context -> Context
addParameterBinding id (Parameter name alpha _) gamma = gamma { termScope = addBinding id name alpha gamma.termScope }

addParameterBindings :: List (Tuple TermBinding Parameter) -> Context -> Context
addParameterBindings ls gamma = List.foldl (\gamma (Tuple (TermBinding id _) (Parameter name alpha _)) -> gamma { termScope = addBinding id name alpha gamma.termScope }) gamma ls

-- get 
getName :: forall id name value. Ord id => Show id => id -> Scope id name value -> name
getName id scope = Unsafe.lookup id scope.names

getClash :: forall id name value. Ord name => Show name => name -> Scope id name value -> List id
getClash name scope = Unsafe.lookup name scope.clashes

getClashIndex :: forall id name value. Ord id => Show id => Ord name => Show name => id -> Scope id name value -> Int
getClashIndex id scope =
  let
    name = getName id scope

    ids = getClash name scope

    i = Unsafe.fromJust $ List.elemIndex id ids

    l = List.length ids
  in
    l - i - 1

getValue :: forall id name value. Ord id => Show id => id -> Scope id name value -> value
getValue id scope = Unsafe.lookup id scope.values

getType :: TermId -> Context -> Type
getType id gamma = getValue id gamma.termScope

getConstructorIds :: TypeId -> Context -> List TermId
getConstructorIds id gamma = getValue id gamma.typeScope

getTermNameClash :: TermName -> Context -> List TermId
getTermNameClash name gamma = getClash name gamma.termScope

getTermNameClashIndex :: TermId -> Context -> Int
getTermNameClashIndex id gamma = getClashIndex id gamma.termScope

getTypeNameClash :: TypeName -> Context -> List TypeId
getTypeNameClash name gamma = getClash name gamma.typeScope

getTypeNameClashIndex :: TypeId -> Context -> Int
getTypeNameClashIndex id gamma = getClashIndex id gamma.typeScope

getTermName :: TermId -> Context -> TermName
getTermName id gamma = Unsafe.lookup id gamma.termScope.names

getTypeName :: TypeId -> Context -> TypeName
getTypeName id gamma = Unsafe.lookup id gamma.typeScope.names

-- type Context
--   = { typeIdName :: Map TypeId TypeName
--     , typeIdConstructorIds :: Map TypeId (List.List TermId)
--     , termIdType :: Map TermId Type
--     , termNameIds :: Map TermName (List.List TermId)
--     , termIdName :: Map TermId TermName
--     , indentation :: Int
--     }
-- emptyContext :: Context
-- emptyContext =
--   { typeIdName: Map.empty
--   , typeIdConstructorIds: Map.empty
--   , termIdType: Map.empty
--   , termNameIds: Map.empty
--   , termIdName: Map.empty
--   , indentation: 0
--   }
-- -- gets
-- getTypeIdName :: TypeId -> Context -> TypeName
-- getTypeIdName id gamma = lookup id gamma.typeIdName
-- getTypeIdConstructorIds :: TypeId -> Context -> List.List TermId
-- getTypeIdConstructorIds id gamma = lookup id gamma.typeIdConstructorIds
-- getTermIdName :: TermId -> Context -> TermName
-- getTermIdName id gamma = lookup id gamma.termIdName
-- getTermIdType :: TermId -> Context -> Type
-- getTermIdType id gamma = lookup id gamma.termIdType
-- getTermNameIds :: TermName -> Context -> List.List TermId
-- getTermNameIds name gamma = lookup name gamma.termNameIds
-- -- adds
-- addTypeIdName :: TypeName -> TypeId -> Context -> Context
-- addTypeIdName name id gamma = gamma { typeIdName = Map.insert id name gamma.typeIdName }
-- addTermIdType :: TermId -> Type -> Context -> Context
-- addTermIdType id alpha gamma = gamma { termIdType = Map.insert id alpha gamma.termIdType }
-- addTermNameId :: TermName -> TermId -> Context -> Context
-- addTermNameId name id gamma =
--   gamma
--     { termNameIds =
--       Map.alter
--         ( case _ of
--             Just ids -> Just (id List.: ids)
--             Nothing -> Just (List.singleton id)
--         )
--         name
--         gamma.termNameIds
--     }
-- addTermIdName :: TermId -> TermName -> Context -> Context
-- addTermIdName id name gamma = gamma { termIdName = Map.insert id name gamma.termIdName }
-- addTermBinding :: TermName -> TermId -> Type -> Context -> Context
-- addTermBinding x id alpha =
--   addTermIdType id alpha
--     <<< addTermNameId x id
--     <<< addTermIdName id x
-- addTermBindings :: List.List TermName -> List.List TermId -> List.List Type -> Context -> Context
-- addTermBindings names ids alphas gamma = foldl (\gamma' (Tuple (Tuple name id) alpha) -> addTermBinding name id alpha gamma') gamma (List.zip (List.zip names ids) alphas)
-- addTypeBinding :: TypeName -> TypeId -> Context -> Context
-- addTypeBinding name id = addTypeIdName name id
-- addTypeIdConstructorIds :: TypeId -> List.List Constructor -> Context -> Context
-- addTypeIdConstructorIds idType constrs gamma =
--   -- add: constructor id => type
--   ( foldl
--       ( \gamma constr@(Constructor nameConstr idConstr prms) ->
--           addTermBinding nameConstr idConstr (typeOfConstructor idType constr) gamma
--       )
--       gamma
--       constrs
--   )
--     -- add: datatype id => constructor ids
--     { typeIdConstructorIds =
--       Map.insert idType
--         ( List.toUnfoldable
--             $ map (\(Constructor _ idConstr _) -> idConstr) constrs
--         )
--         gamma.typeIdConstructorIds
--     }
-- addDefinition :: Definition -> Context -> Context
-- addDefinition (TermDefinition name id alpha block) gamma = addTermBinding name id alpha gamma
-- addDefinition (DataDefinition name id constrs) gamma =
--   addTypeBinding name id
--     $ addTypeIdConstructorIds id constrs gamma
-- addDefinitions :: List.List Definition -> Context -> Context
-- addDefinitions defs gamma = foldl (\gamma def -> addDefinition def gamma) gamma defs
-- typeOfConstructor :: TypeId -> Constructor -> Type
-- typeOfConstructor idType (Constructor id alphas meta) =
--   if List.length alphas == 0 then
--     BaseType (DataType idType)
--   else
--     ArrowType alphas (DataType idType)
-- -- indentation
-- incrementIndentation :: Context -> Context
-- incrementIndentation gamma = gamma { indentation = gamma.indentation + 1 }
