module Language.Shape.Stlc.Context where

import Data.Foldable
import Data.Maybe
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Data.Array as Array
import Data.List as List
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Unsafe (lookup)

type Context
  = { typeIdName :: Map.Map TypeId TypeName
    , typeIdConstructorIds :: Map.Map TypeId (List.List TermId)
    , termIdType :: Map.Map TermId Type
    , termNameIds :: Map.Map TermName (List.List TermId)
    , termIdName :: Map.Map TermId TermName
    }

emptyContext :: Context
emptyContext =
  { typeIdName: Map.empty
  , typeIdConstructorIds: Map.empty
  , termIdType: Map.empty
  , termNameIds: Map.empty
  , termIdName: Map.empty
  }

-- gets
getTypeIdName :: TypeId -> Context -> TypeName
getTypeIdName id gamma = lookup id gamma.typeIdName

getTypeIdConstructorIds :: TypeId -> Context -> List.List TermId
getTypeIdConstructorIds id gamma = lookup id gamma.typeIdConstructorIds

getTermIdName :: TermId -> Context -> TermName
getTermIdName id gamma = lookup id gamma.termIdName

getTermIdType :: TermId -> Context -> Type
getTermIdType id gamma = lookup id gamma.termIdType

getTermNameIds :: TermName -> Context -> List.List TermId
getTermNameIds name gamma = lookup name gamma.termNameIds

-- adds
addTypeIdName :: TypeName -> TypeId -> Context -> Context
addTypeIdName name id gamma = gamma { typeIdName = Map.insert id name gamma.typeIdName }

addTermIdType :: TermId -> Type -> Context -> Context
addTermIdType id alpha gamma = gamma { termIdType = Map.insert id alpha gamma.termIdType }

addTermNameId :: TermName -> TermId -> Context -> Context
addTermNameId name id gamma =
  gamma
    { termNameIds =
      Map.alter
        ( case _ of
            Just ids -> Just (id List.: ids)
            Nothing -> Just (List.singleton id)
        )
        name
        gamma.termNameIds
    }

addTermIdName :: TermId -> TermName -> Context -> Context
addTermIdName id name gamma = gamma { termIdName = Map.insert id name gamma.termIdName }

addTermBinding :: TermName -> TermId -> Type -> Context -> Context
addTermBinding x id alpha =
  addTermIdType id alpha
    <<< addTermNameId x id
    <<< addTermIdName id x

addTermBindings :: List.List TermName -> List.List TermId -> List.List Type -> Context -> Context
addTermBindings names ids alphas gamma = foldl (\gamma' (Tuple (Tuple name id) alpha) -> addTermBinding name id alpha gamma') gamma (List.zip (List.zip names ids) alphas)

addTypeBinding :: TypeName -> TypeId -> Context -> Context
addTypeBinding name id = addTypeIdName name id

addTypeIdConstructorIds :: TypeId -> List.List Constructor -> Context -> Context
addTypeIdConstructorIds idType constrs gamma =
  -- add: constructor id => type
  ( foldl
      ( \gamma constr@(Constructor nameConstr idConstr prms) ->
          addTermBinding nameConstr idConstr (typeOfConstructor idType constr) gamma
      )
      gamma
      constrs
  )
    -- add: datatype id => constructor ids
    { typeIdConstructorIds =
      Map.insert idType
        ( List.toUnfoldable
            $ map (\(Constructor _ idConstr _) -> idConstr) constrs
        )
        gamma.typeIdConstructorIds
    }

addDefinition :: Definition -> Context -> Context
addDefinition (TermDefinition name id alpha block) gamma = addTermBinding name id alpha gamma

addDefinition (DataDefinition name id constrs) gamma =
  addTypeBinding name id
    $ addTypeIdConstructorIds id constrs gamma

addDefinitions :: List.List Definition -> Context -> Context
addDefinitions defs gamma = foldl (\gamma def -> addDefinition def gamma) gamma defs

typeOfConstructor :: TypeId -> Constructor -> Type
typeOfConstructor idType (Constructor x id prms) =
  if List.length prms == 0 then
    BaseType (DataType idType)
  else
    ArrowType prms (DataType idType)
