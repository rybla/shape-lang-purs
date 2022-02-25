module Language.Shape.Stlc.Context where

import Data.Maybe
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Data.Array as Array
import Data.List as List
import Data.Map as Map
import Undefined (undefined)

type Context
  = { typeIdName :: Map.Map TypeId TypeName
    , principleNameDataType :: Map.Map TermId TypeId
    , termIdType :: Map.Map TermId Type
    , termNameIds :: Map.Map TermName (Array TermId)
    , termIdName :: Map.Map TermId TermName
    }

emptyContext :: Context
emptyContext =
  { typeIdName: Map.empty
  , principleNameDataType: Map.empty
  , termIdType: Map.empty
  , termNameIds: Map.empty
  , termIdName: Map.empty
  }

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
            Just ids -> Just (Array.cons id ids)
            Nothing -> Just [ id ]
        )
        name
        gamma.termNameIds
    }

addTermIdName :: TermId -> TermName -> Context -> Context
addTermIdName id name gamma = gamma { termIdName = Map.insert id name gamma.termIdName }

addUniqueTermBinding :: UniqueTermBinding -> Type -> Context -> Context
addUniqueTermBinding (UniqueTermBinding name id) alpha gamma =
  addTermNameId name id
    <<< addTermIdName id name
    <<< addTermIdType id alpha
    $ gamma

addTermBindinding :: TermBinding -> Type -> Context -> Context
addTermBindinding (TermBinding id) alpha gamma = addTermIdType id alpha gamma

addUniqueTypeBinding :: UniqueTypeBinding -> Context -> Context
addUniqueTypeBinding (UniqueTypeBinding name id) gamma = addTypeIdName name id gamma

addDefinitions :: List.List Definition -> Context -> Context
addDefinitions defs gamma =
  List.foldl
    ( \gamma -> case _ of
        TermDefinition x alpha _ -> addUniqueTermBinding x alpha gamma
        DataDefinition alpha _ -> addUniqueTypeBinding alpha gamma
    )
    gamma
    defs
