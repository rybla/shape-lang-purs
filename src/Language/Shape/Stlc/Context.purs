module Language.Shape.Stlc.Context where

import Data.Tuple
import Data.Tuple.Nested
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Record
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))

-- | Context
type Context
  = { datas :: Map Id Data -- typeId => data
    , varTypes :: Map Id Type -- varId => type
    , constrDataTypes :: Map Id DataType -- constrId => dataType
    }

_datas = Proxy :: Proxy "datas"

_varTypes = Proxy :: Proxy "varTypes"

_constrDataTypes = Proxy :: Proxy "constrDataTypes"

insertData :: Data -> Context -> Context
insertData data_ = modify _datas (Map.insert data_.id data_)

insertVarType :: Id -> Type -> Context -> Context
insertVarType id type_ = modify _varTypes (Map.insert id type_)

lookupVarType :: Id -> Context -> Type
lookupVarType id ctx = case Map.lookup id ctx.varTypes of
  Just type_ -> type_
  Nothing -> unsafeCrashWith $ "could not find " <> show id <> " in context " <> show ctx

insertConstrDataType :: Id -> DataType -> Context -> Context
insertConstrDataType id dataType = modify _constrDataTypes (Map.insert id dataType)

flattenType :: Type -> (List Type /\ Type)
flattenType type_ = case type_ of
  ArrowType { dom, cod } ->
    let
      doms /\ out = flattenType cod
    in
      (dom : doms) /\ out
  _ -> Nil /\ type_
