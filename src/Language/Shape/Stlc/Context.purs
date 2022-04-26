module Language.Shape.Stlc.Context where

import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Record
import Data.Map (Map)
import Data.Map as Map
import Type.Proxy (Proxy(..))

-- | Context
type Context
  = { datas :: Map Id Data -- typeId => data
    , types :: Map Id Type -- varId => type
    , dataTypes :: Map Id DataType -- constrId => dataType
    }

_datas = Proxy :: Proxy "datas"

_types = Proxy :: Proxy "types"

_dataTypes = Proxy :: Proxy "dataTypes"

insertData :: Data -> Context -> Context
insertData data_ = modify _datas (Map.insert data_.id data_)

insertType :: Id -> Type -> Context -> Context
insertType id type_ = modify _types (Map.insert id type_)

insertDataType :: Id -> DataType -> Context -> Context
insertDataType id dataType = modify _dataTypes (Map.insert id dataType)
