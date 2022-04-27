module Language.Shape.Stlc.Context where

import Data.Tuple
import Data.Tuple.Nested
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Record
import Control.Monad.Free (wrap)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))

-- | Context
newtype Context
  = Context
  { datas :: Map TypeId Data -- typeId => data
  , varTypes :: Map TermId Type -- varId => type
  , constrDataTypes :: Map TermId DataType -- constrId => dataType
  }

derive instance newTypeContext :: Newtype Context _

derive newtype instance showContext :: Show Context

_datas = Proxy :: Proxy "datas"

_varTypes = Proxy :: Proxy "varTypes"

_constrDataTypes = Proxy :: Proxy "constrDataTypes"

insertData :: Data -> Context -> Context
insertData data_ = over Context $ modify _datas (Map.insert data_.typeBind.typeId data_)

insertVarType :: TermId -> Type -> Context -> Context
insertVarType id type_ = over Context $ modify _varTypes (Map.insert id type_)

lookupVarType :: TermId -> Context -> Type
lookupVarType id ctx = case Map.lookup id (unwrap ctx).varTypes of
  Just type_ -> type_
  Nothing -> unsafeCrashWith $ "could not find " <> show id <> " in context " <> show ctx

insertConstrDataType :: TermId -> DataType -> Context -> Context
insertConstrDataType termId dataType = over Context $ modify _constrDataTypes (Map.insert termId dataType)

flattenType :: Type -> (List Type /\ Type)
flattenType type_ = case type_ of
  ArrowType { dom, cod } ->
    let
      doms /\ out = flattenType cod
    in
      (dom : doms) /\ out
  _ -> Nil /\ type_
