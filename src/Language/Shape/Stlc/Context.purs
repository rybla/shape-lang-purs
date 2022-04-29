module Language.Shape.Stlc.Context where

import Data.Tuple
import Data.Tuple.Nested
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Record
import Control.Monad.Free (wrap)
import Data.Default (class Default)
import Data.List (List(..), (:))
import Data.OrderedMap (OrderedMap)
import Data.OrderedMap as OrderedMap
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))

-- | Context
newtype Context
  = Context
  { datas :: OrderedMap TypeId Data -- typeId => data
  , varTypes :: OrderedMap TermId Type -- varId => type
  , constrDataTypes :: OrderedMap TermId DataType -- constrId => dataType
  }

derive instance newTypeContext :: Newtype Context _

instance defaultContext :: Default Context where
  default =
    Context
      { datas: OrderedMap.empty
      , varTypes: OrderedMap.empty
      , constrDataTypes: OrderedMap.empty
      }

derive newtype instance showContext :: Show Context

_datas = Proxy :: Proxy "datas"

_varTypes = Proxy :: Proxy "varTypes"

_constrDataTypes = Proxy :: Proxy "constrDataTypes"

insertData :: Data -> Context -> Context
insertData data_ = over Context $ modify _datas (OrderedMap.insert data_.typeBind.typeId data_)

insertVarType :: TermId -> Type -> Context -> Context
insertVarType id type_ = over Context $ modify _varTypes (OrderedMap.insert id type_)

lookupVarType :: TermId -> Context -> Type
lookupVarType id ctx = case OrderedMap.lookup id (unwrap ctx).varTypes of
  Just type_ -> type_
  Nothing -> unsafeCrashWith $ "could not find " <> show id <> " in context " <> show ctx

insertConstrDataType :: TermId -> DataType -> Context -> Context
insertConstrDataType termId dataType = over Context $ modify _constrDataTypes (OrderedMap.insert termId dataType)

flattenType :: Type -> (List Type /\ Type)
flattenType type_ = case type_ of
  ArrowType { dom, cod } ->
    let
      doms /\ out = flattenType cod
    in
      (dom : doms) /\ out
  _ -> Nil /\ type_
