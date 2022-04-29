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
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Data.OrderedMap (OrderedMap)
import Data.OrderedMap as OrderedMap
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))
import Undefined (undefined)

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

lookupData :: TypeId -> Context -> Data 
lookupData typeId ctx = case OrderedMap.lookup typeId (unwrap ctx).datas of 
  Just data_ -> data_ 
  Nothing -> unsafeCrashWith $ "could not find TypeId " <> show typeId <> " in context " <> show ctx 

insertVarType :: TermId -> Type -> Context -> Context
insertVarType termId type_ = over Context $ modify _varTypes (OrderedMap.insert termId type_)

lookupVarType :: TermId -> Context -> Type
lookupVarType termId ctx = case OrderedMap.lookup termId (unwrap ctx).varTypes of
  Just type_ -> type_
  Nothing -> unsafeCrashWith $ "could not find TermId " <> show termId <> " in context " <> show ctx

insertConstrDataType :: TermId -> DataType -> Context -> Context
insertConstrDataType termId dataType = over Context $ modify _constrDataTypes (OrderedMap.insert termId dataType)

flattenType :: Type -> {doms::List Type, cod:: Type}
flattenType type_ = case type_ of
  ArrowType { dom, cod } ->
    modify (Proxy :: Proxy "doms") (dom : _) (flattenType cod)
  _ -> {doms: Nil, cod: type_}

typeOfSumItem :: TypeId -> SumItem -> Type 
typeOfSumItem = undefined
