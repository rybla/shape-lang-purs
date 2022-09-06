module Language.Shape.Stlc.Context where

import Data.Tuple
import Data.Tuple.Nested
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Record
import Control.Monad.Free (wrap)
import Data.Default (class Default, default)
import Data.Foldable (foldr, foldl)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Data.OrderedMap (OrderedMap)
import Data.OrderedMap as OrderedMap
import Debug as Debug
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
insertData data_ =
  foldr (>>>) identity
    [ (over Context $ modify _datas (OrderedMap.insert data_.typeBind.typeId data_)) -- insert datatype into context
    , foldr (\sumItem f -> insertVarType sumItem.termBind.termId (typeOfSumItem data_.typeBind.typeId sumItem) >>> f) identity data_.sumItems -- for each constructor, associate it with its appropriate type in varTypes
    , foldr (\sumItem f -> insertConstrDataType sumItem.termBind.termId { typeId: data_.typeBind.typeId, meta: default } >>> f) identity data_.sumItems -- for each constructor, associate it with this datatype in constrDataTypes
    ]

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

flattenType :: Type -> { doms :: List Type, cod :: Type }
flattenType type_ = case type_ of
  ArrowType { dom, cod } -> modify (Proxy :: Proxy "doms") (dom : _) (flattenType cod)
  _ -> { doms: Nil, cod: type_ }

-- TODO: make sure this is right
unflattenType :: { doms :: List Type, cod :: Type } -> Type
unflattenType { doms: Nil, cod } = cod
unflattenType { doms: Cons dom doms, cod } = ArrowType { dom, cod: unflattenType { doms, cod }, meta: default }

typeOfSumItem :: TypeId -> SumItem -> Type
typeOfSumItem typeId sumItem = unflattenType { doms: (_.type_) <$> sumItem.paramItems, cod: DataType { typeId, meta: default } }

-- Given `f : A -> B -> C` and `a : A`, computes that output type of `f a` is 
-- `B -> C`.
neuOutputType :: Type -> Neu -> Type
neuOutputType alpha neu =
  foldr
    (\dom cod -> ArrowType { dom, cod, meta: default })
    cod
    (List.drop (List.length neu.argItems) doms)
  where
  { doms, cod } = flattenType alpha
