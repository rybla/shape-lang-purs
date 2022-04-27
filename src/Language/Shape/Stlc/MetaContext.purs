module Language.Shape.Stlc.MetaContext where

import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Prelude
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype
import Data.Newtype as NT
import Data.Set (Set)
import Data.Set as Set
import Record as Record
import Type.Proxy (Proxy(..))
import Undefined (undefined)

newtype MetaContext
  = MetaContext
  { varNames :: Map Id Name -- varId => varName
  , dataNames :: Map Id Name -- dataId => dataName
  , indentation :: Int
  }

derive instance newTypeMetaContext :: Newtype MetaContext _

_varNames = Proxy :: Proxy "varNames"

_dataNames = Proxy :: Proxy "dataNames"

_indentation = Proxy :: Proxy "indentation"

insertVarName :: Id -> Name -> MetaContext -> MetaContext
insertVarName id name = over MetaContext $ Record.modify _varNames (Map.insert id name)

insertDataName :: Id -> Name -> MetaContext -> MetaContext
insertDataName id name = over MetaContext $ Record.modify _dataNames (Map.insert id name)

incrementIndentation :: MetaContext -> MetaContext
incrementIndentation = over MetaContext $ Record.modify _indentation (_ + 1)
