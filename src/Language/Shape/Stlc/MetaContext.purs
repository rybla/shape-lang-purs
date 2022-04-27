module Language.Shape.Stlc.MetaContext where

import Data.Newtype
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Prelude
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype as NT
import Data.Set (Set)
import Data.Set as Set
import Record as Record
import Type.Proxy (Proxy(..))
import Undefined (undefined)

newtype MetaContext
  = MetaContext
  { varNames :: Map TermId Name -- varId => varName
  , varShadows :: Map Name Int -- varName => shadowDepth
  , dataNames :: Map TypeId Name -- dataId => dataName
  , dataShadows :: Map Name Int -- dataName => shadowDepth
  , indentation :: Int
  }

derive instance newTypeMetaContext :: Newtype MetaContext _

_varNames = Proxy :: Proxy "varNames"

_varShadows = Proxy :: Proxy "varShadows"

_dataNames = Proxy :: Proxy "dataNames"

_dataShadows = Proxy :: Proxy "dataShadows"

_indentation = Proxy :: Proxy "indentation"

insertVarName :: TermId -> Name -> MetaContext -> MetaContext
insertVarName termId name =
  over MetaContext
    $ Record.modify _varNames (Map.insert termId name)
    <<< Record.modify _varShadows (Map.alter (maybe (Just 0) (Just <<< (1 + _))) name)

insertDataName :: TypeId -> Name -> MetaContext -> MetaContext
insertDataName id name =
  over MetaContext
    $ Record.modify _dataNames (Map.insert id name)
    <<< Record.modify _dataShadows (Map.alter (maybe (Just 0) (Just <<< (1 + _))) name)

incrementIndentation :: MetaContext -> MetaContext
incrementIndentation = over MetaContext $ Record.modify _indentation (_ + 1)
