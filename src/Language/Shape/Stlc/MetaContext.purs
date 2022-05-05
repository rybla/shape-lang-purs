module Language.Shape.Stlc.Metacontext where

import Data.Newtype
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Prelude
import Data.Default (class Default)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype as NT
import Data.Set (Set)
import Data.Set as Set
import Record as Record
import Type.Proxy (Proxy(..))
import Undefined (undefined)

newtype Metacontext
  = Metacontext
  { varNames :: Map TermId Name -- varId => varName
  , varShadows :: Map Name Int -- varName => shadowDepth
  , dataNames :: Map TypeId Name -- dataId => dataName
  , dataShadows :: Map Name Int -- dataName => shadowDepth
  , indentation :: Int
  }

derive instance newTypeMetacontext :: Newtype Metacontext _

derive newtype instance showMetacontext :: Show Metacontext

instance defaultMetacontext :: Default Metacontext where
  default =
    Metacontext
      { varNames: Map.empty
      , varShadows: Map.empty
      , dataNames: Map.empty
      , dataShadows: Map.empty
      , indentation: 0
      }

_varNames = Proxy :: Proxy "varNames"

_varShadows = Proxy :: Proxy "varShadows"

_dataNames = Proxy :: Proxy "dataNames"

_dataShadows = Proxy :: Proxy "dataShadows"

_indentation = Proxy :: Proxy "indentation"

insertVarName :: TermId -> Name -> Metacontext -> Metacontext
insertVarName termId name =
  over Metacontext
    $ Record.modify _varNames (Map.insert termId name)
    <<< Record.modify _varShadows (Map.alter (maybe (Just 0) (Just <<< (1 + _))) name)

insertDataName :: TypeId -> Name -> Metacontext -> Metacontext
insertDataName id name =
  over Metacontext
    $ Record.modify _dataNames (Map.insert id name)
    <<< Record.modify _dataShadows (Map.alter (maybe (Just 0) (Just <<< (1 + _))) name)

incrementIndentation :: Metacontext -> Metacontext
incrementIndentation = over Metacontext $ Record.modify _indentation (_ + 1)
