module Language.Shape.Stlc.Metacontext where

import Data.Newtype
import Data.Tuple.Nested
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
import Data.String as String
import Record as Record
import Type.Proxy (Proxy(..))
import Undefined (undefined)

newtype Metacontext
  = Metacontext (Record MetacontextRow)

type MetacontextRow
  = ( varNames :: Map TermId Name -- varId => varName
    , varShadows :: Map Name Int -- varName => shadowDepth
    , varMetas :: Map TermId Metacontext
    , dataNames :: Map TypeId Name -- dataId => dataName
    , dataShadows :: Map Name Int -- dataName => shadowDepth
    , dataMetas :: Map TypeId Metacontext
    , indentation :: Int
    )

instance newtypeMetacontext :: Newtype Metacontext (Record MetacontextRow)

instance showMetacontext :: Show Metacontext where
  show (Metacontext m) =
    String.joinWith ", " <<< map (\(l /\ t) -> l <> ": " <> t)
      $ [ "varNames" /\ show m.varNames
        , "varShadows" /\ show m.varShadows
        , "varMetas" /\ show m.varMetas
        , "dataNames" /\ show m.dataNames
        , "dataShadows" /\ show m.dataShadows
        , "dataMetas" /\ show m.dataMetas
        , "indentation" /\ show m.indentation
        ]

instance defaultMetacontext :: Default Metacontext where
  default =
    Metacontext
      { varNames: Map.empty
      , varShadows: Map.empty
      , varMetas: Map.empty
      , dataNames: Map.empty
      , dataShadows: Map.empty
      , dataMetas: Map.empty
      , indentation: 0
      }

_varNames = Proxy :: Proxy "varNames"

_varShadows = Proxy :: Proxy "varShadows"

_varMetas = Proxy :: Proxy "varMetas"

_dataNames = Proxy :: Proxy "dataNames"

_dataShadows = Proxy :: Proxy "dataShadows"

_dataMetas = Proxy :: Proxy "dataMetas"

_indentation = Proxy :: Proxy "indentation"

insertVar :: TermId -> Name -> Metacontext -> Metacontext
insertVar termId name =
  (\meta -> over Metacontext (Record.modify _varMetas (Map.insert termId meta)) meta)
    <<< over Metacontext
        ( Record.modify _varNames (Map.insert termId name)
            <<< Record.modify _varShadows (Map.alter (maybe (Just 0) (Just <<< (1 + _))) name)
        )

-- TODO: need to add constructors
insertData :: Data -> Metacontext -> Metacontext
insertData data_ =
  (\meta -> over Metacontext (Record.modify _dataMetas (Map.insert data_.typeBind.typeId meta)) meta)
    <<< over Metacontext
        ( Record.modify _dataNames (Map.insert data_.typeBind.typeId (unwrap data_.typeBind.meta).name)
            <<< Record.modify _dataShadows (Map.alter (maybe (Just 0) (Just <<< (1 + _))) (unwrap data_.typeBind.meta).name)
        )

incrementIndentation :: Metacontext -> Metacontext
incrementIndentation = over Metacontext $ Record.modify _indentation (_ + 1)

resetIndentation :: Metacontext -> Metacontext
resetIndentation = over Metacontext $ Record.set _indentation 0
