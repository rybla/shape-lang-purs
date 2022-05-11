module Language.Shape.Stlc.Metacontext where

import Data.Newtype
import Data.Foldable
import Data.Tuple.Nested
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Prelude
import Data.Default (class Default)
import Data.Map.Unsafe (Map)
import Data.Map.Unsafe as Map
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
    , varShadowIndices :: Map TermId Int -- varId => shadowIndex
    , varShadowDepths :: Map Name Int -- varName => shadowDepth
    , dataNames :: Map TypeId Name -- dataId => dataName
    , dataShadowIndices :: Map TypeId Int -- dataName => shadowIndex
    , dataShadowDepths :: Map Name Int -- dataName => shadowDepth
    , indentation :: Int
    )

instance newtypeMetacontext :: Newtype Metacontext (Record MetacontextRow)

derive newtype instance showMetacontext :: Show Metacontext

instance defaultMetacontext :: Default Metacontext where
  default =
    Metacontext
      { varNames: Map.empty
      , varShadowIndices: Map.empty
      , varShadowDepths: Map.empty
      , dataNames: Map.empty
      , dataShadowIndices: Map.empty
      , dataShadowDepths: Map.empty
      , indentation: 0
      }

_varNames = Proxy :: Proxy "varNames"

_varShadowDepths = Proxy :: Proxy "varShadowDepths"

_varShadowIndices = Proxy :: Proxy "varShadowIndices"

_dataNames = Proxy :: Proxy "dataNames"

_dataShadowDepths = Proxy :: Proxy "dataShadowDepths"

_dataShadowIndices = Proxy :: Proxy "dataShadowIndices"

_indentation = Proxy :: Proxy "indentation"

insertVar :: TermId -> Name -> Metacontext -> Metacontext
insertVar termId name meta =
  let
    shadowDepth' = case Map.lookup name (unwrap meta).varShadowDepths of
      Just shadowDepth -> shadowDepth + 1
      Nothing -> 0
  in
    over Metacontext
      ( Record.modify _varNames (Map.insert termId name) -- set name
          <<< Record.modify _varShadowDepths (Map.insert name shadowDepth') -- increment shadow depth
          <<< Record.modify _varShadowIndices (Map.insert termId shadowDepth') -- set shadow index
      )
      meta

insertData :: Data -> Metacontext -> Metacontext
insertData data_ meta =
  let
    typeId = data_.typeBind.typeId

    name = (unwrap data_.typeBind.meta).name

    shadowDepth' = case Map.lookup name (unwrap meta).dataShadowDepths of
      Just shadowDepth -> shadowDepth + 1
      Nothing -> 0
  in
    -- (\meta -> foldl (\meta sumItem -> insertVar sumItem.termBind.termId (unwrap sumItem.termBind.meta).name meta) meta data_.sumItems) -- * unflipped version
    flip (foldl (flip (\sumItem -> insertVar sumItem.termBind.termId (unwrap sumItem.termBind.meta).name))) data_.sumItems
      $ over Metacontext
          ( Record.modify _dataNames (Map.insert typeId name) -- set name
              <<< Record.modify _dataShadowDepths (Map.insert name shadowDepth') -- increment shadow depth
              <<< Record.modify _dataShadowIndices (Map.insert typeId shadowDepth') -- set shadow index
          )
          meta


incrementIndentation :: Metacontext -> Metacontext
incrementIndentation = over Metacontext $ Record.modify _indentation (_ + 1)

resetIndentation :: Metacontext -> Metacontext
resetIndentation = over Metacontext $ Record.set _indentation 0
