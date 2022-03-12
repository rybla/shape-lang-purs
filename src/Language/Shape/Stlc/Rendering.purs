module Language.Shape.Stlc.Rendering where

import Data.Tuple.Nested
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.RenderingAux
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Data.List.Unsafe (List)
import Data.List.Unsafe as List
import Data.Map.Unsafe as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Symbol (class IsSymbol)
import Data.Tuple as Tuple
import Debug as Debug
import Language.Shape.Stlc.Changes
import Language.Shape.Stlc.Recursion.MetaContext
import Language.Shape.Stlc.Recursion.MetaContext as RecMeta
import Language.Shape.Stlc.Index as Index
import Language.Shape.Stlc.Recursion.Index as RecIndex
import Language.Shape.Stlc.Typing
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe as Unsafe
