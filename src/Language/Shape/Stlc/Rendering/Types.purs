module Language.Shape.Stlc.Rendering.Types where

import Prelude
import Data.Default
import Prim hiding (Type)
import Data.Maybe
import Data.OrderedSet (OrderedSet)
import Language.Shape.Stlc.Context
import Language.Shape.Stlc.Metacontext
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Types
import Type.Proxy (Proxy(..))

type RenderEnvironment
  = { gamma :: Context
    , meta :: Metacontext
    , alpha :: Maybe Type
    , actions :: Array Action
    , holeIds :: OrderedSet HoleId
    }

_holeIds = Proxy :: Proxy "holeIds"

emptyRenderEnvironment :: RenderEnvironment
emptyRenderEnvironment =
  { gamma: default
  , meta: default
  , alpha: default
  , actions: []
  , holeIds: mempty
  }
