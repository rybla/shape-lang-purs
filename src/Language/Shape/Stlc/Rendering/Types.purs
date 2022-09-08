module Language.Shape.Stlc.Rendering.Types where

import Data.Default
import Data.Maybe
import Language.Shape.Stlc.Context
import Language.Shape.Stlc.Metacontext
import Language.Shape.Stlc.Recursor.Index
import Language.Shape.Stlc.Rendering.Token
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Types
import Prelude
import Prim hiding (Type)
import Control.Monad.State as State
import Data.OrderedSet (OrderedSet)
import Type.Proxy (Proxy(..))

type M a
  = State.State RenderEnvironment a

type RenderEnvironment
  = { syntax :: Maybe Syntax -- selected syntax
    , gamma :: Context
    , meta :: Metacontext
    , alpha :: Maybe Type
    , actions :: Array Action
    , mb_queryResult :: Maybe { action :: Action, n :: Int }
    , holeIds :: OrderedSet HoleId
    , st :: State
    , syntaxtheme :: SyntaxTheme
    }

-- -- TODO: needed?
-- data QueryResult
--   = TermVariableQueryResult { name :: Name, termId :: TermId, type_ :: Type }
--   | DataTypeQueryResult { name :: Name, typeId :: TypeId }
--   | ActionQueryResult Action
_holeIds = Proxy :: Proxy "holeIds"

emptyRenderEnvironment :: State -> RenderEnvironment
emptyRenderEnvironment st =
  { syntax: Nothing
  , gamma: default
  , meta: default
  , alpha: default
  , actions: []
  , mb_queryResult: Nothing 
  , holeIds: mempty
  , st
  , syntaxtheme: st.syntaxtheme
  }

defaultNodeProps :: NodeProps
defaultNodeProps =
  { syntax: default
  , label: default
  , visit: nonVisit
  , alpha: default
  , gamma: default
  , meta: default
  , actions: []
  }

makeNodeProps :: forall r. { gamma :: Context, visit :: Visit, meta :: Metacontext, actions :: Array Action | r } -> NodeProps
makeNodeProps { gamma, visit, meta, actions } =
  defaultNodeProps
    { gamma = gamma
    , visit = visit
    , meta = meta
    , actions = actions
    }

type NodeProps
  = { syntax :: Maybe Syntax
    , label :: Maybe String
    , gamma :: Context
    , alpha :: Maybe Type
    , visit :: Visit
    , meta :: Metacontext
    , actions :: Array Action
    }
