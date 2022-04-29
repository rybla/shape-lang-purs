module Language.Shape.Stlc.Rendering.Syntax where

import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Data.Default (default)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Language.Shape.Stlc.Context (Context(..))
import Language.Shape.Stlc.Metacontext (Metacontext(..))
import Language.Shape.Stlc.Recursor.Action as RecAct
import Language.Shape.Stlc.Recursor.Context as RecCtx
import Language.Shape.Stlc.Recursor.Index (Visit)
import Language.Shape.Stlc.Recursor.Index as RecIx
import Language.Shape.Stlc.Recursor.Metacontext as RecMeta
import Language.Shape.Stlc.Types (Action(..), This, getState')
import Prim.Row (class Union)
import React (ReactElement)
import React.DOM (span)
import Record (union)
import Undefined (undefined)

renderProgram :: This -> Array ReactElement
renderProgram this =
  renderTerm
    -- TODO: maybe pull this out into multiple files or at least somewhere else?
    { argsAct: {}
    , argsCtx: { ctx: default, type_: HoleType { holeId: freshHoleId unit, weakening: Set.empty, meta: default } }
    , argsIx: { visit: { csr: Just st.ix, ix: mempty } }
    , argsMeta: { meta: default }
    , argsSyn: { term: st.term }
    }
  where
  st = getState' this

renderType :: RecAct.ProtoRec RecAct.ArgsType () (Array ReactElement)
renderType =
  RecAct.recType
    { arrow:
        \args ->
          renderNode
            ( (useArgsCtx_Type args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "ArrowType" }
            )
            []
    , data_:
        \args ->
          renderNode
            ( (useArgsCtx_Type args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "DataType" }
            )
            []
    , hole:
        \args ->
          renderNode
            ( (useArgsCtx_Type args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "HoleType" }
            )
            []
    }

renderTerm :: RecAct.ProtoRec RecAct.ArgsTerm () (Array ReactElement)
renderTerm = undefined

type NodeProps
  = { label :: Maybe String
    , visit :: Maybe Visit
    , type_ :: Maybe Type
    , ctx :: Maybe Context
    , meta :: Maybe Metacontext
    , actions :: Array Action
    }

defaultNodeProps :: NodeProps
defaultNodeProps =
  { label: default
  , visit: default
  , type_: default
  , ctx: default
  , meta: default
  , actions: []
  }

useArgsCtx_Type :: forall r1 r2. Record (RecCtx.ProtoArgsType r1 r2) -> NodeProps -> NodeProps
useArgsCtx_Type { argsCtx } = _ { ctx = Just argsCtx.ctx }

useArgsCtx_Term :: forall r1 r2. Record (RecCtx.ProtoArgsTerm r1 r2) -> NodeProps -> NodeProps
useArgsCtx_Term { argsCtx } = _ { ctx = Just argsCtx.ctx, type_ = Just argsCtx.type_ }

useArgsMeta :: forall r1 r2. Record (RecMeta.ProtoArgs r1 r2) -> NodeProps -> NodeProps
useArgsMeta { argsMeta } = _ { meta = Just argsMeta.meta }

useArgsIx :: forall r1 r2. Record (RecIx.ProtoArgs r1 r2) -> NodeProps -> NodeProps
useArgsIx { argsIx } = _ { visit = Just (argsIx.visit) }

useArgsAct :: forall r1 r2. Record (RecAct.ProtoArgs ( actions :: Array Action | r1 ) r2) -> NodeProps -> NodeProps
useArgsAct { argsAct } = _ { actions = argsAct.actions }

renderNode :: NodeProps -> Array ReactElement -> Array ReactElement
renderNode props = undefined
