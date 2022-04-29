module Language.Shape.Stlc.Rendering.Syntax where

import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Control.Monad.State (State, evalState, get)
import Data.Array (concat)
import Data.Default (default)
import Data.Maybe (Maybe(..))
import Data.OrderedSet (OrderedSet)
import Data.OrderedSet as OrderedSet
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (sequence)
import Language.Shape.Stlc.Context (Context(..))
import Language.Shape.Stlc.Metacontext (Metacontext(..))
import Language.Shape.Stlc.Recursor.Action as RecAct
import Language.Shape.Stlc.Recursor.Context as RecCtx
import Language.Shape.Stlc.Recursor.Index (Visit)
import Language.Shape.Stlc.Recursor.Index as RecIx
import Language.Shape.Stlc.Recursor.Metacontext as RecMeta
import Language.Shape.Stlc.Rendering.Keyword (token)
import Language.Shape.Stlc.Types (Action(..), This, getState')
import Prim.Row (class Union)
import React (ReactElement)
import React.DOM (span, text)
import React.DOM.Props (className)
import Record (union)
import Undefined (undefined)

renderProgram :: This -> Array ReactElement
renderProgram this =
  flip evalState mempty
    $ renderTerm
        -- TODO: maybe pull this out into multiple files or at least somewhere else?
        { argsAct: {}
        , argsCtx: { ctx: default, type_: HoleType { holeId: freshHoleId unit, weakening: Set.empty, meta: default } }
        , argsMeta: { meta: default }
        , argsIx: { visit: { csr: Just st.ix, ix: mempty } }
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
            [ renderType { argsSyn: { type_: args.argsSyn.arrow.dom }, argsCtx: args.argsCtx, argsIx: { visit: args.argsIx.visit_dom }, argsMeta: { meta: args.argsMeta.meta }, argsAct: {} }
            , pure [ token.arrowType1 ]
            , renderType { argsSyn: { type_: args.argsSyn.arrow.cod }, argsCtx: args.argsCtx, argsIx: { visit: args.argsIx.visit_cod }, argsMeta: { meta: args.argsMeta.meta_cod }, argsAct: {} }
            ]
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
            [ do
                i <- OrderedSet.findIndex (args.argsSyn.hole.holeId == _) <$> get
                pure
                  [ span [ className "holeId" ] [ text $ show i ]
                  ]
            ]
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

maybeArray :: forall a b. Maybe a -> (a -> b) -> Array b
maybeArray ma f = case ma of
  Just a -> [ f a ]
  Nothing -> []

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

renderNode :: NodeProps -> Array (State (OrderedSet HoleId) (Array ReactElement)) -> State (OrderedSet HoleId) (Array ReactElement)
renderNode props elemsM = do
  elems <- concat <$> sequence elemsM
  pure
    $ [ span
          (className # maybeArray props.label)
          elems
      ]
