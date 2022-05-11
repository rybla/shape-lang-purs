module Language.Shape.Stlc.Rendering.Editor where

import Data.Tuple.Nested
import Language.Shape.Stlc.Rendering.Syntax
import Language.Shape.Stlc.Rendering.Token
import Language.Shape.Stlc.Types
import Prelude
import Control.Monad.State as State
import Data.Array as Array
import Data.List.Unsafe as List
import Data.Map.Unsafe as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap, wrap)
import Data.OrderedMap as OrderedMap
import Effect (Effect)
import Language.Shape.Stlc.Context (Context(..))
import Language.Shape.Stlc.Metacontext (Metacontext(..))
import Language.Shape.Stlc.Metadata (SumItemMetadata(..))
import Partial.Unsafe (unsafeCrashWith)
import React (ReactElement)
import React.DOM as DOM
import React.DOM.Props as Props
import Record as Record
import Type.Proxy (Proxy(..))
import Undefined (undefined)

-- | renderEditor
renderEditor :: This -> Effect ReactElement
renderEditor this = do
  elems /\ env <- renderProgram this
  pure $ DOM.div [ Props.className "editor" ]
    $ renderPanel this env
    <> elems

-- | renderPanel
renderPanel :: This -> RenderEnvironment -> Array ReactElement
renderPanel this env =
  [ DOM.div [ Props.className "panel" ]
      $ Array.concat
          [ renderEnvironment this env
          , renderPalette this env
          ]
  ]

renderEnvironment :: This -> RenderEnvironment -> Array ReactElement
renderEnvironment this env =
  [ DOM.div [ Props.className "environment" ]
      $ Array.concat
          [ [ DOM.div [ Props.className "header" ] [ DOM.text "Context" ] ]
          , renderContext env.gamma
          , Array.concat
              $ maybeArray env.alpha \alpha ->
                  Array.concat
                    [ [ DOM.div [ Props.className "environment-divider" ] [] ]
                    , flip State.evalState env $ undefined -- renderType this { syn: { type_: alpha }, gamma: { gamma: gamma }, ix: { visit: { ix: Nothing, csr: Nothing } }, meta: { meta: env.meta }, act: {} }
                    ]
          ]
  ]
  where
  renderContext gamma =
    [ DOM.div
        [ Props.className "context" ]
        [ DOM.div [ Props.className "context-datas" ] $ Array.fromFoldable $ renderData <$> OrderedMap.keys (unwrap gamma).datas
        , DOM.div [ Props.className "context-varTypes" ] $ Array.fromFoldable $ renderVarType <$> OrderedMap.keys (unwrap gamma).varTypes
        ]
    ]
    where
    renderData typeId =
      let
        data_ = OrderedMap.lookup'' "renderData.data_" typeId (unwrap gamma).datas

        meta' = Map.lookup'' "renderData.meta'" typeId (unwrap env.meta).dataMetas

        env' = env { meta = meta' }
      in
        DOM.span [ Props.className "context-data" ]
          $ Array.concat
              [ [ token.data1 ]
              , flip State.evalState env' $ undefined -- renderTypeBind this ?a
              -- { syn: { typeBind: data_.typeBind }, gamma: { gamma: gamma }, ix: { visit: { ix: Nothing, csr: Nothing } }, meta: { meta: meta' }, act: {} }
              ]

    renderVarType termId =
      let
        type_ = OrderedMap.lookup'' "renderVarType.type_" termId (unwrap gamma).varTypes

        meta' = Map.lookup'' "renderVarType.meta'" termId (unwrap env.meta).varMetas

        env' = env { meta = meta' }
      in
        DOM.span [ Props.className "context-varType" ]
          $ Array.concat
              [ flip State.evalState env' $ undefined -- renderTermId this { syn: { termId }, gamma: { gamma }, ix: { visit: { ix: Nothing, csr: Nothing } }, meta: { meta: meta' }, act: {} }
              , [ token.let2 ]
              , flip State.evalState env' $ undefined -- renderType this { syn: { type_ }, gamma: { gamma }, ix: { visit: { ix: Nothing, csr: Nothing } }, meta: { meta: meta' }, act: {} }
              ]

renderPalette :: This -> RenderEnvironment -> Array ReactElement
renderPalette this env =
  [ DOM.div [ Props.className "palette" ]
      [ DOM.div [ Props.className "header" ] [ DOM.text "Palette" ]
      , DOM.div [ Props.className "actions" ] $ Array.concat $ renderAction <$> env.actions
      ]
  ]
  where
  renderAction (Action action) = case action.label of
    Just str -> [ DOM.div [ Props.className "action" ] [ DOM.text str ] ]
    Nothing -> []
