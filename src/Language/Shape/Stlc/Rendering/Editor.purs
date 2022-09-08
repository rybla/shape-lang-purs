module Language.Shape.Stlc.Rendering.Editor where

import Data.Tuple.Nested
import Language.Shape.Stlc.Action
import Language.Shape.Stlc.Recursor.Index
import Language.Shape.Stlc.Rendering.Syntax
import Language.Shape.Stlc.Rendering.Types
import Language.Shape.Stlc.Rendering.Utilities
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Types
import Prelude
import Control.Monad.State as State
import Data.Array as Array
import Data.List.Unsafe as List
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.OrderedMap as OrderedMap
import Effect (Effect)
import React (ReactElement)
import React.DOM as DOM
import React.DOM.Props as Props

-- | renderEditor
renderEditor :: This -> Effect (RenderEnvironment /\ Array (ReactElement))
renderEditor this = do
  res /\ env <- renderProgram this
  let
    actions = case env.st.mode of
      TopMode _topMode ->
        [ undo
        , gotoCursorTop
        , gotoCursorBottom
        ]
      SelectMode _selMode ->
        Array.concat
          [ [ undo ]
          , [ stepCursorForwards, stepCursorBackwards ]
          , [ indent ]
          , env.actions
          , [ escape ]
          , [ editQuery ]
          ]
      DragMode _dragMode ->
        [ escape
        ]
  env <- pure $ env { actions = actions }
  pure $ env
    /\ [ DOM.div [ Props.className "editor" ]
          $ Array.concat
              [ res
              , renderPanel this env
              ]
      ]

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
                    , [ DOM.div [ Props.className "environment-goal-wrapper" ] <<< pure
                          $ DOM.div [ Props.className "environment-goal" ]
                          $ flip State.evalState env
                          $ renderType
                              { this, syntaxtheme: env.syntaxtheme }
                              { type_: alpha, gamma: env.gamma, visit: nonVisit, meta: env.meta }
                      ]
                    ]
          ]
  ]
  where
  renderContext gamma =
    [ DOM.div
        [ Props.className "context" ]
        [ DOM.div [ Props.className "context-datas" ]
            $ Array.fromFoldable
            $ renderData
            <$> List.reverse (OrderedMap.keys (unwrap gamma).datas)
        , DOM.div [ Props.className "context-varTypes" ] $ Array.fromFoldable
            $ renderVarType
            <$> List.reverse (OrderedMap.keys (unwrap gamma).varTypes)
        ]
    ]
    where
    renderData typeId = do
      let
        data_ = OrderedMap.lookup'' "renderData.data_" typeId (unwrap gamma).datas
      let
        dataContextItem =
          flip State.evalState env do
            typeBind <-
              renderTypeBind { this, syntaxtheme: env.syntaxtheme }
                { typeBind: data_.typeBind
                , gamma: gamma
                , visit: nonVisit
                , meta: env.meta
                }
            pure $ env.syntaxtheme.dataContextItem { typeBind, metactx: env.meta }
      DOM.span [ Props.className "context-data-wrapper" ] <<< pure
        $ DOM.span [ Props.className "context-data context-item" ]
        $ [ DOM.span
              [ Props.onClick \event -> case env.syntax of
                  Just (SyntaxType (HoleType holeType)) ->
                    doAction
                      { this, event: MouseActionTrigger event }
                      (pasteDatatype { holeType, typeId })
                  Just (SyntaxTerm (Hole _hole)) ->
                    doAction
                      { this, event: MouseActionTrigger event }
                      (pasteMatch { data_, typeId })
                  _ -> pure unit
              ]
              dataContextItem
          ]

    renderVarType termId =
      let
        type_ = OrderedMap.lookup'' "renderVarType.type_" termId (unwrap gamma).varTypes

        varContextItem =
          flip State.evalState env do
            termId <- do
              termId <-
                renderTermId
                  { this, syntaxtheme: env.syntaxtheme }
                  { termId: termId, gamma: gamma, visit: nonVisit, meta: env.meta }
              pure $ [ DOM.span [ Props.className "context-varType-var" ] termId ]
            type_ <- do
              type_ <-
                renderType
                  { this, syntaxtheme: env.syntaxtheme }
                  { type_: type_, gamma: gamma, visit: nonVisit, meta: env.meta }
              pure $ [ DOM.span [ Props.className "context-varType-type" ] type_ ]
            pure $ env.syntaxtheme.varContextItem { termId, type_, metactx: env.meta }
      in
        DOM.span [ Props.className "context-varType-wrapper" ] <<< pure
          $ DOM.span
              [ Props.className "context-varType context-item"
              , Props.onClick \event ->
                  doAction
                    { this, event: MouseActionTrigger event }
                    (pasteVar { env, type_, termId })
              ]
              varContextItem

renderPalette :: This -> RenderEnvironment -> Array ReactElement
renderPalette this env =
  [ DOM.div [ Props.className "palette-wrapper" ]
      [ DOM.div [ Props.className "palette" ]
          [ DOM.div [ Props.className "header" ]
              [ DOM.text "Palette" ]
          , DOM.div [ Props.className "actions-wrapper" ]
              [ DOM.div [ Props.className "actions" ]
                  $ Array.concat
                  $ renderAction
                  <$> Array.filter (_.queryable <<< unwrap) env.actions
              ]
          ]
      ]
  ]
  where
  renderAction action =
    [ DOM.div [ Props.className "action-wrapper" ] <<< pure
        $ DOM.div
            ( [ Props.className "action"
              , Props.onClick \event ->
                  doAction
                    { this, event: MouseActionTrigger event }
                    action
              ]
                <> maybeArray (unwrap action).tooltip Props.title
            )
        $ [ DOM.div [ Props.className "action-label" ]
              [ DOM.text (unwrap action).label ]
          , DOM.div [ Props.className "action-shortcuts" ]
              $ ( \shortcut ->
                    DOM.div [ Props.className "action-shortcut" ]
                      [ DOM.text (show shortcut) ]
                )
              <$> (unwrap action).shortcuts
          ]
    ]

{-
  


























































































