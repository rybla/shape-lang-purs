module Language.Shape.Stlc.Rendering.Editor where

import Data.Tuple.Nested
import Language.Shape.Stlc.Action as Action
import Language.Shape.Stlc.ChAtIndex
import Language.Shape.Stlc.Changes
import Language.Shape.Stlc.Context
import Language.Shape.Stlc.CopyPasteBackend
import Language.Shape.Stlc.Hole
import Language.Shape.Stlc.Key
import Language.Shape.Stlc.Metacontext
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Recursor.Action
import Language.Shape.Stlc.Recursor.Index
import Language.Shape.Stlc.Rendering.Syntax
import Language.Shape.Stlc.Rendering.Token
import Language.Shape.Stlc.Rendering.Types
import Language.Shape.Stlc.Rendering.Utilities
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Types
import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.State (get)
import Control.Monad.State as State
import Data.Array as Array
import Data.Default (default)
import Data.List.Unsafe as List
import Data.Map.Unsafe as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (over, unwrap, wrap)
import Data.OrderedMap as OrderedMap
import Debug as Debug
import Effect (Effect)
import Effect.Console as Console
import KeyboardCursor (getLastIndex, stepCursorBackwards, stepCursorForwards)
import Language.Shape.Stlc.Index (nilIxDown)
import Language.Shape.Stlc.Transition
import Partial.Unsafe (unsafeCrashWith)
import React (ReactElement, getState, modifyState)
import React.DOM as DOM
import React.DOM.Props as Props
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe (error)

-- | renderEditor
renderEditor :: This -> Effect (RenderEnvironment /\ Array (ReactElement))
renderEditor this = do
  res /\ env <- renderProgram this
  let
    actions = case env.st.mode of
      TopMode topMode ->
        [ Action.undo
        , Action.stepCursorForwards
        , Action.stepCursorBackwards
        ]
      SelectMode selMode ->
        Array.concat
          [ env.actions
          , [ Action.undo ]
          , [ Action.stepCursorForwards ]
          , [ Action.stepCursorBackwards ]
          ]
      QueryMode queMode -> error "TODO"
      DragMode dragMode -> error "TODO"
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
                          $ renderType { this, syntaxtheme: env.syntaxtheme } { type_: alpha, gamma: env.gamma, visit: nonVisit, meta: env.meta }
                      ]
                    ]
          ]
  ]
  where
  renderContext gamma =
    -- Debug.trace (show env) \_ ->
    [ DOM.div
        [ Props.className "context" ]
        [ DOM.div [ Props.className "context-datas" ] $ Array.fromFoldable $ renderData <$> List.reverse (OrderedMap.keys (unwrap gamma).datas)
        , DOM.div [ Props.className "context-varTypes" ] $ Array.fromFoldable $ renderVarType <$> List.reverse (OrderedMap.keys (unwrap gamma).varTypes)
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
                    doTransition { this, event: MouseTransitionEvent event }
                      { label: "paste datatype"
                      , effect:
                          do
                            state <- get
                            selMode <- requireSelectMode
                            holeSub <- pure $ Map.singleton holeType.holeId (DataType { typeId, meta: default })
                            term <- pure $ subTerm holeSub state.program.term
                            type_ <- pure $ subType holeSub state.program.type_
                            setProgram { term, type_ }
                      }
                  Just (SyntaxTerm (Hole hole)) ->
                    doTransition { this, event: MouseTransitionEvent event }
                      { label: "match with datatype"
                      , effect:
                          do
                            selMode <- requireSelectMode
                            applyChange
                              { ix: selMode.ix
                              , toReplace:
                                  ReplaceTerm
                                    ( Match
                                        { typeId: typeId
                                        , term: Hole { meta: default }
                                        , caseItems:
                                            ( \sumItem ->
                                                { termBindItems: (\_ -> { termBind: freshTermBind unit, meta: default }) <$> sumItem.paramItems
                                                , body: Hole { meta: default }
                                                , meta: default
                                                }
                                            )
                                              <$> data_.sumItems
                                        , meta: default
                                        }
                                    )
                                    NoChange
                              }
                      }
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
              -- termId <- renderTermId { this: error "TODO", syntaxtheme: env.syntaxtheme } { termId: termId, gamma: gamma, visit: nonVisit, meta: env.meta }
              -- pure $ [ DOM.span [ Props.className "context-varType-var" ] termId ]
              error "TODO"
            type_ <- do
              type_ <- renderType { this: error "TODO", syntaxtheme: env.syntaxtheme } { type_: type_, gamma: gamma, visit: nonVisit, meta: env.meta }
              pure $ [ DOM.span [ Props.className "context-varType-type" ] type_ ]
            pure $ env.syntaxtheme.varContextItem { termId, type_, metactx: env.meta }
      in
        DOM.span [ Props.className "context-varType-wrapper" ] <<< pure
          $ DOM.span
              [ Props.className "context-varType context-item"
              , Props.onClick \event ->
                  doTransition { this, event: MouseTransitionEvent event }
                    { label: "paste variable"
                    , effect:
                        do
                          selMode <- requireSelectMode
                          alpha <- maybeTransitionM "rendering environment doesn't have type" env.alpha
                          nArgs /\ holeSub <-
                            maybeTransitionM "variable doesn't fit in hole"
                              $ fitsInHole type_ alpha
                          term <- pure $ createNeu termId nArgs
                          -- TODO: do I actually need to do the holeSub? or does that happen automatically via applyChange?
                          applyChange
                            { ix: selMode.ix
                            , toReplace: ReplaceTerm term NoChange
                            }
                    }
              ]
              varContextItem

renderPalette :: This -> RenderEnvironment -> Array ReactElement
renderPalette this env =
  [ DOM.div [ Props.className "palette-wrapper" ]
      [ DOM.div [ Props.className "palette" ]
          [ DOM.div [ Props.className "header" ] [ DOM.text "Palette" ]
          , DOM.div [ Props.className "actions-wrapper" ]
              [ DOM.div [ Props.className "actions" ]
                  $ Array.concat
                  $ renderAction
                  <$> env.actions
              ]
          ]
      ]
  ]
  where
  renderAction action =
    [ DOM.div [ Props.className "action-wrapper" ] <<< pure
        $ DOM.div
            ( [ Props.className "action"
              , Props.onClick \event -> doTransition { this, event: MouseTransitionEvent event } (unwrap action).transition
              ]
                <> maybeArray (unwrap action).tooltip Props.title
            )
        $ [ DOM.div [ Props.className "action-label" ] [ DOM.text (unwrap action).transition.label ]
          , DOM.div [ Props.className "action-triggers" ]
              $ ( \trigger ->
                    DOM.div [ Props.className "action-trigger" ] [ DOM.text (show trigger) ]
                )
              <$> (unwrap action).triggers
          ]
    ]

{-
  































































