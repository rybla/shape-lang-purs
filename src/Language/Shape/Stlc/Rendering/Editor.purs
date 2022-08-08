module Language.Shape.Stlc.Rendering.Editor where

import Data.Tuple.Nested
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
import KeyboardCursor (stepCursorBackwards, stepCursorForwards)
import Language.Shape.Stlc.Index (nilIxDown)
import Partial.Unsafe (unsafeCrashWith)
import React (ReactElement, getState, modifyState)
import React.DOM as DOM
import React.DOM.Props as Props
import Record as Record
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe (fromJust)

-- | renderEditor
renderEditor :: This -> Effect (RenderEnvironment /\ ReactElement)
renderEditor this = do
  elems /\ env <- renderProgram this
  -- global actions 
  env <- pure $ env { actions = env.actions <> globalActions }
  pure $ env
    /\ ( DOM.div [ Props.className "editor" ]
          $ renderPanel this env
          <> elems
      )
  where
  globalActions =
    [ Action
        { label: Just "undo"
        , tooltip: Just "undo the previous change"
        , triggers: [ ActionTrigger_Keypress keys.undo ]
        , effect:
            \{ this } -> do
              modifyState this \st -> case Array.uncons st.history of
                Just { head: { term, type_, mb_ix, change }, tail: history } ->
                  st
                    { term = term
                    , type_ = type_
                    , mb_ix = mb_ix
                    , history = history
                    , clipboard = Nothing
                    , dragboard = Nothing
                    }
                Nothing -> st -- cannot undo if there is no history
        }
    , Action
        { label: Just "stepCursorForwards"
        , tooltip: Just "move the cursor fowards in a tree walk"
        , triggers: [ ActionTrigger_Keypress keys.cursorForwards ]
        , effect:
            \{ this } ->
              modifyState this \st -> case st.mb_ix of
                Just ix ->
                  maybe st identity do
                    ix' <- stepCursorForwards (SyntaxTerm st.term) ix
                    pure st { mb_ix = Just ix' }
                Nothing -> st { mb_ix = Just $ nilIxDown }
        }
    , Action
        { label: Just "stepCursorBackwards"
        , tooltip: Just "move the cursor backwards in a tree walk"
        , triggers: [ ActionTrigger_Keypress keys.cursorBackwards ]
        , effect:
            \{ this } -> do
              modifyState this \st -> case st.mb_ix of
                Just ix ->
                  maybe st identity do
                    ix' <- stepCursorBackwards (SyntaxTerm st.term) ix
                    pure st { mb_ix = Just ix' }
                Nothing -> st { mb_ix = Just $ nilIxDown }
        }
    -- TODO: this isn't actually treated as an explicit action, since it's handled by handleKey_QueryMode
    -- but probably would be better to have "editing" modes handled more generally
    -- for when you're doing lots of typing
    {-
    , Action
        { label: Just "normalMode"
        , triggers: [ ActionTrigger_Keypress keys.normalMode ]
        , effect: \{ this } -> modifyState this (_ { mode = NormalMode })
        }
    -}
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
                          $ renderType this { type_: alpha, gamma: env.gamma, visit: nonVisit, meta: env.meta }
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
    renderData typeId =
      let
        data_ = OrderedMap.lookup'' "renderData.data_" typeId (unwrap gamma).datas
      in
        DOM.span [ Props.className "context-data-wrapper" ] <<< pure
          $ DOM.span [ Props.className "context-data context-item" ]
          $ [ DOM.span
                [ Props.onClick \event -> do
                    -- Debug.traceM "trying to paste datatype"
                    -- Debug.traceM $ "env.syntax = " <> show env.syntax
                    case env.syntax of
                      Just (SyntaxType (HoleType holeType)) -> do
                        modifyState this \st ->
                          maybe st identity do
                            -- applyChange
                            -- { ix: fromJust st.mb_ix
                            -- , toReplace: ReplaceType (DataType { typeId, meta: default }) NoChange
                            -- }
                            -- st
                            -- apply holeSub
                            let
                              holeSub = Map.singleton holeType.holeId (DataType { typeId, meta: default })
                            pure
                              $ st
                                  { term = subTerm holeSub st.term
                                  , type_ = subType holeSub st.type_
                                  }
                      Just (SyntaxTerm (Hole hole)) -> do
                        -- match on term of type clicked
                        st <- getState this
                        doChange this
                          { ix: fromJust st.mb_ix
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
                      _ -> pure unit
                ]
                $ [ token.data1 ]
                <> ( flip State.evalState env
                      $ renderTypeBind this
                          { typeBind: data_.typeBind
                          , gamma: gamma
                          , visit: nonVisit
                          , meta: env.meta
                          }
                  )
            ]

    renderVarType termId =
      let
        type_ = OrderedMap.lookup'' "renderVarType.type_" termId (unwrap gamma).varTypes
      in
        DOM.span [ Props.className "context-varType-wrapper" ] <<< pure
          $ DOM.span
              [ Props.className "context-varType context-item"
              , Props.onClick \event -> do
                  case fitsInHole type_ (fromJust env.alpha) of
                    -- does fit in hole 
                    Just (nArgs /\ holeSub) -> do
                      let
                        term = createNeu termId nArgs
                      modifyState this \st ->
                        maybe st identity do
                          st <-
                            applyChange
                              { ix: fromJust st.mb_ix
                              , toReplace: ReplaceTerm term NoChange
                              }
                              st
                          st <-
                            pure
                              $ st
                                  { term = subTerm holeSub st.term
                                  , type_ = subType holeSub st.type_
                                  }
                          pure st
                    Nothing -> pure unit -- doesn't fit in hole 
              ]
          $ Array.concat
              [ [ DOM.span
                    [ Props.className "context-varType-var" ]
                    (flip State.evalState env $ renderTermId this { termId: termId, gamma: gamma, visit: nonVisit, meta: env.meta })
                ]
              , [ token.let2 ]
              , [ DOM.span [ Props.className "context-varType-type" ]
                    (flip State.evalState env $ renderType this { type_: type_, gamma: gamma, visit: nonVisit, meta: env.meta })
                ]
              ]

renderPalette :: This -> RenderEnvironment -> Array ReactElement
renderPalette this env =
  [ DOM.div [ Props.className "palette" ]
      [ DOM.div [ Props.className "header" ] [ DOM.text "Palette" ]
      , DOM.div [ Props.className "actions" ] $ Array.concat $ renderAction <$> env.actions
      ]
  ]
  where
  renderAction action = case (unwrap action).label of
    Just label ->
      [ DOM.div [ Props.className "action-wrapper" ] <<< pure
          $ DOM.div
              [ Props.className "action"
              , Props.onClick \event -> (unwrap action).effect { this, mb_event: Nothing, trigger: ActionTrigger_Click }
              ]
          $ ( [ DOM.div [ Props.className "action-label" ] [ DOM.text label ]
              , DOM.div [ Props.className "action-triggers" ]
                  $ ( \trigger ->
                        DOM.div [ Props.className "action-trigger" ] [ DOM.text (show trigger) ]
                    )
                  <$> (unwrap action).triggers
              -- , DOM.div [ Props.className "action-tooltip" ]
              --   [ DOM.text  ]
              ]
                <> maybe []
                    (\str -> [ DOM.div [ Props.className "action-tooltip" ] [ DOM.text str ] ])
                    (unwrap action).tooltip
            )
      ]
    Nothing -> []
