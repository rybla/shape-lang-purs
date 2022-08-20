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
import Control.Monad.Error.Class (throwError)
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
import Partial.Unsafe (unsafeCrashWith)
import React (ReactElement, getState, modifyState)
import React.DOM as DOM
import React.DOM.Props as Props
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe (error, fromJust)

-- | renderEditor
renderEditor :: This -> Effect (RenderEnvironment /\ Array (ReactElement))
renderEditor this = do
  res /\ env <- renderProgram this
  -- global actions 
  env <- pure $ env { actions = env.actions <> globalActions }
  pure $ env
    /\ [ DOM.div [ Props.className "editor" ]
          $ Array.concat
              [ res
              , renderPanel this env
              ]
      ]
  where
  globalActions =
    [ Action
        { tooltip: Nothing
        , triggers: [ ActionTrigger_Keypress keys.undo ]
        , transition:
            { label: "undo"
            , effect:
                \{ state } -> case Array.uncons state.history of
                  Just { head: { program, change }, tail: history } ->
                    pure
                      state
                        { mode = error "TODO" -- SelectMode change.ix 
                        , program = program
                        , history = history
                        , clipboard = Nothing
                        }
                  Nothing -> throwError "cannot undo at beginning of history"
            }
        }
    , Action
        { tooltip: Just "move the cursor fowards in a tree walk"
        , triggers: [ ActionTrigger_Keypress keys.cursorForwards ]
        , transition: error "TODO"
        -- , effect:
        --     \{ this } ->
        --       modifyState this \st -> case st.mb_ix of
        --         Just ix ->
        --           maybe st identity do
        --             ix' <- stepCursorForwards (SyntaxTerm st.term) ix
        --             pure st { mb_ix = Just ix' }
        --         Nothing -> st { mb_ix = Just $ nilIxDown }
        }
    , Action
        { tooltip: Just "move the cursor backwards in a tree walk"
        , triggers: [ ActionTrigger_Keypress keys.cursorBackwards ]
        , transition: error "TODO"
        -- , effect:
        --     \{ this } -> do
        --       modifyState this \st -> case st.mb_ix of
        --         Just ix ->
        --           maybe st identity do
        --             ix' <- stepCursorBackwards (SyntaxTerm st.term) ix
        --             pure st { mb_ix = Just ix' }
        --         -- selects the rightmost element of the term
        --         Nothing -> st { mb_ix = Just $ wrap (getLastIndex (SyntaxTerm st.term)) }
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
              [ Props.onClick \event -> do
                  -- Debug.traceM "trying to paste datatype"
                  -- Debug.traceM $ "env.syntax = " <> show env.syntax
                  -- case env.syntax of
                  --   Just (SyntaxType (HoleType holeType)) -> do
                  --     modifyState this \st ->
                  --       maybe st identity do
                  --         -- applyChange
                  --         -- { ix: fromJust st.mb_ix
                  --         -- , toReplace: ReplaceType (DataType { typeId, meta: default }) NoChange
                  --         -- }
                  --         -- st
                  --         -- apply holeSub
                  --         let
                  --           holeSub = Map.singleton holeType.holeId (DataType { typeId, meta: default })
                  --         pure
                  --           $ st
                  --               { term = subTerm holeSub st.term
                  --               , type_ = subType holeSub st.type_
                  --               }
                  --   Just (SyntaxTerm (Hole hole)) -> do
                  --     -- match on term of type clicked
                  --     st <- getState this
                  --     doChange this
                  --       { ix: fromJust st.mb_ix
                  --       , toReplace:
                  --           ReplaceTerm
                  --             ( Match
                  --                 { typeId: typeId
                  --                 , term: Hole { meta: default }
                  --                 , caseItems:
                  --                     ( \sumItem ->
                  --                         { termBindItems: (\_ -> { termBind: freshTermBind unit, meta: default }) <$> sumItem.paramItems
                  --                         , body: Hole { meta: default }
                  --                         , meta: default
                  --                         }
                  --                     )
                  --                       <$> data_.sumItems
                  --                 , meta: default
                  --                 }
                  --             )
                  --             NoChange
                  --       }
                  --   _ -> pure unit
                  error "TODO"
              ]
              dataContextItem
          ]

    renderVarType termId =
      let
        type_ = OrderedMap.lookup'' "renderVarType.type_" termId (unwrap gamma).varTypes

        varContextItem =
          flip State.evalState env do
            termId <- do
              termId <- renderTermId { this, syntaxtheme: env.syntaxtheme } { termId: termId, gamma: gamma, visit: nonVisit, meta: env.meta }
              pure $ [ DOM.span [ Props.className "context-varType-var" ] termId ]
            type_ <- do
              type_ <- renderType { this, syntaxtheme: env.syntaxtheme } { type_: type_, gamma: gamma, visit: nonVisit, meta: env.meta }
              pure $ [ DOM.span [ Props.className "context-varType-type" ] type_ ]
            pure $ env.syntaxtheme.varContextItem { termId, type_, metactx: env.meta }
      in
        DOM.span [ Props.className "context-varType-wrapper" ] <<< pure
          $ DOM.span
              [ Props.className "context-varType context-item"
              , Props.onClick \event -> do
                  -- case fitsInHole type_ (fromJust env.alpha) of
                  --   -- does fit in hole 
                  --   Just (nArgs /\ holeSub) -> do
                  --     let
                  --       term = createNeu termId nArgs
                  --     modifyState this \st ->
                  --       maybe st identity do
                  --         st <-
                  --           applyChange
                  --             { ix: fromJust st.mb_ix
                  --             , toReplace: ReplaceTerm term NoChange
                  --             }
                  --             st
                  --         st <-
                  --           pure
                  --             $ st
                  --                 { term = subTerm holeSub st.term
                  --                 , type_ = subType holeSub st.type_
                  --                 }
                  --         pure st
                  --   Nothing -> pure unit -- doesn't fit in hole 
                  error "TODO"
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
              , Props.onClick \event -> error "TODO" --  (unwrap action).effect { this, mb_event: Nothing, trigger: ActionTrigger_Click }
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
  -- OLD: with tooltip divs
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
              ]
                <> maybe []
                    (\res -> [ DOM.div [ Props.className "action-tooltip" ] res ])
                    (unwrap action).tooltip
            )
      ]
    Nothing -> []
  -}
