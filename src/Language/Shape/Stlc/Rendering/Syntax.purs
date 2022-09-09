module Language.Shape.Stlc.Rendering.Syntax where

import Data.Tuple.Nested
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Metacontext
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Recursor.Index
import Language.Shape.Stlc.Rendering.ClickDragDrop
import Language.Shape.Stlc.Rendering.Token
import Language.Shape.Stlc.Rendering.Types
import Language.Shape.Stlc.Rendering.Utilities
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Types
import Prelude
import Prim hiding (Type)
import Control.Monad.State (get, runState)
import Control.Monad.State as State
import Data.Array (concat)
import Data.Array as Array
import Data.Default (default)
import Data.List.Unsafe (List)
import Data.Map.Unsafe as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.OrderedMap as OrderedMap
import Data.OrderedSet as OrderedSet
import Data.String (joinWith)
import Data.Traversable (sequence)
import Effect (Effect)
import Language.Shape.Stlc.Action (deselect, doAction, fillDatatype, fillVar)
import Language.Shape.Stlc.FuzzyFilter (fuzzyDistance)
import Language.Shape.Stlc.Recursor.Action as Rec
import Language.Shape.Stlc.Rendering.Highlight (propsHighlight)
import Partial.Unsafe (unsafeCrashWith)
import React (ReactElement, getState)
import React.DOM as DOM
import React.DOM.Props as Props
import Record as Record
import Undefined (undefined)

type RenderArgs
  = { this :: This, syntaxtheme :: SyntaxTheme }

renderProgram :: This -> Effect (Array ReactElement /\ RenderEnvironment)
renderProgram this = do
  st <- getState this
  mb_ix <- pure $ getStateIndex st
  let
    renEnv = emptyRenderEnvironment st

    syntaxtheme = renEnv.syntaxtheme
  -- Debug.traceM $ "===[ st.term ]==============================="
  -- Debug.traceM $ show st.term
  let
    (elems /\ env) =
      flip runState renEnv
        $ renderTerm { this, syntaxtheme } Nothing
            -- TODO: maybe pull this out into multiple files or at least somewhere else?
            { term: st.program.term
            , gamma: default
            , alpha: st.program.type_
            , visit: nilVisit mb_ix
            , meta: default
            }
  pure
    ( [ DOM.div
          [ Props.className "program"
          , Props.onClick \event -> do
              doAction
                { this, actionTrigger: MouseActionTrigger event, mb_queryResult: Nothing }
                deselect
          ]
          elems
      ]
        /\ env
    )

renderType :: RenderArgs -> Record (Rec.ArgsType ()) -> M (Array ReactElement)
renderType renArgs@{ this, syntaxtheme } =
  Rec.recType
    { arrowType:
        \args -> do
          dom <- renderType renArgs args.dom
          cod <- renderType renArgs args.cod
          dom_assoc <-
            pure case args.dom.type_ of
              ArrowType _ -> true
              _ -> false
          cod_arr <-
            pure case args.cod.type_ of
              ArrowType _ -> true
              _ -> false
          cod_assoc <-
            pure case args.cod.type_ of
              ArrowType _ -> true
              _ -> false
          renderNewNode renArgs
            ((makeNodeProps args) { label = Just "ArrowType", syntax = Just $ SyntaxType $ ArrowType args.arrowType })
            (syntaxtheme.type_.arr { dom, cod, dom_assoc, cod_arr, cod_assoc, meta: args.arrowType.meta, metactx: args.meta })
    , dataType:
        \args -> do
          typeId <- printTypeId args.typeId
          renderNewNode renArgs
            ((makeNodeProps args) { label = Just "DataType", syntax = Just $ SyntaxType $ DataType args.dataType })
            (syntaxtheme.type_.data_ { typeId, meta: args.dataType.meta, metactx: args.meta })
    , holeType:
        \args -> do
          State.modify_ (Record.modify _holeIds (OrderedSet.insert args.holeType.holeId))
          holeId <- printHoleId { holeId: args.holeType.holeId, meta: args.holeId.meta }
          (\elems -> [ DOM.span [ Props.className "typehole-container" ] elems ])
            <$> renderNewNode' renArgs
                ((makeNodeProps args) { label = Just "HoleType", syntax = Just $ SyntaxType $ HoleType args.holeType })
                [ renderTypeHoleQuery args.holeType renArgs args
                , pure $ syntaxtheme.type_.hole { holeId, weakening: Nothing, meta: args.holeType.meta, metactx: args.meta }
                ]
    }

addHoleIdsFromType :: Type -> M Unit
addHoleIdsFromType = case _ of
  ArrowType arrow -> do
    addHoleIdsFromType arrow.dom
    addHoleIdsFromType arrow.cod
  DataType data_ -> pure unit
  HoleType hole -> State.modify_ (Record.modify _holeIds (OrderedSet.insert hole.holeId))

renderTerm :: RenderArgs -> Maybe Syntax -> Record (Rec.ArgsTerm ()) -> M (Array ReactElement)
renderTerm renArgs@{ this, syntaxtheme } mb_syn_parent =
  Rec.recTerm
    { lam:
        \args -> do
          addHoleIdsFromType args.alpha
          termBind <- renderTermBind renArgs args.termBind
          body <- renderTerm renArgs (Just $ SyntaxTerm $ Lam $ args.lam) args.body
          parent_lam <-
            pure case mb_syn_parent of
              Just (SyntaxTerm (Lam _)) -> true
              _ -> false
          body_lam <-
            pure case args.lam.body of
              Lam _ -> true
              _ -> false
          body_assoc <-
            pure case args.lam.body of
              Lam _ -> true
              _ -> false
          renderNewNode renArgs
            ((makeNodeProps args) { label = Just "Lam", alpha = Just args.alpha, syntax = Just $ SyntaxTerm $ Lam args.lam })
            (syntaxtheme.term.lam { termBind, body, parent_lam, body_lam, body_assoc, meta: args.lam.meta, metactx: args.meta })
    , neu:
        \args -> do
          termId <- renderTermId renArgs args.termId
          argItems <- renderItems (renderArgItem renArgs <$> args.argItems)
          renderNewNode renArgs
            ((makeNodeProps args) { label = Just "Neu", alpha = Just args.alpha, syntax = Just $ SyntaxTerm $ Neu args.neu })
            (syntaxtheme.term.neu { termId, argItems, meta: args.neu.meta, metactx: args.meta })
    , let_:
        \args -> do
          termBind <- renderTermBind renArgs args.termBind
          sign <- renderType renArgs args.sign
          impl <- renderTerm renArgs (Just $ SyntaxTerm $ Let args.let_) args.impl
          body <- renderTerm renArgs (Just $ SyntaxTerm $ Let args.let_) args.body
          body_noindent <-
            pure case args.let_.body of
              Let _ -> true
              Buf _ -> true
              Data _ -> true
              _ -> false
          renderNewNode renArgs
            ((makeNodeProps args) { label = Just "Let", alpha = Just args.alpha, syntax = Just $ SyntaxTerm $ Let args.let_ })
            (syntaxtheme.term.let_ { termBind, sign, impl, body, meta: args.let_.meta, metactx: args.meta, body_noindent })
    , buf:
        \args -> do
          sign <- renderType renArgs args.sign
          impl <- renderTerm renArgs (Just $ SyntaxTerm $ Buf args.buf) args.impl
          body <- renderTerm renArgs (Just $ SyntaxTerm $ Buf args.buf) args.body
          body_noindent <-
            pure case args.buf.body of
              Let _ -> true
              Buf _ -> true
              Data _ -> true
              _ -> false
          renderNewNode renArgs
            ((makeNodeProps args) { label = Just "Buf", alpha = Just args.alpha, syntax = Just $ SyntaxTerm $ Buf args.buf })
            (syntaxtheme.term.buf { sign, impl, body, meta: args.buf.meta, metactx: args.meta, body_noindent })
    , data_:
        \args -> do
          typeBind <- renderTypeBind renArgs args.typeBind
          sumItems <- renderItems (renderSumItem renArgs <$> args.sumItems)
          body <- renderTerm renArgs (Just $ SyntaxTerm $ Data args.data_) args.body
          body_noindent <-
            pure case args.data_.body of
              Let _ -> true
              Buf _ -> true
              Data _ -> true
              _ -> false
          renderNewNode renArgs
            ((makeNodeProps args) { label = Just "Data", alpha = Just args.alpha, syntax = Just $ SyntaxTerm $ Data args.data_ })
            (syntaxtheme.term.data_ { typeBind, sumItems, body, meta: args.data_.meta, metactx: args.meta, body_noindent })
    , match:
        \args -> do
          term <- renderTerm renArgs (Just $ SyntaxTerm $ Match args.match) args.term
          caseItems <- renderItems (renderCaseItem renArgs <$> args.caseItems)
          renderNewNode renArgs
            ((makeNodeProps args) { label = Just "Match", alpha = Just args.alpha, syntax = Just $ SyntaxTerm $ Match args.match })
            (syntaxtheme.term.match { term, caseItems, meta: args.match.meta, metactx: args.meta })
    , hole:
        \args ->
          (\elems -> [ DOM.span [ Props.className "hole-container" ] elems ])
            <$> renderNewNode' renArgs
                ((makeNodeProps args) { label = Just "Hole", alpha = Just args.alpha, syntax = Just $ SyntaxTerm $ Hole args.hole })
                [ renderTermHoleQuery renArgs args
                , renderType renArgs { type_: args.alpha, gamma: args.gamma, visit: nonVisit, meta: args.meta }
                ]
    }

renderArgItem :: RenderArgs -> Record (Rec.ArgsArgItem ()) -> M (Array ReactElement)
renderArgItem renArgs@{ this, syntaxtheme } =
  Rec.recArgItem
    { argItem:
        \args -> do
          term <- renderTerm { this, syntaxtheme } (Just $ SyntaxArgItem args.argItem) args.term
          let
            term_assoc = requiresParenTerm args.term.term
          renderNewNode renArgs
            ((makeNodeProps args) { label = Just "ArgItem", visit = nonVisit })
            (syntaxtheme.argItem { term, term_assoc, meta: args.argItem.meta, metactx: args.meta })
    }

renderSumItem :: RenderArgs -> Record (Rec.ArgsSumItem ()) -> M (Array ReactElement)
renderSumItem renArgs@{ this, syntaxtheme } =
  Rec.recSumItem
    { sumItem:
        \args -> do
          termBind <- renderTermBind renArgs args.termBind
          paramItems <- renderItems (renderParamItem renArgs <$> args.paramItems)
          renderNewNode renArgs
            ((makeNodeProps args) { label = Just "SumItem" })
            (syntaxtheme.sumItem { termBind, paramItems, meta: args.sumItem.meta, metactx: args.meta })
    }

renderCaseItem :: RenderArgs -> Record (Rec.ArgsCaseItem ()) -> M (Array ReactElement)
renderCaseItem renArgs@{ this, syntaxtheme } =
  Rec.recCaseItem
    { caseItem:
        \args -> do
          termId <- printTermId { termId: args.termId, gamma: args.gamma, visit: nonVisit, meta: args.meta }
          termBindItems <- renderItems (renderTermBindItem renArgs <$> args.termBindItems)
          body <- renderTerm renArgs (Just $ SyntaxCaseItem args.caseItem) args.body
          renderNewNode renArgs
            (makeNodeProps args) { label = Just "CaseItem" }
            (syntaxtheme.caseItem { termId, termBindItems, body, meta: args.caseItem.meta, metactx: args.meta })
    }

renderParamItem :: RenderArgs -> Record (Rec.ArgsParamItem ()) -> M (Array ReactElement)
renderParamItem renArgs@{ this, syntaxtheme } =
  Rec.recParamItem
    { paramItem:
        \args -> do
          type_ <- renderType renArgs args.type_
          renderNewNode renArgs
            (makeNodeProps args) { label = Just "ParamItem" }
            (syntaxtheme.paramItem { type_, meta: args.paramItem.meta, metactx: args.meta })
    }

renderTermBindItem :: RenderArgs -> Record (Rec.ArgsTermBindItem ()) -> M (Array ReactElement)
renderTermBindItem renArgs@{ this, syntaxtheme } =
  Rec.recTermBindItem
    { termBindItem:
        \args -> do
          termBind <- renderTermBind renArgs args.termBind
          renderNewNode renArgs
            (makeNodeProps args) { label = Just "TermBindItem" }
            (syntaxtheme.termBindItem { termBind, meta: args.termBindItem.meta, metactx: args.meta })
    }

renderTermBind :: RenderArgs -> Record (Rec.ArgsTermBind ()) -> M (Array ReactElement)
renderTermBind renArgs@{ this, syntaxtheme } =
  Rec.recTermBind
    { termBind:
        \args -> do
          termId <- printTermId args.termId
          renderNewNode renArgs
            ( (makeNodeProps args)
                { label = Just "TermBind" }
            )
            termId
    }

renderTypeBind :: RenderArgs -> Record (Rec.ArgsTypeBind ()) -> M (Array ReactElement)
renderTypeBind renArgs@{ this, syntaxtheme } =
  Rec.recTypeBind
    { typeBind:
        \args -> do
          typeId <- printTypeId args.typeId
          renderNewNode renArgs
            (makeNodeProps args)
              { label = Just "TypeBind" }
            typeId
    }

renderTypeId :: RenderArgs -> Record (Rec.ArgsTypeId ()) -> M (Array ReactElement)
renderTypeId renArgs@{ this, syntaxtheme } argsTypeId =
  Rec.recTypeId
    { typeId:
        \args -> do
          typeId <- printTypeId argsTypeId
          renderNewNode renArgs
            (makeNodeProps args) { label = Just "TypeId" }
            typeId
    }
    argsTypeId

renderTermId :: RenderArgs -> Record (Rec.ArgsTermId ()) -> M (Array ReactElement)
renderTermId renArgs@{ this, syntaxtheme } argsTermId =
  Rec.recTermId
    { termId:
        \args -> do
          termId <- printTermId argsTermId
          renderNewNode renArgs
            (makeNodeProps args) { label = Just "TermId" }
            termId
    }
    argsTermId

printTypeId :: Record (Rec.ArgsTypeId ()) -> M (Array ReactElement)
printTypeId renArgs@{ typeId, meta } =
  pure
    [ DOM.span [ Props.className "typeId" ]
        $ printName name
        <> printShadow shadow
    ]
  where
  -- name = case Map.lookup typeId (unwrap meta).dataNames of
  --   Just name -> name
  --   Nothing -> unsafeCrashWith $ "could not find name of type id " <> show typeId <> " in metacontext " <> show meta
  name = lookupDataName typeId meta

  shadow = case Map.lookup typeId (unwrap meta).dataShadowIndices of
    Just i -> i
    Nothing -> unsafeCrashWith $ "could not find shadow of data name " <> show name <> " in metacontext " <> show meta

printTermId :: Record (Rec.ArgsTermId ()) -> M (Array ReactElement)
printTermId { termId, meta } =
  pure
    [ DOM.span [ Props.className "termId" ]
        $ printName name
        <> printShadow shadow
    ]
  where
  -- name = case Map.lookup termId (unwrap meta).varNames of
  --   Just name -> name
  --   Nothing -> unsafeCrashWith $ "could not find name of term id " <> show termId <> " in metacontext " <> show meta
  name = lookupVarName termId meta

  shadow = case Map.lookup termId (unwrap meta).varShadowIndices of
    Just i -> i
    Nothing -> unsafeCrashWith $ "could not find shadow of var name " <> show name <> " in metacontext " <> show meta

printHoleId :: { holeId :: HoleId, meta :: Metacontext } -> M (Array ReactElement)
printHoleId args = do
  mb_i <- OrderedSet.findIndexRev (args.holeId == _) <$> State.gets _.holeIds
  case mb_i of
    Just i -> pure [ DOM.span [ Props.className "holeId" ] [ DOM.text $ "?" <> show i ] ]
    Nothing -> unsafeCrashWith $ "count not find index of holeId " <> show args.holeId <> " in metacontext " <> show args.meta

printName :: Name -> Array ReactElement
printName (Name mb_str) = case mb_str of
  Just str -> [ DOM.span [ Props.className "name" ] [ DOM.text str ] ]
  Nothing -> [ DOM.span [ Props.className "name discarded" ] [ DOM.text "~" ] ]

printShadow :: Int -> Array ReactElement
printShadow shadow =
  if 0 < shadow then
    [ DOM.span [ Props.className "shadow" ] [ DOM.text (show shadow) ] ]
  else
    []

renderItems :: List (M (Array ReactElement)) -> M (Array (Array ReactElement))
renderItems items = Array.fromFoldable <$> sequence items

renderNewNode :: RenderArgs -> NodeProps -> Array ReactElement -> M (Array ReactElement)
renderNewNode args props res = renderNewNode' args props [ pure res ]

renderNewNode' :: RenderArgs -> NodeProps -> Array (M (Array ReactElement)) -> M (Array ReactElement)
renderNewNode' { this, syntaxtheme } props mres = do
  -- if this node is selected
  when isSelected do
    -- update environment
    State.modify_
      ( _
          { syntax = props.syntax
          , alpha = props.alpha
          , gamma = props.gamma
          , actions = props.actions
          , meta = props.meta
          }
      )
  -- render children
  res <- Array.concat <$> sequence mres
  pure $ [ DOM.span propsSpan res ]
  where
  isSelected = props.visit.csr == Just nilIxDown

  mb_elemId = hashIxUp <$> props.visit.ix

  propsSpan =
    concat
      -- className
      [ maybeArray props.label \label ->
          Props.className $ joinWith " " [ "node", label, if isSelected then "selected" else "" ]
      , maybeArray mb_elemId \elemId ->
          Props._id elemId
      -- click, drag, drop
      , propsClickDragDrop this props
      -- highlight
      , concat
          $ maybeArray mb_elemId \elemId ->
              propsHighlight this props elemId
      ]

renderTermHoleQuery :: RenderArgs -> _ -> M (Array ReactElement)
renderTermHoleQuery renArgs args = do
  state <- State.get
  case state.st.mode of
    SelectMode selMode
      | args.visit.csr == Just nilIxDown
      , Just query <- selMode.mb_query -> do
        let
          -- filter the context for variables that have a
          -- name fuzzy-matching the query string
          queryResults :: Array (TermId /\ Type)
          queryResults =
            map (\(termId /\ type_ /\ _) -> (termId /\ type_))
              $ Array.sortWith (\(_ /\ _ /\ dist) -> dist)
              $ Array.foldMap
                  ( \(termId /\ type_) ->
                      let
                        Name mb_str = lookupVarName termId args.meta
                      in
                        case fuzzyDistance query.string =<< mb_str of
                          Just dist -> [ termId /\ type_ /\ dist ]
                          Nothing -> []
                  )
              $ OrderedMap.toArray (unwrap args.gamma).varTypes
        -- recurds the current query result
        State.modify_ \env ->
          env
            { mb_queryResult =
              do
                (termId /\ type_) <- Array.index queryResults query.i
                Just
                  { action: fillVar { env, termId, type_ }
                  , n: Array.length queryResults
                  }
            }
        -- render each items from the query result
        variableQueryElems <-
          sequence
            $ Array.mapWithIndex
                ( \i (termId /\ type_) -> do
                    elemTermId <- printTermId { termId, gamma: args.gamma, meta: args.meta, visit: nonVisit }
                    elemType <- renderType renArgs { type_, gamma: args.gamma, meta: args.meta, visit: nonVisit }
                    pure
                      $ ( DOM.span [ Props.className $ "query-context-item" <> if query.i == i then " selected" else "" ]
                            $ concat
                                [ elemTermId
                                , [ DOM.text " : " ]
                                , elemType
                                ]
                        )
                )
            $ queryResults
        pure
          $ [ DOM.div [ Props.className "query" ]
                [ DOM.div [ Props.className "query-context" ]
                    ( if Array.length variableQueryElems > 0 then
                        variableQueryElems
                      else
                        [ DOM.span [ Props.className "query-no-matches" ]
                            [ DOM.text "no matches" ]
                        ]
                    )
                , DOM.span [ Props.className "query-text" ]
                    [ DOM.text query.string ]
                ]
            , DOM.span [ Props.className "query-sep" ]
                [ DOM.text " .. : " ]
            ]
    _ -> pure []

-- mb_actions <- case state.mode of
--   SelectMode selMode
--     | Just query <- selMode.mb_query -> do
--       -- gather all query results, filtering out those with insufficient fuzzy
--       -- proximity
--       measuredActions <-
--         pure
--           $ Array.concat
--               [ Array.concatMap
--                   ( \(Action action) -> case fuzzyDistance query.string action.label of
--                       Just dist -> [ dist /\ Action action ]
--                       Nothing -> []
--                   )
--                   props.actions
--               , case props.syntax of
--                   Nothing -> []
--                   -- if at a term, add the term vars in context
--                   Just (SyntaxTerm _) ->
--                     Array.foldMap
--                       ( \(termId /\ type_) ->
--                           let
--                             name@(Name mb_str) = lookupVarName termId props.meta
--                           in
--                             case fuzzyDistance query.string =<< mb_str of
--                               Just dist -> [ dist /\ fillVar { name, termId, type_ } ]
--                               Nothing -> []
--                       )
--                       $ OrderedMap.toArray (unwrap props.gamma).varTypes
--                   -- if at a type, add the data types in context
--                   Just (SyntaxType _) ->
--                     Array.foldMap
--                       ( \(typeId /\ _data_) ->
--                           let
--                             name@(Name mb_str) = lookupDataName typeId props.meta
--                           in
--                             case fuzzyDistance query.string =<< mb_str of
--                               Just dist -> [ dist /\ fillDatatype { name, typeId } ]
--                               Nothing -> []
--                       )
--                       $ OrderedMap.toArray (unwrap props.gamma).datas
--                   _ -> []
--               ]
--       -- sort by fuzzy proximity
--       measuredActions <-
--         pure $ Array.sortWith (\(dist /\ _) -> dist) measuredActions
--       -- extract query results
--       pure $ Just $ map (\(_ /\ qr) -> qr) measuredActions
--   _ -> pure Nothing
renderTypeHoleQuery :: HoleType -> RenderArgs -> _ -> M (Array ReactElement)
renderTypeHoleQuery holeType renArgs args = do
  state <- State.get
  case state.st.mode of
    SelectMode selMode
      | args.visit.csr == Just nilIxDown
      , Just query <- selMode.mb_query -> do
        let
          -- filter the context for variables that have a
          -- name fuzzy-matching the query string
          queryResults :: Array (TypeId /\ Data)
          queryResults =
            map (\(typeId /\ data_ /\ _) -> (typeId /\ data_))
              $ Array.sortWith (\(_ /\ _ /\ dist) -> dist)
              $ Array.foldMap
                  ( \(typeId /\ data_) ->
                      let
                        Name mb_str = lookupDataName typeId args.meta
                      in
                        case fuzzyDistance query.string =<< mb_str of
                          Just dist -> [ typeId /\ data_ /\ dist ]
                          Nothing -> []
                  )
              $ OrderedMap.toArray (unwrap args.gamma).datas
        -- recurds the current query result
        State.modify_ \env ->
          env
            { mb_queryResult =
              do
                (typeId /\ data_) <- Array.index queryResults query.i
                Just
                  { action: fillDatatype { holeType, typeId }
                  , n: Array.length queryResults
                  }
            }
        -- render each items from the query result
        variableQueryElems <-
          sequence
            $ Array.mapWithIndex
                ( \i (typeId /\ data_) -> do
                    elemTypeId <- printTypeId { typeId, gamma: args.gamma, meta: args.meta, visit: nonVisit }
                    pure
                      $ ( DOM.span [ Props.className $ "query-context-item" <> if query.i == i then " selected" else "" ]
                            $ elemTypeId
                        )
                )
            $ queryResults
        pure
          $ [ DOM.div [ Props.className "query" ]
                [ DOM.div [ Props.className "query-context" ]
                    ( if Array.length variableQueryElems > 0 then
                        variableQueryElems
                      else
                        [ DOM.span [ Props.className "query-no-matches" ]
                            [ DOM.text "no matches" ]
                        ]
                    )
                , DOM.span [ Props.className "query-text" ]
                    [ DOM.text query.string ]
                , DOM.span [ Props.className "query-sep" ]
                    [ DOM.text " = " ]
                ]
            ]
    _ -> pure []
