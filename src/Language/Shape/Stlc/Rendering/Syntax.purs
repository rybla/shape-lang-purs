module Language.Shape.Stlc.Rendering.Syntax where

import Data.Tuple
import Data.Tuple.Nested
import Language.Shape.Stlc.ChAtIndex
import Language.Shape.Stlc.Changes
import Language.Shape.Stlc.Context
import Language.Shape.Stlc.Hole
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
import Type.Proxy
import Unsafe
import Control.Monad.State (State, gets, runState)
import Control.Monad.State as State
import Data.Array (concat, (:))
import Data.Array as Array
import Data.Default (default)
import Data.Foldable (foldM)
import Data.List.Unsafe (List(..), reverse)
import Data.List.Unsafe as List
import Data.Map.Unsafe as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.OrderedMap as OrderedMap
import Data.OrderedSet (OrderedSet)
import Data.OrderedSet as OrderedSet
import Data.Set as Set
import Data.String (joinWith)
import Data.Traversable (sequence)
import Debug as Debug
import Effect (Effect)
import Effect.Console as Console
import Language.Shape.Stlc.CopyPasteBackend (changesBetweenContexts, fitsInHole)
import Language.Shape.Stlc.FuzzyFilter (fuzzyDistance)
import Language.Shape.Stlc.Recursor.Action as Rec
import Language.Shape.Stlc.Recursor.Context as RecCtx
import Language.Shape.Stlc.Recursor.Index as RecIx
import Language.Shape.Stlc.Recursor.Metacontext as RecMeta
import Language.Shape.Stlc.Rendering.Highlight (propsHighlight)
import Language.Shape.Stlc.Transition (deselect, doTransition)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Union)
import React (ReactElement, getState, modifyState)
import React.DOM as DOM
import React.DOM.Props as Props
import React.SyntheticEvent (shiftKey, stopPropagation)
import Record as Record

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
              doTransition { this, event: MouseTransitionEvent event }
                { label: "deselect" -- i.e. go to TopMode
                , effect: deselect
                }
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
          renderNewNode renArgs
            ((makeNodeProps args) { label = Just "HoleType", syntax = Just $ SyntaxType $ HoleType args.holeType })
            (syntaxtheme.type_.hole { holeId, weakening: Nothing, meta: args.holeType.meta, metactx: args.meta })
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
        \args -> do
          -- TODO: actually, this happens at each node, not just at a hole 
          -- query <-
          --   State.gets (_.st.mode)
          --     >>= case _ of
          --         QueryMode q
          --           | args.visit.csr == Just nilIxDown -> do
          --             let
          --               -- filter the context for variables that have a
          --               -- name fuzzy-matching the query string
          --               queryResults :: Array (TermId /\ Type)
          --               queryResults =
          --                 map (\(termId /\ type_ /\ _) -> (termId /\ type_))
          --                   $ Array.sortWith (\(_ /\ _ /\ dist) -> dist)
          --                   $ Array.foldMap
          --                       ( \(termId /\ type_) ->
          --                           let
          --                             Name mb_str = lookupVarName termId args.meta
          --                           in
          --                             case fuzzyDistance q.query =<< mb_str of
          --                               Just dist -> [ termId /\ type_ /\ dist ]
          --                               Nothing -> []
          --                       )
          --                   $ OrderedMap.toArray (unwrap args.gamma).varTypes
          --             -- update the render environment to know about the
          --             -- current variable query result
          --             -- State.modify_ _ { variableQueryResult = Just queryResults }
          --             State.modify_ \env -> env { queryResults = env.queryResults <> var }
          --             -- render each items from the query result
          --             variableQueryElems <-
          --               sequence
          --                 $ Array.mapWithIndex
          --                     ( \i (termId /\ type_) -> do
          --                         elemTermId <- printTermId { termId, gamma: args.gamma, meta: args.meta, visit: nonVisit }
          --                         elemType <- renderType renArgs { type_, gamma: args.gamma, meta: args.meta, visit: nonVisit }
          --                         pure
          --                           $ ( DOM.span [ Props.className $ "query-context-item" <> if q.i == i then " selected" else "" ]
          --                                 $ concat
          --                                     [ elemTermId
          --                                     , [ DOM.text " : " ]
          --                                     , elemType
          --                                     ]
          --                             )
          --                     )
          --                 $ queryResults
          --             pure
          --               $ [ DOM.div [ Props.className "query" ]
          --                     [ DOM.div [ Props.className "query-context" ]
          --                         ( if Array.length variableQueryElems > 0 then
          --                             variableQueryElems
          --                           else
          --                             [ DOM.span [ Props.className "query-no-matches" ]
          --                                 [ DOM.text "no matches" ]
          --                             ]
          --                         )
          --                     , DOM.span [ Props.className "query-text" ]
          --                         [ DOM.text q.query ]
          --                     ]
          --                 , DOM.span [ Props.className "query-sep" ]
          --                     [ DOM.text " .. : " ]
          --                 ]
          --         _ -> pure []
          hole <- renderType renArgs { type_: args.alpha, gamma: args.gamma, visit: nonVisit, meta: args.meta }
          (\elems -> [ DOM.span [ Props.className "hole-container" ] elems ])
            <$> renderNewNode renArgs
                ((makeNodeProps args) { label = Just "Hole", alpha = Just args.alpha, syntax = Just $ SyntaxTerm $ Hole args.hole })
                -- (query <> hole)
                hole
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
renderNewNode { this, syntaxtheme } props res = do
  -- if this node is selected
  when isSelected do
    state <- State.gets _.st
    -- compute queryResults
    mb_queryResults <- case state.mode of
      QueryMode queryMode -> do
        -- gather all query results, filtering out those with insufficient fuzzy
        -- proximity
        measuredQueryResults <-
          pure
            $ Array.concat
                [ Array.concatMap
                    ( \(Action action) -> case fuzzyDistance queryMode.query action.transition.label of
                        Just dist -> [ dist /\ ActionQueryResult (Action action) ]
                        Nothing -> []
                    )
                    props.actions
                , case props.syntax of
                    Nothing -> []
                    -- if at a term, add the term vars in context
                    Just (SyntaxTerm _) ->
                      Array.foldMap
                        ( \(termId /\ type_) ->
                            let
                              name@(Name mb_str) = lookupVarName termId props.meta
                            in
                              case fuzzyDistance queryMode.query =<< mb_str of
                                Just dist -> [ dist /\ TermVariableQueryResult { name, termId, type_ } ]
                                Nothing -> []
                        )
                        $ OrderedMap.toArray (unwrap props.gamma).varTypes
                    -- if at a type, add the data types in context
                    Just (SyntaxType _) ->
                      Array.foldMap
                        ( \(typeId /\ _data_) ->
                            let
                              name@(Name mb_str) = lookupDataName typeId props.meta
                            in
                              case fuzzyDistance queryMode.query =<< mb_str of
                                Just dist -> [ dist /\ DataTypeQueryResult { name, typeId } ]
                                Nothing -> []
                        )
                        $ OrderedMap.toArray (unwrap props.gamma).datas
                    _ -> []
                ]
        -- sort by fuzzy proximity
        measuredQueryResults <-
          pure $ Array.sortWith (\(dist /\ _) -> dist) measuredQueryResults
        -- extract query results
        pure $ Just $ map (\(_ /\ qr) -> qr) measuredQueryResults
      _ -> pure Nothing
    -- update environment
    State.modify_
      ( _
          { syntax = props.syntax
          , alpha = props.alpha
          , gamma = props.gamma
          , actions = props.actions
          , meta = props.meta
          , mb_queryResults = mb_queryResults
          }
      )
  -- render children
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
