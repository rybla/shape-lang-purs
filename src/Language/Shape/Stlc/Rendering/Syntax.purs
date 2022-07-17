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
import Control.Monad.State (State, runState)
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
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Union)
import React (ReactElement, getState, modifyState)
import React.DOM as DOM
import React.DOM.Props as Props
import React.SyntheticEvent (shiftKey, stopPropagation)
import Record as Record
import Undefined (undefined)

renderProgram :: This -> Effect (Array ReactElement /\ RenderEnvironment)
renderProgram this = do
  st <- getState this
  let
    renEnv = emptyRenderEnvironment st
  -- Debug.traceM $ "===[ st.term ]==============================="
  -- Debug.traceM $ show st.term
  let
    (elems /\ env) =
      flip runState renEnv
        $ renderTerm this
            -- TODO: maybe pull this out into multiple files or at least somewhere else?
            { term: st.term
            , gamma: default
            , alpha: st.type_
            , visit: nilVisit st.mb_ix
            , meta: default
            }
  pure
    ( [ DOM.div
          [ Props.className "program"
          , Props.onClick \event -> do
              -- Debug.traceM "clicked on program background"
              stopPropagation event
              modifyState this (_ { mb_ix = Nothing }) -- unselect
          -- , Props.onMouseDown \event -> do
          --     Debug.traceM "onMouseDown on program background"
          ]
          elems
      ]
        /\ env
    )

renderType :: This -> Record (Rec.ArgsType ()) -> M (Array ReactElement)
renderType this =
  Rec.recType
    { arrowType:
        \args ->
          renderNode this
            ((makeNodeProps args) { label = Just "ArrowType", syntax = Just $ SyntaxType $ ArrowType args.arrowType })
            [ enParenIf (renderType this args.dom) (requiresParenType args.dom.type_)
            , pure [ token.arrowType1 ]
            , renderType this args.cod
            ]
    , dataType:
        \args ->
          renderNode this
            ((makeNodeProps args) { label = Just "DataType", syntax = Just $ SyntaxType $ DataType args.dataType })
            [ printTypeId args.typeId ]
    , holeType:
        \args -> do
          State.modify_ (Record.modify _holeIds (OrderedSet.insert args.holeType.holeId)) -- should be inserted into ordered set
          renderNode this
            ((makeNodeProps args) { label = Just "HoleType", syntax = Just $ SyntaxType $ HoleType args.holeType })
            [ printHoleId { holeId: args.holeType.holeId, meta: args.holeId.meta } ]
    }

addHoleIdsFromType :: Type -> M Unit
addHoleIdsFromType = case _ of
  ArrowType arrow -> do
    addHoleIdsFromType arrow.dom
    addHoleIdsFromType arrow.cod
  DataType data_ -> pure unit
  HoleType hole -> State.modify_ (Record.modify _holeIds (OrderedSet.insert hole.holeId))

renderTerm :: This -> Record (Rec.ArgsTerm ()) -> M (Array ReactElement)
renderTerm this =
  Rec.recTerm
    { lam:
        \args -> do
          addHoleIdsFromType args.alpha
          renderNode this
            ((makeNodeProps args) { label = Just "Lam", alpha = Just args.alpha, syntax = Just $ SyntaxTerm $ Lam args.lam })
            [ pure [ token.lam1 ]
            , renderTermBind this args.termBind
            , pure [ token.lam2 ]
            , pure $ newline args.body.meta (unwrap args.lam.meta).indentedBody
            , renderTerm this args.body
            ]
    , neu:
        \args ->
          renderNode this
            ((makeNodeProps args) { label = Just "Neu", alpha = Just args.alpha, syntax = Just $ SyntaxTerm $ Neu args.neu })
            if List.length args.neu.argItems == 0 then
              [ renderTermId this args.termId
              ]
            else
              [ renderTermId this args.termId
              -- , pure [ token.neu1 ]
              , renderItems (renderArgItem this <$> args.argItems)
              ]
    , let_:
        \args ->
          renderNode this
            ((makeNodeProps args) { label = Just "Let", alpha = Just args.alpha, syntax = Just $ SyntaxTerm $ Let args.let_ })
            [ pure [ token.let1 ]
            , renderTermBind this args.termBind
            , pure [ token.let2 ]
            , if (unwrap args.let_.meta).indentedSign then
                concat
                  <$> sequence
                      [ pure $ newline args.sign.meta true
                      , renderType this args.sign
                      ]
              else
                renderType this args.sign
            , pure [ token.let3 ]
            , if (unwrap args.let_.meta).indentedImpl then
                concat
                  <$> sequence
                      [ pure $ newline args.impl.meta true
                      , renderTerm this args.impl
                      ]
              else
                renderTerm this args.impl
            , if (unwrap args.let_.meta).indentedBody then
                concat
                  <$> sequence
                      [ pure $ newline args.body.meta true
                      , renderTerm this args.body
                      ]
              else
                concat
                  <$> sequence
                      [ pure [ token.let4 ]
                      , renderTerm this args.body
                      ]
            ]
    , buf:
        \args ->
          renderNode this
            ((makeNodeProps args) { label = Just "Buf", alpha = Just args.alpha, syntax = Just $ SyntaxTerm $ Buf args.buf })
            [ pure [ token.buf1 ]
            , pure $ newline args.impl.meta (unwrap args.buf.meta).indentedImpl
            , renderTerm this args.impl
            , pure [ token.buf2 ]
            , pure $ newline args.sign.meta (unwrap args.buf.meta).indentedSign
            , renderType this args.sign
            -- , pure $ newlineOrSpace args.body.meta (unwrap args.buf.meta).indentedBody
            -- , pure [ token.buf3 ]
            -- , pure $ newlineOrSpace args.body.meta (unwrap args.buf.meta).indentedBody
            -- , renderTerm this args.body
            , if (unwrap args.buf.meta).indentedBody then
                concat
                  <$> sequence
                      [ pure $ newline args.body.meta true
                      , renderTerm this args.body
                      ]
              else
                concat
                  <$> sequence
                      [ pure [ token.buf3 ]
                      , renderTerm this args.body
                      ]
            ]
    , data_:
        \args ->
          renderNode this
            ((makeNodeProps args) { label = Just "Data", alpha = Just args.alpha, syntax = Just $ SyntaxTerm $ Data args.data_ })
            [ pure [ token.data1 ]
            , renderTypeBind this args.typeBind
            , pure [ token.data2 ]
            , renderItems (renderSumItem this <$> args.sumItems)
            , if (unwrap args.data_.meta).indentedBody then
                concat
                  <$> sequence
                      [ pure $ newline args.body.meta true
                      , renderTerm this args.body
                      ]
              else
                concat
                  <$> sequence
                      [ pure [ token.data3 ]
                      , renderTerm this args.body
                      ]
            ]
    , match:
        \args -> do
          -- Debug.traceM $ "rendering args.caseItems: " <> show args.caseItems
          renderNode this
            ((makeNodeProps args) { label = Just "Match", alpha = Just args.alpha, syntax = Just $ SyntaxTerm $ Match args.match })
            [ pure [ token.match1 ]
            , renderTerm this args.term
            , pure [ token.match2 ]
            , renderItems (renderCaseItem this <$> args.caseItems)
            ]
    , hole:
        \args ->
          (\elems -> [ DOM.span [ Props.className "hole-container" ] elems ])
            <$> renderNode this
                ((makeNodeProps args) { label = Just "Hole", alpha = Just args.alpha, syntax = Just $ SyntaxTerm $ Hole args.hole })
                [ State.gets (_.st.mode)
                    >>= case _ of
                        QueryMode q
                          | args.visit.csr == Just nilIxDown -> do
                            let
                              -- filter the context for variables that have a
                              -- name fuzzy-matching the query string
                              variableQueryItems :: Array (TermId /\ Type)
                              variableQueryItems =
                                map (\(termId /\ type_ /\ _) -> (termId /\ type_))
                                  $ Array.sortWith (\(_ /\ _ /\ dist) -> dist)
                                  $ Array.foldMap
                                      ( \(termId /\ type_) ->
                                          let
                                            Name mb_str = lookupVarName termId args.meta
                                          in
                                            case fuzzyDistance q.query =<< mb_str of
                                              Just dist -> [ termId /\ type_ /\ dist ]
                                              Nothing -> []
                                      )
                                  $ OrderedMap.toArray (unwrap args.gamma).varTypes
                            -- update the render environment to know about the
                            -- current variable query result
                            State.modify_ _ { variableQueryResult = Just variableQueryItems }
                            -- render each items from the query result
                            variableQueryElems <-
                              sequence
                                $ Array.mapWithIndex
                                    ( \i (termId /\ type_) -> do
                                        elemTermId <- printTermId { termId, gamma: args.gamma, meta: args.meta, visit: nonVisit }
                                        elemType <- renderType this { type_, gamma: args.gamma, meta: args.meta, visit: nonVisit }
                                        pure
                                          $ ( DOM.span [ Props.className $ "query-context-item" <> if q.i == i then " selected" else "" ]
                                                $ concat
                                                    [ elemTermId
                                                    , [ DOM.text " : " ]
                                                    , elemType
                                                    ]
                                            )
                                    )
                                $ variableQueryItems
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
                                        [ DOM.text q.query ]
                                    ]
                                , DOM.span [ Props.className "query-sep" ]
                                    [ DOM.text " .. : " ]
                                ]
                        _ -> pure []
                , renderType this { type_: args.alpha, gamma: args.gamma, visit: nonVisit, meta: args.meta }
                ]
    }

renderArgItem :: This -> Record (Rec.ArgsArgItem ()) -> M (Array ReactElement)
renderArgItem this =
  Rec.recArgItem
    { argItem:
        \args ->
          renderNode this
            ( (makeNodeProps args) { label = Just "ArgItem" }
            )
            $ [ pure $ newlineOrSpace args.meta (unwrap args.argItem.meta).indented
              , enParenIf (renderTerm this args.term) (requiresParenTerm args.term.term)
              -- , renderTerm this args.term
              ]
    }

renderSumItem :: This -> Record (Rec.ArgsSumItem ()) -> M (Array ReactElement)
renderSumItem this =
  Rec.recSumItem
    { sumItem:
        \args ->
          renderNode this
            ( (makeNodeProps args) { label = Just "SumItem" }
            )
            [ pure $ newline args.meta (unwrap args.sumItem.meta).indented
            , pure [ token.sumItem1 ]
            , renderTermBind this args.termBind
            , pure [ token.sumItem2 ]
            , renderItems (renderParamItem this <$> args.paramItems)
            ]
    }

renderCaseItem :: This -> Record (Rec.ArgsCaseItem ()) -> M (Array ReactElement)
renderCaseItem this =
  Rec.recCaseItem
    { caseItem:
        \args -> do
          renderNode this
            (makeNodeProps args) { label = Just "CaseItem" }
            [ pure $ newline args.meta (unwrap args.caseItem.meta).indented
            , pure [ token.caseItem1 ]
            , printTermId { termId: args.termId, gamma: args.gamma, visit: nonVisit, meta: args.meta }
            , pure [ token.caseItem2 ]
            , renderItems (renderTermBindItem this <$> args.termBindItems)
            , pure [ token.caseItem3 ]
            , renderTerm this args.body
            ]
    }

renderParamItem :: This -> Record (Rec.ArgsParamItem ()) -> M (Array ReactElement)
renderParamItem this =
  Rec.recParamItem
    { paramItem:
        \args ->
          renderNode this
            (makeNodeProps args) { label = Just "ParamItem" }
            [ pure $ newline args.meta (unwrap args.paramItem.meta).indented
            , renderType this args.type_
            ]
    }

renderTermBindItem :: This -> Record (Rec.ArgsTermBindItem ()) -> M (Array ReactElement)
renderTermBindItem this =
  Rec.recTermBindItem
    { termBindItem:
        \args ->
          renderNode this
            (makeNodeProps args) { label = Just "TermBindItem" }
            [ pure $ newline args.meta (unwrap args.termBindItem.meta).indented
            , renderTermBind this args.termBind
            ]
    }

renderTermBind :: This -> Record (Rec.ArgsTermBind ()) -> M (Array ReactElement)
renderTermBind this =
  Rec.recTermBind
    { termBind:
        \args ->
          renderNode this
            ( (makeNodeProps args)
                { label = Just "TermBind" }
            )
            [ printTermId args.termId ]
    }

renderTypeBind :: This -> Record (Rec.ArgsTypeBind ()) -> M (Array ReactElement)
renderTypeBind this =
  Rec.recTypeBind
    { typeBind:
        \args ->
          renderNode this
            (makeNodeProps args)
              { label = Just "TypeBind" }
            [ printTypeId args.typeId ]
    }

renderTypeId :: This -> Record (Rec.ArgsTypeId ()) -> M (Array ReactElement)
renderTypeId this argsTypeId =
  Rec.recTypeId
    { typeId:
        \args ->
          renderNode this
            (makeNodeProps args) { label = Just "TypeId" }
            [ printTypeId argsTypeId ]
    }
    argsTypeId

renderTermId :: This -> Record (Rec.ArgsTermId ()) -> M (Array ReactElement)
renderTermId this argsTermId =
  Rec.recTermId
    { termId:
        \args ->
          renderNode this
            (makeNodeProps args) { label = Just "TermId" }
            [ printTermId argsTermId ]
    }
    argsTermId

printTypeId :: Record (Rec.ArgsTypeId ()) -> M (Array ReactElement)
printTypeId { typeId, meta } =
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

renderItems :: List (M (Array ReactElement)) -> M (Array ReactElement)
renderItems items = foldM (\elems -> ((elems <> _) <$> _)) [] items

renderNode :: This -> NodeProps -> Array (M (Array ReactElement)) -> M (Array ReactElement)
renderNode this props elemsM = do
  -- if this node is selected
  when isSelected do
    -- Debug.traceM $ "================================="
    -- Debug.traceM $ "renderNode isSelected"
    -- Debug.traceM $ "label = " <> show props.label
    -- Debug.traceM $ "props = " <> show props
    -- Debug.traceM $ "================================="
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
  elems <- concat <$> sequence elemsM
  pure $ [ DOM.span propsSpan elems ]
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
