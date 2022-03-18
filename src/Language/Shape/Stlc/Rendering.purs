module Language.Shape.Stlc.Rendering where

import Data.List.Unsafe
import Data.Tuple.Nested
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.RenderingAux
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Typing
import Prelude
import Prim hiding (Type)
import Data.Array as Array
import Data.Map.Unsafe as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Debug as Debug
import Effect (Effect)
import Effect.Class.Console as Console
import Language.Shape.Stlc.Index as Index
import Language.Shape.Stlc.Initial as Initial
import Language.Shape.Stlc.Recursion.Index as RecIndex
import Language.Shape.Stlc.Recursion.MetaContext as RecMetaContext
import Language.Shape.Stlc.Syntax as Syntax
import React as React
import React.DOM as DOM
import React.DOM.Props as Prop
import React.DOM.Props as Props
import React.SyntheticEvent (NativeEvent, NativeEventTarget, stopPropagation, target)
import Record as R
import Undefined (undefined)
import Unsafe as Unsafe
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (HTMLElement, window)
import Web.HTML.Window (toEventTarget)

-- import Web.DOM (Element)
-- import Web.DOM.NonElementParentNode (getElementById)
-- import Web.Event.EventTarget (addEventListener, eventListener)
-- import Web.HTML.HTMLDocument (toNonElementParentNode)
-- import Web.HTML.Window (document, toEventTarget)
type ProgramProps
  = {}

type ProgramState
  = { module_ :: Module
    , ix_cursor :: DownwardIndex
    , environment :: Environment
    , parents :: List HTMLElement
    }

type Environment
  = { goal :: Maybe Type
    , gamma :: Context
    , metaGamma :: RecMetaContext.MetaContext
    }

type ProgramGiven
  = { state :: ProgramState
    , render :: Effect React.ReactElement
    , componentDidMount :: Effect Unit
    }

foreign import code :: Event -> String

-- foreign import setNativeEventTargetProp :: forall a. NativeEventTarget -> String -> a -> Effect Unit
-- foreign import getClassName :: NativeEventTarget -> String
foreign import setHTMLElementField :: forall a. String -> a -> HTMLElement -> Effect Unit

foreign import getElementById :: String -> Effect HTMLElement

programClass :: React.ReactClass ProgramProps
programClass = React.component "Program" programComponent

programComponent :: React.ReactThis ProgramProps ProgramState -> Effect ProgramGiven
programComponent this =
  pure
    { state
    , render: render <$> React.getState this
    , componentDidMount:
        do
          Console.log "componentDidMount"
          win <- window
          listener <- eventListener keyboardEventHandler
          addEventListener (EventType "keydown") listener false (toEventTarget win)
    }
  where
  keyboardEventHandler :: Event -> Effect Unit
  keyboardEventHandler event = do
    -- st <- React.getState this
    -- Debug.traceM $ "--------------------------------------------------------------------------------"
    -- Debug.traceM $ show st.ix_cursor
    -- case code event of
    --   "ArrowUp" -> React.modifyState this \st -> st { ix_cursor = moveDownwardIndex Up st.module_ st.ix_cursor }
    --   "ArrowDown" -> React.modifyState this \st -> st { ix_cursor = moveDownwardIndex Down st.module_ st.ix_cursor }
    --   "ArrowLeft" -> React.modifyState this \st -> st { ix_cursor = moveDownwardIndex Left st.module_ st.ix_cursor }
    --   "ArrowRight" -> React.modifyState this \st -> st { ix_cursor = moveDownwardIndex Right st.module_ st.ix_cursor }
    --   _ -> pure unit
    -- st' <- React.getState this
    -- Debug.traceM $ show st'.ix_cursor
    Debug.traceM event

  state :: ProgramState
  state =
    { module_: Initial.module_
    , ix_cursor: DownwardIndex Nil
    , environment:
        { goal: Nothing
        , gamma: Map.empty
        , metaGamma: RecMetaContext.emptyMetaContext
        }
    , parents: Nil
    }

  render :: ProgramState -> React.ReactElement
  render st =
    DOM.div [ Props.className "editor" ]
      [ renderModule st.module_ Map.empty RecMetaContext.emptyMetaContext (UpwardIndex Nil) (Just st.ix_cursor)
      -- , renderEnvironment st.environment
      ]

  renderEnvironment :: Environment -> React.ReactElement
  renderEnvironment env =
    let
      contextItemProps termId type_ =
        let
          _ /\ beta = flattenArrowType type_
        in
          [ Props.className $ "context-item"
          -- TODO: this doesnt work with `==` since that's syntactic equality
          -- <> if Just type_ == env.goal then
          --     " suggested exact"
          --   else if Just beta == env.goal then
          --     " suggested apply"
          --   else
          --     ""
          ]
    in
      DOM.div [ Props.className "environment" ]
        $ [ DOM.div
              [ Props.className "context" ]
              ( map
                  ( \(termId /\ type_) ->
                      DOM.div
                        (contextItemProps termId type_)
                        [ renderTermId' termId env.metaGamma
                        , punctuation.space
                        , punctuation.colon
                        , punctuation.space
                        , renderType' type_ Map.empty env.metaGamma -- actually, renderType' shouldn't take a type context at all
                        ]
                  )
                  (Array.reverse $ Map.toUnfoldable env.gamma)
              )
          ]
        <> case env.goal of
            Just type_ ->
              [ DOM.div [ Props.className "goal" ]
                  [ renderType' type_ Map.empty env.metaGamma ]
              ]
            Nothing -> []

  renderModule :: RecIndex.RecModule React.ReactElement
  renderModule =
    RecIndex.recModule
      { module_:
          \defItems meta gamma metaGamma ix isSelected ix_defItems cursor_defItems ->
            DOM.span
              (selectableProps "module" isSelected ix)
              [ renderDefinitionItems defItems gamma metaGamma ix ix_defItems cursor_defItems
              , punctuation.newline
              , punctuation.newline
              , renderInsertDefinitionButton (ix :- IndexStep StepCons 1)
              ]
      }

  renderBlock :: RecIndex.RecBlock React.ReactElement
  renderBlock =
    RecIndex.recBlock
      { block:
          \defItems a meta gamma alpha metaGamma ix isSelected ix_defItems cursor_defItems ix_term cursor_term ->
            DOM.span
              (selectableProps "block" isSelected ix)
              [ renderDefinitionItems defItems gamma metaGamma ix ix_defItems cursor_defItems
              , indent meta metaGamma
              , renderInsertDefinitionButton (ix :- IndexStep StepCons 1)
              , indent meta metaGamma
              , renderTerm a gamma alpha metaGamma ix_term cursor_term
              ]
      }

  renderDefinitionItems :: RecIndex.RecDefinitionItems React.ReactElement
  renderDefinitionItems =
    RecIndex.recDefinitionItems
      { definitionItems:
          \defItems gamma metaGamma ix_parent ix isSelected ix_def_at cursor_def_at ->
            DOM.span
              (inertProps "definitionItems")
              [ intercalateHTML
                  [ punctuation.newline, punctuation.newline ]
                  $ toUnfoldable
                  $ mapWithIndex (\i def -> renderDefinition def gamma metaGamma ix (ix_def_at i) (cursor_def_at i)) (fromItem <$> defItems)
              ]
      }

  renderDefinition :: RecIndex.RecDefinition React.ReactElement
  renderDefinition =
    RecIndex.recDefinition
      { term:
          \termBinding alpha a meta gamma metaGamma ix_parent ix isSelected ix_termBinding cursor_termBinding ix_alpha cursor_alpha ix_a cursor_a ->
            DOM.span
              (selectableProps "term definition" isSelected ix)
              [ DOM.span (selectableTriggerProps ix Nothing gamma metaGamma)
                  [ renderTermBinding termBinding gamma metaGamma ix_termBinding cursor_termBinding ]
              , punctuation.space
              , punctuation.colon
              , indentOrSpace meta metaGamma
              , renderType alpha gamma metaGamma ix_alpha cursor_alpha
              , punctuation.newline
              , indentation (R.modify RecMetaContext._indentation (_ - 1) metaGamma)
              , DOM.span (selectableTriggerProps ix Nothing gamma metaGamma)
                  [ renderTermBinding termBinding gamma metaGamma ix_termBinding cursor_termBinding ]
              , punctuation.space
              , punctuation.termdef
              , indentOrSpace meta metaGamma
              , renderTerm a gamma alpha metaGamma ix_a cursor_a
              ]
      , data:
          \typeBinding@(TypeBinding typeId _) constrItems meta gamma metaGamma ix_parent ix isSelected ix_typeBinding cursor_typeBinding ix_constr_at cursor_constr_at ->
            DOM.span
              (selectableProps "data definition" isSelected ix)
              [ DOM.span (selectableTriggerProps ix Nothing gamma metaGamma)
                  [ renderTypeBinding typeBinding gamma metaGamma ix_typeBinding cursor_typeBinding ]
              , punctuation.space
              , punctuation.typedef
              , DOM.span'
                  [ intersperseLeftHTML
                      [ indentOrSpace { indented: true } metaGamma, punctuation.alt, punctuation.space ]
                      $ Array.fromFoldable
                      $ mapWithIndex (\i constr -> renderConstructor constr typeId gamma metaGamma ix ix_parent (ix_constr_at i) (cursor_constr_at i)) (fromItem <$> constrItems)
                  ]
              ]
      }

  renderConstructor :: RecIndex.RecConstructor React.ReactElement
  renderConstructor =
    RecIndex.recConstructor
      { constructor:
          \termBinding prms meta typeId gamma alpha metaGamma metaGamma_prm_at ix_parent ix_def ix isSelected ix_termBinding cursor_termBinding ix_prm_at cursor_prm_at ->
            DOM.span
              (selectableProps "constructor" isSelected ix)
              $ [ renderTermBinding termBinding gamma metaGamma ix_termBinding cursor_termBinding
                , punctuation.space
                , DOM.span
                    (inertProps "constructor parameters")
                    [ intersperseRightHTML
                        [ punctuation.space ]
                        $ Array.fromFoldable
                        $ mapWithIndex
                            ( \i (prm /\ meta) ->
                                renderParameter prm gamma (metaGamma_prm_at i) (ix_prm_at i) (cursor_prm_at i)
                            )
                            prms
                    ]
                ]
      -- -- DOM.span
      -- --   (selectableProps "constructor" isSelected ix)
      -- --   $ [ DOM.span (selectableTriggerProps ix)
      -- --         [ renderTermBinding termBinding gamma metaGamma ix_termBinding cursor_termBinding ]
      -- --     , punctuation.space
      -- --     , punctuation.colon
      -- --     , punctuation.space
      -- --     ]
      -- --   <> ( if length prms == 0 then
      -- --         []
      -- --       else
      -- --         [ DOM.span
      -- --             (inertProps "constructor parameters")
      -- --             [ intersperseRightHTML
      -- --                 [ punctuation.space ]
      -- --                 $ Array.fromFoldable
      -- --                 $ mapWithIndex (\i prm -> renderParameter prm gamma metaGamma (ix_prm_at i) (cursor_prm_at i)) prms
      -- --             ]
      -- --         , punctuation.space
      -- --         ]
      -- --     )
      -- --   <> [ renderType' (DataType typeId defaultDataTypeMetadata) gamma metaGamma ]
      -- DOM.span
      --   (selectableProps "constructor" isSelected ix)
      --   $ [ DOM.span (selectableTriggerProps ix Nothing gamma metaGamma)
      --         [ renderTermBinding termBinding gamma metaGamma ix_termBinding cursor_termBinding ]
      --     , punctuation.space
      --     , punctuation.colon
      --     , punctuation.space
      --     , renderType' alpha gamma metaGamma
      --     ]
      }

  renderType :: RecIndex.RecType React.ReactElement
  renderType =
    RecIndex.recType
      { arrow:
          \prm beta meta gamma metaGamma ix isSelected ix_prm cursor_prm ix_beta cursor_beta ->
            DOM.span
              (selectableProps "arrow type" isSelected ix)
              $ [ renderParameter prm gamma metaGamma ix_prm cursor_prm
                , punctuation.space
                ]
              <> case beta of
                  ArrowType _ _ _ ->
                    [ renderType beta gamma metaGamma ix_beta cursor_beta
                    ]
                  _ ->
                    [ punctuation.arrow
                    , punctuation.space
                    , renderType beta gamma metaGamma ix_beta cursor_beta
                    ]
      , data:
          \typeId meta gamma metaGamma ix isSelected ->
            DOM.span
              (selectableProps "data type typeId" isSelected ix <> selectableTriggerProps ix Nothing gamma metaGamma)
              [ printTypeId typeId metaGamma ]
      , hole:
          \holeId wkn meta gamma metaGamma ix isSelected ->
            DOM.span
              (selectableProps "hole type" isSelected ix)
              [ DOM.text "?" ]
      , proxyHole:
          \holeId gamma metaGamma ix isSelected ->
            DOM.span
              (selectableProps "proxy hole type" isSelected ix)
              [ DOM.text "?" ]
      }

  renderType' :: RecMetaContext.RecType React.ReactElement
  renderType' =
    RecMetaContext.recType
      { arrow:
          \prm beta meta gamma metaGamma ->
            DOM.span
              (inertProps "arrow type")
              $ [ renderParameter' prm gamma metaGamma
                , punctuation.space
                ]
              <> case beta of
                  ArrowType _ _ _ ->
                    [ renderType' beta gamma metaGamma
                    ]
                  _ ->
                    [ punctuation.arrow
                    , punctuation.space
                    , renderType' beta gamma metaGamma
                    ]
      , data:
          \typeId meta gamma metaGamma ->
            DOM.span
              (inertProps "data type")
              [ printTypeId typeId metaGamma ]
      , hole:
          \holeId wkn meta gamma metaGamma ->
            DOM.span
              (inertProps "hole type")
              [ DOM.text "?" ]
      , proxyHole:
          \holeId gamma metaGamma ->
            DOM.span
              (inertProps "proxy hole type")
              [ DOM.text "?" ]
      }

  renderTerm :: RecIndex.RecTerm React.ReactElement
  renderTerm term_prt gamma_prt type_prt metaGamma_prt ix_prt crs_prt =
    RecIndex.recTerm
      { lambda:
          \termId block meta gamma prm beta metaGamma ix isSelected ix_termId cursor_termId ix_block cursor_block ->
            DOM.span
              (selectableProps "lambda term" isSelected ix)
              $ [ DOM.span (selectableTriggerProps ix (Just (ArrowType prm beta defaultArrowTypeMetadata)) gamma_prt metaGamma)
                    [ renderTermId termId gamma metaGamma ix_termId cursor_termId ]
                ]
              <> case block of
                  Block Nil (LambdaTerm _ _ _) _ ->
                    [ renderBlock block gamma beta metaGamma ix_block cursor_block
                    ]
                  _ ->
                    [ punctuation.space
                    , punctuation.mapsto
                    , indent meta metaGamma
                    , renderBlock block gamma beta metaGamma ix_block cursor_block
                    ]
      , neutral:
          \termId argItems meta gamma alpha metaGamma ix isSelected ix_termId cursor_termId ix_argItems cursor_argItems ->
            DOM.span
              (selectableProps "neutral term" isSelected ix)
              [ DOM.span
                  (selectableProps "termId" isSelected ix <> selectableTriggerProps ix (Just $ Map.lookup' termId gamma) gamma_prt metaGamma)
                  [ printTermId termId metaGamma ]
              -- renderTermId termId gamma metaGamma ix_termId cursor_termId
              , renderArgItems argItems gamma alpha metaGamma ix_argItems cursor_argItems
              ]
      , match:
          \typeId a caseItems meta gamma alpha metaGamma constrIds ix isSelected ix_term cursor_term ix_case_at cursor_case_at ->
            DOM.span
              (selectableProps "match term" isSelected ix)
              [ DOM.span (selectableTriggerProps ix (Just alpha) gamma_prt metaGamma)
                  [ keyword.match ]
              , punctuation.space
              , renderTerm a gamma (DataType typeId defaultDataTypeMetadata) metaGamma ix_term cursor_term
              , punctuation.space
              , keyword.with
              , DOM.span
                  (inertProps "match caseItems")
                  [ intercalateHTML [ indentOrSpace meta metaGamma, punctuation.alt, punctuation.space ]
                      $ Array.fromFoldable
                      $ mapWithIndex
                          (\i case_ -> renderCase case_ typeId (index' constrIds i) gamma alpha metaGamma ix (ix_case_at i) (cursor_case_at i))
                          (fromItem <$> caseItems)
                  ]
              ]
      , hole:
          \meta gamma alpha metaGamma ix isSelected ->
            DOM.span
              (selectableProps "hole term" isSelected ix <> selectableTriggerProps ix (Just alpha) gamma_prt metaGamma)
              [ renderType' alpha gamma metaGamma ]
      }
      term_prt
      gamma_prt
      type_prt
      metaGamma_prt
      ix_prt
      crs_prt

  renderArgItems :: RecIndex.RecArgItems React.ReactElement
  renderArgItems =
    RecIndex.recArgItems
      { nil: \gamma alpha metaGamma -> DOM.span' []
      , cons:
          \(a /\ argItem_meta) argItems gamma (Parameter alpha _) beta metaGamma ix isSelected ix_a cursor_a ix_argItems cursor_argItems ->
            DOM.span
              (selectableProps "argItems" isSelected ix)
              $ [ punctuation.space ]
              <> case a of
                  LambdaTerm _ _ _ -> [ punctuation.lparen, renderTerm a gamma alpha metaGamma ix_a cursor_a, punctuation.lparen ]
                  _ -> [ renderTerm a gamma alpha metaGamma ix_a cursor_a ]
              <> if argItems == Nil then
                  []
                else
                  [ punctuation.space
                  , renderArgItems argItems gamma beta metaGamma ix_argItems cursor_argItems
                  ]
      }

  renderCase :: RecIndex.RecCase React.ReactElement
  renderCase case_prt typeId_prt termId_prt gamma_prt alpha_prt metaGamma_prt ix_prt cursor_prt =
    RecIndex.recCase
      { case_:
          \termIdItems term meta typeId constrId gamma alpha metaGamma ix_match ix isSelected ix_termId_at cursor_termId_at ix_term cursor_term ->
            DOM.span
              (selectableProps "case" isSelected ix)
              [ DOM.span (selectableTriggerProps ix Nothing gamma metaGamma)
                  [ renderTermId' constrId metaGamma ]
              , DOM.span
                  (inertProps "case termIdItems")
                  [ intersperseLeftHTML [ punctuation.space ]
                      $ Array.fromFoldable
                      $ mapWithIndex (\i termId -> renderTermId termId gamma metaGamma (ix_termId_at i) (cursor_termId_at i)) (fromItem <$> termIdItems)
                  ]
              , punctuation.space
              , renderTerm term gamma alpha metaGamma ix_term cursor_term
              ]
      }
      case_prt
      typeId_prt
      termId_prt
      gamma_prt
      alpha_prt
      metaGamma_prt
      ix_prt
      cursor_prt

  renderParameter :: RecIndex.RecParameter React.ReactElement
  renderParameter =
    RecIndex.recParameter
      { parameter:
          \alpha meta gamma metaGamma ix isSelected ix_alpha cursor_alpha ->
            DOM.span
              (selectableProps "parameter" isSelected ix)
              -- [ renderType alpha gamma metaGamma ix_alpha cursor_alpha ]
              [ punctuation.lparen
              , DOM.span (selectableTriggerProps ix Nothing gamma metaGamma) [ printTermName meta.name metaGamma ]
              , punctuation.space
              , punctuation.colon
              , punctuation.space
              , renderType alpha gamma metaGamma ix_alpha cursor_alpha
              , punctuation.rparen
              ]
      }

  renderParameter' :: RecMetaContext.RecParameter React.ReactElement
  renderParameter' =
    RecMetaContext.recParameter
      { parameter:
          \alpha meta gamma metaGamma ->
            DOM.span
              (inertProps "parameter")
              -- [ renderType' alpha gamma metaGamma ]
              [ punctuation.lparen
              , printTermName meta.name metaGamma
              , punctuation.space
              , punctuation.colon
              , punctuation.space
              , renderType' alpha gamma metaGamma
              , punctuation.rparen
              ]
      }

  renderTypeBinding :: RecIndex.RecTypeBinding React.ReactElement
  renderTypeBinding =
    RecIndex.recTypeBinding
      { typeBinding:
          \typeId meta gamma metaGamma ix isSelected ->
            DOM.span
              (selectableProps "typeBinding" isSelected ix <> selectableTriggerProps ix Nothing gamma metaGamma)
              [ printTypeId typeId metaGamma ]
      }

  renderTermBinding :: RecIndex.RecTermBinding React.ReactElement
  renderTermBinding =
    RecIndex.recTermBinding
      { termBinding:
          \termId meta gamma metaGamma ix isSelected ->
            DOM.span
              (selectableProps "termBinding" isSelected ix <> selectableTriggerProps ix Nothing gamma metaGamma)
              [ printTermId termId metaGamma ]
      }

  renderTermId :: RecIndex.RecTermId React.ReactElement
  renderTermId =
    RecIndex.recTermId
      { termId:
          \termId gamma metaGamma ix isSelected ->
            DOM.span
              (selectableProps "termId" isSelected ix <> selectableTriggerProps ix Nothing gamma metaGamma)
              [ printTermId termId metaGamma ]
      }

  renderTermId' :: TermId -> RecMetaContext.MetaContext -> React.ReactElement
  renderTermId' termId metaGamma = DOM.span (inertProps "termId") [ printTermId termId metaGamma ]

  printTypeId :: TypeId -> RecMetaContext.MetaContext -> React.ReactElement
  printTypeId typeId metaGamma = DOM.span [ Props.className "typeId" ] ([ DOM.text typeString ] <> shadow_suffix)
    where
    TypeName typeLabel = Map.lookup' typeId metaGamma.typeScope.names

    typeString = maybe "_" identity typeLabel

    shadow_i = Map.lookup' typeId metaGamma.typeScope.shadowIndices

    shadow_suffix = if shadow_i == 0 then [] else [ printShadowSuffix $ show shadow_i ]

  printTermId :: TermId -> RecMetaContext.MetaContext -> React.ReactElement
  printTermId termId metaGamma = DOM.span' ([ DOM.text termString ] <> shadow_suffix)
    where
    TermName termLabel = Map.lookup' termId metaGamma.termScope.names

    termString = maybe "_" identity termLabel

    shadow_i = Map.lookup' termId metaGamma.termScope.shadowIndices

    shadow_suffix = if shadow_i == 0 then [] else [ printShadowSuffix $ show shadow_i ]

  printTermName :: TermName -> RecMetaContext.MetaContext -> React.ReactElement
  printTermName termName@(TermName termLabel) metaGamma = DOM.span [ Props.className "termName" ] ([ DOM.text termString ] <> shadow_suffix)
    where
    termString = maybe "_" identity termLabel

    shadow_i = Map.lookup' termName metaGamma.termScope.shadows

    shadow_suffix = if shadow_i == 0 then [] else [ printShadowSuffix $ show shadow_i ]

  printShadowSuffix :: String -> React.ReactElement
  printShadowSuffix str = DOM.span [ Props.className "shadow-suffix" ] [ DOM.text str ]

  renderInsertDefinitionButton :: UpwardIndex -> React.ReactElement
  renderInsertDefinitionButton ix =
    DOM.span
      [ Props.className "insertDefinition" ]
      []

  selectableProps :: String -> Boolean -> UpwardIndex -> Array Props.Props
  selectableProps title isSelected ix =
    let
      _id = upwardIndexToId ix
    in
      [ Props.className $ title <> if isSelected then " selected" else ""
      , Props._id _id
      -- , Props.style
      --     { boxShadow: if isSelected then "0 0 0 1px rgb(211, 115, 0)" else ""
      --     , backgroundColor: if isSelected then "rgba(255, 255, 255, 0.2)" else ""
      --     }
      , Props.onMouseDown \event -> do
          stopPropagation event
          Debug.traceM $ show (toDownwardIndex ix)
          React.modifyState this \st -> st { ix_cursor = toDownwardIndex ix }
      , Prop.onMouseOver \event -> do
          stopPropagation event
          st <- React.getState this
          self <- getElementById _id
          -- if parent on top of stack, then unhighlight it
          -- highlight self
          -- push self to stack
          case st.parents of
            Nil -> pure unit
            Cons parent _ -> unhighlight parent
          highlight self
          React.modifyState this \st -> st { parents = Cons self st.parents }
      , Prop.onMouseOut \event -> do
          stopPropagation event
          st <- React.getState this
          self <- getElementById _id
          -- unhighlight self
          -- pop self from stack
          -- if parent on top of stack, then highlight it
          unhighlight self
          case st.parents of
            Nil -> Unsafe.error "tried to pop self, but parents were empty"
            Cons _ Nil -> do
              React.modifyState this \st -> st { parents = Nil }
            Cons _ (Cons parent parents') -> do
              highlight parent
              React.modifyState this \st -> st { parents = Cons parent parents' }
      ]

  selectableTriggerProps :: UpwardIndex -> Maybe Type -> Context -> RecMetaContext.MetaContext -> Array Props.Props
  selectableTriggerProps ix type_ gamma metaGamma = []

  inertProps :: String -> Array Props.Props
  inertProps title = [ Props.className title ]

highlight :: HTMLElement -> Effect Unit
highlight elem = do
  -- Debug.traceM elem
  setHTMLElementField "style" "box-shadow: 0 0 0 1px purple; background-color: rgba(255, 255, 255, 0.2)" elem

unhighlight :: HTMLElement -> Effect Unit
unhighlight elem = do
  -- Debug.traceM elem
  setHTMLElementField "style" "" elem

-- highlight :: NativeEventTarget -> Effect Unit
-- highlight t = setNativeEventTargetProp t "style" "box-shadow: 0 0 0 1px purple; background-color: rgba(255, 255, 255, 0.2)"
-- unhighlight :: NativeEventTarget -> Effect Unit
-- unhighlight t = setNativeEventTargetProp t "style" ""
upwardIndexToId :: UpwardIndex -> String
upwardIndexToId ix = case unconsUpwardIndex ix of
  Nothing -> ""
  Just { step: IndexStep label i, ix' } -> show label <> "-" <> show i <> "--" <> upwardIndexToId ix'
