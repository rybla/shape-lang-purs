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
import Data.Char as Char
import Data.Enum as Enum
import Data.Map.Unsafe as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (fst, snd)
import Data.Unfoldable (class Unfoldable, class Unfoldable1)
import Debug as Debug
import Effect (Effect)
import Effect.Class.Console as Console
import Language.Shape.Stlc.ChangeAtIndex (Change(..), chAtModule)
import Language.Shape.Stlc.Changes (TypeChange(..))
import Language.Shape.Stlc.Holes (subModule)
import Language.Shape.Stlc.Index as Index
import Language.Shape.Stlc.Initial as Initial
import Language.Shape.Stlc.Recursion.Index as RecIndex
import Language.Shape.Stlc.Recursion.MetaContext as RecMetaContext
import Language.Shape.Stlc.Syntax as Syntax
import React as React
import React.DOM as DOM
import React.DOM.Props as Prop
import React.DOM.Props as Props
import React.SyntheticEvent (NativeEvent, NativeEventTarget, SyntheticMouseEvent, stopPropagation, target)
import Record as R
import Undefined (undefined)
import Unsafe as Unsafe
import Web.DOM.DOMTokenList as DOMTokenList
import Web.Event.Event (Event, EventType(..), preventDefault)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLElement (classList)
import Web.HTML.Window (prompt, promptDefault, toEventTarget)

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
    , syntax_dragging :: Maybe Syntax
    , environment :: Environment
    , outlineParents :: List HTMLElement
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

foreign import key :: Event -> String

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
    preventDefault event
    let
      k = key event
    if k `Set.member` nameKeys || k == "Backspace" then do
      let
        modifyName name =
          if k `Set.member` nameKeys then
            maybe (Just k) (Just <<< (_ <> k)) name
          else if k == "Backspace" then
            maybe Nothing (\str -> if String.length str == 1 then Nothing else Just (String.take (String.length str - 1) str)) name
          else
            name
      st <- React.getState this
      module_ <-
        toModule
          <$> modifySyntaxAtM st.ix_cursor
              ( case _ of
                  SyntaxTermBinding termBinding@(TermBinding termId meta) -> pure $ SyntaxTermBinding $ TermBinding termId meta { name = TermName $ modifyName case meta.name of TermName name -> name }
                  SyntaxParameter param@(Parameter alpha meta) -> pure $ SyntaxParameter $ Parameter alpha meta { name = TermName $ modifyName case meta.name of TermName name -> name }
                  SyntaxTypeBinding typeBinding@(TypeBinding typeId meta) -> pure $ SyntaxTypeBinding $ TypeBinding typeId meta { name = TypeName $ modifyName case meta.name of TypeName name -> name }
                  x -> pure x
              )
              (SyntaxModule st.module_)
      React.modifyState this \st -> st { module_ = module_ }
    else
      pure unit
    -- case key event of
    -- "ArrowUp" -> React.modifyState this \st -> st { ix_cursor = moveDownwardIndex Up st.module_ st.ix_cursor }
    -- "ArrowDown" -> React.modifyState this \st -> st { ix_cursor = moveDownwardIndex Down st.module_ st.ix_cursor }
    -- "ArrowLeft" -> React.modifyState this \st -> st { ix_cursor = moveDownwardIndex Left st.module_ st.ix_cursor }
    -- "ArrowRight" -> React.modifyState this \st -> st { ix_cursor = moveDownwardIndex Right st.module_ st.ix_cursor }
    -- "e" -> do
    -- _ -> pure unit
    Debug.traceM event
    pure unit

  state :: ProgramState
  state =
    { module_: Initial.module_
    , ix_cursor: DownwardIndex Nil
    , syntax_dragging: Nothing
    , environment:
        { goal: Nothing
        , gamma: emptyContext
        , metaGamma: RecMetaContext.emptyMetaContext
        }
    , outlineParents: Nil
    }

  render :: ProgramState -> React.ReactElement
  render st =
    DOM.div
      [ Props.className "editor"
      , Props.onMouseUp \event -> do
          stopPropagation event
          Debug.traceM $ "drag cancel"
          React.modifyState this \st -> st { syntax_dragging = Nothing }
      ]
      [ renderModule st.module_ emptyContext RecMetaContext.emptyMetaContext { ix: (UpwardIndex Nil), csr: (Just st.ix_cursor) }
      -- , renderEnvironment st.environment
      ]

  {-
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
  -}
  renderModule :: RecIndex.RecModule React.ReactElement
  renderModule =
    RecIndex.recModule
      { module_:
          \defItems meta gamma metaGamma ixArgs ->
            let
              eid = upwardIndexToEid ixArgs.ix
            in
              DOM.div
                ( [ selectableClassName "module" ixArgs.isSelected, Props._id eid ] <> selectableProps ixArgs.ix
                )
                [ renderDefinitionItems defItems gamma metaGamma { ix_parentBlock: ixArgs.ix, ix: ixArgs.ix_defItems, csr: ixArgs.csr_defItems }
                ]
      }

  renderBlock :: RecIndex.RecBlock React.ReactElement
  renderBlock =
    RecIndex.recBlock
      { block:
          \defItems a meta gamma alpha metaGamma ixArgs ->
            let
              eid = upwardIndexToEid ixArgs.ix
            in
              DOM.span
                ( [ selectableClassName "block" ixArgs.isSelected, Props._id eid ] <> selectableProps ixArgs.ix
                    <> highlightableProps eid
                )
                $ [ renderDefinitionItems defItems gamma metaGamma { ix_parentBlock: ixArgs.ix, ix: ixArgs.ix_defItems, csr: ixArgs.csr_defItems }
                  , indentOrNothing meta metaGamma
                  , renderTerm a gamma alpha metaGamma { ix: ixArgs.ix_term, csr: ixArgs.csr_term }
                  ]
      }

  renderDefinitionItems :: RecIndex.RecDefinitionItems React.ReactElement
  renderDefinitionItems =
    RecIndex.recDefinitionItems
      { definitionItems:
          \defItems gamma metaGamma ixArgs ->
            DOM.span
              (inertProps "definitionItems")
              -- [ intersperseRightHTML
              --     [ punctuation.newline, undefined, punctuation.newline ]
              --     $ toUnfoldable
              --     $ mapWithIndex (\i def -> renderDefinition def gamma metaGamma ix (ix_def_at i) (csr_def_at i)) (fromItem <$> defItems)
              -- ]
              [ DOM.span'
                  $ toUnfoldable
                  $ mapWithIndex
                      ( \i def ->
                          DOM.span'
                            [ indent { indented: true } (R.modify RecMetaContext._indentation (_ - 1) metaGamma)
                            , renderDefinitionSeparator
                                { indented: true }
                                { ix_parentBlock: ixArgs.ix_parentBlock, ix: (ixArgs.ix_defSep_at i), csr: (ixArgs.csr_defSep_at i) }
                            -- , punctuation.newline
                            , indent { indented: true } (R.modify RecMetaContext._indentation (_ - 1) metaGamma)
                            , renderDefinition def gamma metaGamma { ix_parentBlock: ixArgs.ix_parentBlock, ix: (ixArgs.ix_def_at i), csr: (ixArgs.csr_def_at i) }
                            --  { ixArgs.ix (ixArgs.ix_def_at i) (ixArgs.csr_def_at i)}
                            -- , punctuation.newline
                            ]
                      )
                      (fromItem <$> defItems)
              , if length defItems == 0 then
                  renderDefinitionSeparator
                    { indented: false }
                    { ix_parentBlock: ixArgs.ix, ix: (ixArgs.ix_defSep_at (length defItems)), csr: (ixArgs.csr_defSep_at (length defItems)) }
                else
                  DOM.span'
                    [ indent { indented: true } (R.modify RecMetaContext._indentation (_ - 1) metaGamma)
                    , renderDefinitionSeparator
                        { indented: true }
                        { ix_parentBlock: ixArgs.ix, ix: (ixArgs.ix_defSep_at (length defItems)), csr: (ixArgs.csr_defSep_at (length defItems)) }
                    ]
              ]
      }

  renderDefinitionSeparator :: { indented :: Boolean } -> RecIndex.RecDefinitionSeparator React.ReactElement
  renderDefinitionSeparator renderArgs =
    RecIndex.recDefinitionSeparator
      { separator:
          \ixArgs ->
            let
              eid = upwardIndexToEid ixArgs.ix
            in
              DOM.span
                ( [ selectableClassName ("definitionSeparator separator " <> indentedToClassName renderArgs) ixArgs.isSelected, Props._id eid ] <> selectableProps ixArgs.ix
                    <> highlightableProps eid
                )
                -- [ DOM.span' $ if renderArgs.indented then [] else [ DOM.text "|" ]
                [ DOM.span' [ DOM.text "•" ] ]
      }

  renderDefinition :: RecIndex.RecDefinition React.ReactElement
  renderDefinition =
    RecIndex.recDefinition
      { term:
          \termBinding alpha a meta gamma metaGamma ixArgs ->
            let
              eid = upwardIndexToEid ixArgs.ix
            in
              DOM.span
                ( [ selectableClassName "term definition" ixArgs.isSelected, Props._id eid ] <> selectableProps ixArgs.ix
                    <> highlightableProps eid
                )
                [ renderTermBinding termBinding gamma metaGamma { ix: ixArgs.ix_termBinding, csr: ixArgs.csr_termBinding }
                , punctuation.space
                , punctuation.colon
                , indentOrSpace meta metaGamma
                , renderType alpha gamma metaGamma { ix: ixArgs.ix_type, csr: ixArgs.csr_type }
                , punctuation.newline
                , indentation (R.modify RecMetaContext._indentation (_ - 1) metaGamma)
                -- , renderTermBinding termBinding gamma metaGamma ix_termBinding csr_termBinding
                , renderTermBinding' termBinding metaGamma
                , punctuation.space
                , punctuation.termdef
                , indentOrSpace meta metaGamma
                , renderTerm a gamma alpha metaGamma { ix: ixArgs.ix_term, csr: ixArgs.csr_term }
                ]
      , data:
          \typeBinding@(TypeBinding typeId _) constrItems meta gamma metaGamma ixArgs ->
            let
              eid = upwardIndexToEid ixArgs.ix
            in
              DOM.span
                ( [ selectableClassName "data definition" ixArgs.isSelected, Props._id eid ]
                    <> selectableProps ixArgs.ix
                    <> highlightableProps eid
                )
                [ renderTypeBinding typeBinding gamma metaGamma { ix: ixArgs.ix_typeBinding, csr: ixArgs.csr_typeBinding }
                , punctuation.space
                , punctuation.typedef
                , DOM.span'
                    $ Array.fromFoldable
                    $ mapWithIndex
                        ( \i constr ->
                            DOM.span'
                              [ indent { indented: true } metaGamma
                              , renderConstructorSeparator { indented: true } { ix_parentBlock: ixArgs.ix_parentBlock, ix_parentDef: ixArgs.ix, ix: (ixArgs.ix_constrSep_at i), csr: (ixArgs.csr_constrSep_at i) }
                              , indent { indented: true } metaGamma
                              , renderConstructor constr typeId gamma metaGamma { ix_parentBlock: ixArgs.ix_parentBlock, ix_parentDef: ixArgs.ix, ix: (ixArgs.ix_constr_at i), csr: (ixArgs.csr_constr_at i) }
                              ]
                        )
                        (fromItem <$> constrItems)
                , indent { indented: true } metaGamma
                , renderConstructorSeparator { indented: true } { ix_parentBlock: ixArgs.ix_parentBlock, ix_parentDef: ixArgs.ix, ix: (ixArgs.ix_constrSep_at (length constrItems)), csr: (ixArgs.csr_constrSep_at (length constrItems)) }
                ]
      }

  renderConstructorSeparator :: { indented :: Boolean } -> RecIndex.RecConstructorSeparator React.ReactElement
  renderConstructorSeparator renderArgs =
    RecIndex.recConstructorSeparator
      { separator:
          \ixArgs ->
            let
              eid = upwardIndexToEid ixArgs.ix
            in
              DOM.span
                ( [ selectableClassName ("constructorSeparator separator " <> indentedToClassName renderArgs) ixArgs.isSelected, Props._id eid ]
                    <> selectableProps ixArgs.ix
                    <> highlightableProps eid
                )
                -- [ DOM.span' $ if renderArgs.indented then [] else [ DOM.text "|" ] ]
                [ DOM.span' [ DOM.text "•" ] ]
      }

  renderConstructor :: RecIndex.RecConstructor React.ReactElement
  renderConstructor =
    RecIndex.recConstructor
      { constructor:
          \termBinding paramItems meta typeId gamma alpha metaGamma metaGamma_paramItem_at ixArgs ->
            let
              eid = upwardIndexToEid ixArgs.ix
            in
              DOM.span
                ( [ selectableClassName "constructor" ixArgs.isSelected, Props._id eid ]
                    <> selectableProps ixArgs.ix
                    <> highlightableProps eid
                )
                $ [ punctuation.alt
                  , punctuation.space
                  , renderTermBinding termBinding gamma metaGamma { ix: ixArgs.ix_termBinding, csr: ixArgs.csr_termBinding }
                  , DOM.span
                      (inertProps "constructor parameters")
                      [ intercalateHTML
                          [ punctuation.space ]
                          $ Array.fromFoldable
                          $ mapWithIndex
                              ( \i (param /\ meta) ->
                                  DOM.span'
                                    [ renderParameterSeparator meta { ix_parentBlock: ixArgs.ix_parentBlock, ix_parentDef: ixArgs.ix_parentDef, ix_parentConstr: ixArgs.ix, ix: ixArgs.ix_paramSep_at i, csr: ixArgs.csr_paramSep_at i }
                                    , renderParameter param gamma (metaGamma_paramItem_at i) { ix: ixArgs.ix_param_at i, csr: ixArgs.csr_param_at i }
                                    ]
                              )
                              paramItems
                      ]
                  , renderParameterSeparator { indented: any (snd >>> _.indented) paramItems } { ix_parentBlock: ixArgs.ix_parentBlock, ix_parentDef: ixArgs.ix_parentDef, ix_parentConstr: ixArgs.ix, ix: ixArgs.ix_paramSep_at (length paramItems), csr: ixArgs.csr_paramSep_at (length paramItems) }
                  ]
      }

  renderParameterSeparator :: forall r. { indented :: Boolean | r } -> RecIndex.RecParameterSeparator React.ReactElement
  renderParameterSeparator renderArgs =
    RecIndex.recParameterSeparator
      { separator:
          \ixArgs ->
            let
              eid = upwardIndexToEid ixArgs.ix
            in
              DOM.div
                ( [ selectableClassName ("parameterSeparator separator " <> indentedToClassName renderArgs) ixArgs.isSelected, Props._id eid ]
                    <> selectableProps ixArgs.ix
                    <> highlightableProps eid
                )
                -- [ DOM.span' $ if renderArgs.indented then [] else [ DOM.text "|" ] ]
                [ DOM.span' [ DOM.text "•" ] ]
      }

  {-
  renderParameterInsertor :: UpwardIndex -> React.ReactElement
  renderParameterInsertor ix =
    DOM.span
      [ Props.className "parameterInsertor insertor" ]
      [ DOM.text "+" ]
  -}
  {-
  renderParameterDeletor :: UpwardIndex -> React.ReactElement
  renderParameterDeletor ix =
    DOM.span
      [ Props.className "parameterDeletor deletor" ]
      [ DOM.text "-" ]
  -}
  renderType :: RecIndex.RecType React.ReactElement
  renderType =
    RecIndex.recType
      { arrow:
          \param beta meta gamma metaGamma ixArgs ->
            let
              eid = upwardIndexToEid ixArgs.ix
            in
              DOM.span
                ( [ selectableClassName "arrow type" ixArgs.isSelected, Props._id eid ] <> selectableProps ixArgs.ix
                    <> highlightableProps eid
                )
                $ [ renderParameter param gamma metaGamma { ix: ixArgs.ix_param, csr: ixArgs.csr_param }
                  {-, renderParameterDeletor ix-}
                  , punctuation.space
                  ]
                <> [ punctuation.arrow
                  , punctuation.space
                  , renderType beta gamma metaGamma { ix: ixArgs.ix_type, csr: ixArgs.csr_type }
                  ]
      , data:
          \typeId meta gamma metaGamma ixArgs ->
            let
              eid = upwardIndexToEid ixArgs.ix
            in
              DOM.span
                ( [ selectableClassName "data type typeId" ixArgs.isSelected, Props._id eid ] <> selectableProps ixArgs.ix
                    <> highlightableProps eid
                )
                [ DOM.span
                    [ Props.onClick \_ -> do
                        st <- React.getState this
                        let
                          holeId = freshHoleId unit

                          holeType = HoleType holeId Set.empty defaultHoleTypeMetadata
                        case chAtModule st.module_ emptyContext
                            ( SyntaxType
                                $ ArrowType (Parameter holeType defaultParameterMetadata)
                                    (DataType typeId meta)
                                    defaultArrowTypeMetadata
                            )
                            (ChangeTypeChange (InsertArg holeType))
                            (toDownwardIndex ixArgs.ix) of
                          Just (module' /\ ix' /\ holeSub) -> do
                            let
                              module'' = subModule holeSub module'
                            React.setState this
                              st
                                { module_ = module''
                                , ix_cursor = ix'
                                }
                          Nothing -> undefined
                    ]
                    [ DOM.text "enArrow" ]
                , renderTypeId typeId metaGamma
                ]
      , hole:
          \holeId wkn meta gamma metaGamma ixArgs ->
            let
              eid = upwardIndexToEid ixArgs.ix
            in
              DOM.span
                ( [ selectableClassName "hole type" ixArgs.isSelected, Props._id eid ] <> selectableProps ixArgs.ix
                    <> highlightableProps eid
                )
                [ DOM.text "?" ]
      , proxyHole:
          \holeId gamma metaGamma ixArgs ->
            let
              eid = upwardIndexToEid ixArgs.ix
            in
              DOM.span
                ( [ selectableClassName "proxy hole type" ixArgs.isSelected ] <> selectableProps ixArgs.ix
                    <> highlightableProps eid
                )
                [ DOM.text "?" ]
      }

  renderType' :: RecMetaContext.RecType React.ReactElement
  renderType' =
    RecMetaContext.recType
      { arrow:
          \param beta meta gamma metaGamma ->
            DOM.span
              (inertProps "arrow type")
              $ [ renderParameter' param gamma metaGamma
                , punctuation.space
                ]
              <> [ punctuation.arrow
                , punctuation.space
                , renderType' beta gamma metaGamma
                ]
      , data:
          \typeId meta gamma metaGamma ->
            DOM.span
              (inertProps "data type")
              [ renderTypeId typeId metaGamma ]
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
  renderTerm term_prt gamma_prt type_prt metaGamma_prt ixArgs_prt =
    RecIndex.recTerm
      { lambda:
          \termId block meta gamma param beta metaGamma ixArgs ->
            let
              eid = upwardIndexToEid ixArgs.ix
            in
              DOM.span
                ( [ selectableClassName "lambda term" ixArgs.isSelected
                  , Props._id eid
                  ]
                    <> selectableProps ixArgs.ix
                    <> highlightableProps eid
                )
                $ [ DOM.span (draggableProps (SyntaxTermId termId) ixArgs.ix_termId)
                      [ renderTermId termId gamma metaGamma { ix: ixArgs.ix_termId, csr: ixArgs.csr_termId } ]
                  ]
                <> [ punctuation.space
                  , punctuation.mapsto
                  , indentOrNothing meta metaGamma
                  , renderBlock block gamma beta metaGamma { ix: ixArgs.ix_block, csr: ixArgs.csr_block }
                  ]
      , neutral:
          \termId argItems meta gamma alpha metaGamma ixArgs ->
            let
              eid = upwardIndexToEid ixArgs.ix
            in
              DOM.span
                ( [ selectableClassName "neutral term" ixArgs.isSelected, Props._id eid ] <> selectableProps ixArgs.ix
                    <> highlightableProps eid
                )
                [ renderTermId termId gamma metaGamma { ix: ixArgs.ix_termId, csr: ixArgs.csr_termId }
                , renderArgItems argItems gamma alpha metaGamma { ix: ixArgs.ix_argItems, csr: ixArgs.csr_argItems }
                ]
      , match:
          \typeId a caseItems meta gamma alpha metaGamma constrIds ixArgs ->
            let
              eid = upwardIndexToEid ixArgs.ix
            in
              DOM.span
                ( [ selectableClassName "match term" ixArgs.isSelected, Props._id eid ] <> selectableProps ixArgs.ix
                    <> highlightableProps eid
                )
                [ keyword.match
                , punctuation.space
                , renderTerm a gamma (DataType typeId defaultDataTypeMetadata) metaGamma { ix: ixArgs.ix_term, csr: ixArgs.csr_term }
                , punctuation.space
                , keyword.with
                , DOM.span
                    (inertProps "match caseItems")
                    ( Array.fromFoldable
                        $ mapWithIndex
                            ( \i (case_ /\ meta) ->
                                DOM.span'
                                  [ indentOrSpace meta metaGamma
                                  , renderCase case_ typeId (index' constrIds i) gamma alpha metaGamma { ix_parentMatch: ixArgs.ix, ix: ixArgs.ix_case_at i, csr: ixArgs.csr_case_at i }
                                  ]
                            )
                            caseItems
                    )
                -- , DOM.span
                --     (inertProps "match caseItems")
                --     [ intercalateHTML [ indentOrSpace meta metaGamma, punctuation.alt, punctuation.space ]
                --         $ Array.fromFoldable
                --         $ mapWithIndex
                --             (\i case_ -> renderCase case_ typeId (index' constrIds i) gamma alpha metaGamma ix (ix_case_at i) (csr_case_at i))
                --             (fromItem <$> caseItems)
                --     ]
                ]
      , hole:
          \meta gamma alpha metaGamma ixArgs ->
            let
              eid = upwardIndexToEid ixArgs.ix
            in
              DOM.span
                ( [ selectableClassName "hole term" ixArgs.isSelected
                  , Props._id eid
                  ]
                    <> selectableProps ixArgs.ix
                    <> droppableProps gamma alpha ixArgs.ix
                    <> highlightableProps eid
                )
                [ renderType' alpha gamma metaGamma ]
      }
      term_prt
      gamma_prt
      type_prt
      metaGamma_prt
      ixArgs_prt

  renderArgItems :: RecIndex.RecArgItems React.ReactElement
  renderArgItems =
    RecIndex.recArgItems
      -- TODO: render separator
      { nil: \gamma alpha metaGamma -> DOM.span' []
      , cons:
          \(a /\ argItem_meta) argItems gamma (Parameter alpha _) beta metaGamma ixArgs ->
            DOM.span'
              $ [ punctuation.space ]
              <> [ let
                    unparenthesized = renderTerm a gamma alpha metaGamma { ix: ixArgs.ix_term, csr: ixArgs.csr_term }

                    parenthesized = DOM.span' [ punctuation.lparen, renderTerm a gamma alpha metaGamma { ix: ixArgs.ix_term, csr: ixArgs.csr_term }, punctuation.rparen ]
                  in
                    case a of
                      LambdaTerm _ _ _ -> parenthesized
                      HoleTerm _ -> parenthesized
                      MatchTerm _ _ _ _ -> parenthesized
                      NeutralTerm _ argItems' _ -> if null argItems' then unparenthesized else parenthesized
                ]
              <> if argItems == Nil then
                  []
                else
                  [ punctuation.space
                  , renderArgItems argItems gamma beta metaGamma { ix: ixArgs.ix_argItems, csr: ixArgs.csr_argItems }
                  ]
      }

  renderCase :: RecIndex.RecCase React.ReactElement
  renderCase case_prt typeId_prt termId_prt gamma_prt alpha_prt metaGamma_prt ixArgs_prt =
    RecIndex.recCase
      { case_:
          \termIdItems block meta typeId constrId gamma alpha metaGamma ixArgs ->
            let
              eid = upwardIndexToEid ixArgs.ix
            in
              DOM.span
                ( [ selectableClassName "case" ixArgs.isSelected, Props._id eid ] <> selectableProps ixArgs.ix
                    <> highlightableProps eid
                )
                [ punctuation.alt
                , punctuation.space
                , renderTermId' constrId metaGamma
                , DOM.span
                    (inertProps "case termIdItems")
                    [ DOM.span'
                        $ Array.fromFoldable
                        $ mapWithIndex
                            ( \i termId ->
                                DOM.span'
                                  [ punctuation.space
                                  , DOM.span
                                      (draggableProps (SyntaxTermId termId) (ixArgs.ix_termId_at i))
                                      [ renderTermId termId gamma metaGamma { ix: (ixArgs.ix_termId_at i), csr: (ixArgs.csr_termId_at i) } ]
                                  ]
                            )
                            (fromItem <$> termIdItems)
                    ]
                , punctuation.space
                , punctuation.mapsto
                , indentOrNothing meta metaGamma
                , renderBlock block gamma alpha metaGamma { ix: ixArgs.ix_block, csr: ixArgs.csr_block }
                ]
      }
      case_prt
      typeId_prt
      termId_prt
      gamma_prt
      alpha_prt
      metaGamma_prt
      ixArgs_prt

  renderParameter :: RecIndex.RecParameter React.ReactElement
  renderParameter =
    RecIndex.recParameter
      { parameter:
          \alpha meta gamma metaGamma ixArgs ->
            let
              eid = upwardIndexToEid ixArgs.ix

              shadow_i = Map.lookup' meta.name metaGamma.termScope.shadows
            in
              DOM.span
                ( [ selectableClassName "parameter" ixArgs.isSelected, Props._id eid ] <> selectableProps ixArgs.ix
                    <> highlightableProps eid
                )
                -- [ renderType alpha gamma metaGamma ix_type csr_type ]
                [ punctuation.lparen
                , printTermName meta.name shadow_i
                , punctuation.space
                , punctuation.colon
                , punctuation.space
                , renderType alpha gamma metaGamma { ix: ixArgs.ix_type, csr: ixArgs.csr_type }
                , punctuation.rparen
                ]
      }

  renderParameter' :: RecMetaContext.RecParameter React.ReactElement
  renderParameter' =
    RecMetaContext.recParameter
      { parameter:
          \alpha meta gamma metaGamma ->
            let
              shadow_i = Map.lookup' meta.name metaGamma.termScope.shadows
            in
              DOM.span
                (inertProps "parameter")
                [ punctuation.lparen
                , printTermName meta.name shadow_i
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
          \typeId meta gamma metaGamma ixArgs ->
            let
              eid = upwardIndexToEid ixArgs.ix
            in
              DOM.span
                ( [ selectableClassName "typeBinding" ixArgs.isSelected, Props._id eid ] <> selectableProps ixArgs.ix
                    <> highlightableProps eid
                )
                [ renderTypeId typeId metaGamma ]
      }

  renderTypeId :: TypeId -> RecMetaContext.MetaContext -> React.ReactElement
  renderTypeId typeId metaGamma = DOM.span (inertProps "typeId") [ printTypeName typeName shadow_i ]
    where
    typeName = Map.lookup' typeId metaGamma.typeScope.names

    shadow_i = Map.lookup' typeId metaGamma.typeScope.shadowIndices

  printTypeName :: TypeName -> Int -> React.ReactElement
  printTypeName (TypeName name) shadow_i = DOM.pre [ Props.className "typeName" ] [ DOM.text label, printShadowIndex shadow_i ]
    where
    label = maybe "_" identity name

  renderTermBinding :: RecIndex.RecTermBinding React.ReactElement
  renderTermBinding =
    RecIndex.recTermBinding
      { termBinding:
          \termId meta gamma metaGamma ixArgs ->
            let
              eid = upwardIndexToEid ixArgs.ix

              termName = Map.lookup' termId metaGamma.termScope.names

              shadow_i = Map.lookup' termId metaGamma.termScope.shadowIndices
            in
              DOM.span
                ( [ selectableClassName "termBinding" ixArgs.isSelected, Props._id eid ]
                    <> draggableProps (SyntaxTermId termId) ixArgs.ix
                    <> selectableProps ixArgs.ix
                    <> highlightableProps eid
                )
                [ printTermName termName shadow_i ]
      }

  renderTermBinding' :: TermBinding -> RecMetaContext.MetaContext -> React.ReactElement
  renderTermBinding' (TermBinding termId meta) metaGamma =
    let
      termName = Map.lookup' termId metaGamma.termScope.names

      shadow_i = Map.lookup' termId metaGamma.termScope.shadowIndices
    in
      DOM.span (inertProps "termBinding")
        [ printTermName termName shadow_i ]

  renderTermId :: RecIndex.RecTermId React.ReactElement
  renderTermId =
    RecIndex.recTermId
      { termId:
          \termId gamma metaGamma ixArgs ->
            let
              eid = upwardIndexToEid ixArgs.ix

              termName = Map.lookup' termId metaGamma.termScope.names

              shadow_i = Map.lookup' termId metaGamma.termScope.shadowIndices
            in
              DOM.span
                ( [ selectableClassName "termId" ixArgs.isSelected, Props._id eid ]
                    <> selectableProps ixArgs.ix
                    <> highlightableProps eid
                )
                [ printTermName termName shadow_i ]
      }

  renderTermId' :: TermId -> RecMetaContext.MetaContext -> React.ReactElement
  renderTermId' termId metaGamma = DOM.span (inertProps "termId") [ printTermName termName shadow_i ]
    where
    termName = Map.lookup' termId metaGamma.termScope.names

    shadow_i = Map.lookup' termId metaGamma.termScope.shadowIndices

  printTermName :: TermName -> Int -> React.ReactElement
  printTermName (TermName name) shadow_i = DOM.pre [ Props.className "termName" ] [ DOM.text label, printShadowIndex shadow_i ]
    where
    label = maybe "_" identity name

  printShadowSuffix :: String -> React.ReactElement
  printShadowSuffix str = DOM.span [ Props.className "shadow-suffix" ] [ DOM.text str ]

  printShadowIndex :: Int -> React.ReactElement
  printShadowIndex i
    | i == 0 = DOM.span [ Props.className "shadow-index" ] []
    | otherwise = DOM.span [ Props.className "shadow-index" ] [ DOM.text (show i) ]

  highlightableProps :: String -> Array Props.Props
  highlightableProps eid =
    [ Prop.onMouseOver \event -> do
        stopPropagation event
        st <- React.getState this
        self <- getElementById eid
        -- if parent on top of stack, then unhighlight it
        -- highlight self
        -- push self to stack
        case st.outlineParents of
          Nil -> pure unit
          Cons parent _ -> unhighlight parent
        highlight self
        React.modifyState this \st -> st { outlineParents = Cons self st.outlineParents }
    , Prop.onMouseOut \event -> do
        stopPropagation event
        st <- React.getState this
        self <- getElementById eid
        -- unhighlight self
        -- pop self from stack
        -- if parent on top of stack, then highlight it
        unhighlight self
        outlineParents' <- case st.outlineParents of
          Nil -> Unsafe.error "tried to pop self, but outlineParents were empty"
          Cons _ Nil -> pure Nil
          Cons _ (Cons parent outlineParents') -> do
            highlight parent
            pure outlineParents'
        React.modifyState this \st -> st { outlineParents = outlineParents' }
    ]

  selectableProps :: UpwardIndex -> Array Props.Props
  selectableProps ix =
    [ Props.onClick \event -> do
        stopPropagation event
        Debug.traceM $ show (toDownwardIndex ix)
        React.modifyState this \st -> st { ix_cursor = toDownwardIndex ix }
    ]

  draggableProps :: Syntax -> UpwardIndex -> Array Props.Props
  draggableProps syn ix =
    [ Props.onMouseDown \event -> do
        stopPropagation event
        Debug.traceM $ "drag: " <> show syn
        React.modifyState this \st -> st { syntax_dragging = Just syn }
    ]

  droppableProps :: Context -> Type -> UpwardIndex -> Array Props.Props
  droppableProps gamma alpha ix =
    [ Props.onMouseUp \event -> do
        stopPropagation event
        React.modifyState this \st -> case st.syntax_dragging of
          Just syn -> case syn of
            SyntaxTermId termId ->
              Debug.trace ("drop: " <> show syn) \_ ->
                st
                  { module_ = toModule $ modifySyntaxAt (toDownwardIndex ix) (const $ SyntaxTerm $ NeutralTerm termId Nil defaultNeutralTermMetadata) (SyntaxModule st.module_)
                  , syntax_dragging = Nothing
                  }
            _ -> st -- TODO: other cases
          Nothing -> st
    ]

  selectableClassName :: String -> Boolean -> Props.Props
  selectableClassName title isSelected = Props.className $ title <> if isSelected then " selected" else ""

  inertProps :: String -> Array Props.Props
  inertProps title = [ Props.className title ]

highlight :: HTMLElement -> Effect Unit
highlight elem = do
  cls <- classList elem
  isSelected <- DOMTokenList.contains cls "selected"
  if isSelected then
    pure unit
  else
    DOMTokenList.add cls "highlighted"

unhighlight :: HTMLElement -> Effect Unit
unhighlight elem = do
  cls <- classList elem
  DOMTokenList.remove cls "highlighted"

-- highlight :: NativeEventTarget -> Effect Unit
-- highlight t = setNativeEventTargetProp t "style" "box-shadow: 0 0 0 1px purple; background-color: rgba(255, 255, 255, 0.2)"
-- unhighlight :: NativeEventTarget -> Effect Unit
-- unhighlight t = setNativeEventTargetProp t "style" ""
upwardIndexToEid :: UpwardIndex -> String
upwardIndexToEid ix = case unconsUpwardIndex ix of
  Nothing -> ""
  Just { step: IndexStep label i, ix' } -> show label <> "-" <> show i <> "--" <> upwardIndexToEid ix'

indentedToClassName :: forall r. { indented :: Boolean | r } -> String
indentedToClassName { indented } = if indented then "indented" else "nonindented"

nameKeys :: Set.Set String
nameKeys =
  Set.map (String.singleton <<< String.codePointFromChar) $ Set.fromFoldable
    $ ( Enum.enumFromTo 'a' 'z'
          <> Enum.enumFromTo 'A' 'Z'
          <> Enum.enumFromTo '0' '9'
          <> fromFoldable [ ' ', '!', '@', '#', '$', '%', '^', '&', '*', '-', '+', '_', '=', '|', '\\', ':', '\"', ';', '\'', '<', '>', ',', '.', '?', '/' ] ::
          List Char
      )
