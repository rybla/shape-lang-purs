module Language.Shape.Stlc.Rendering where

import Data.Tuple.Nested
import Language.Shape.Stlc.Actions
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.RenderingAux
import Language.Shape.Stlc.RenderingTypes
import Language.Shape.Stlc.Syntax
import Prelude
import Data.Array (concat, concatMap, elemIndex, filter, fromFoldable, mapWithIndex)
import Data.Array.Unsafe as Array
import Data.Foldable (sequence_, traverse_)
import Data.Hashable (hash)
import Data.List (List(..))
import Data.List.Unsafe as List
import Data.Map.Unsafe (Map)
import Data.Map.Unsafe as Map
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst)
import Data.Variant (Variant, inj)
import Data.Words (getword_4letter)
import Debug as Debug
import Effect (Effect)
import Effect.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Language.Shape.Stlc.IndexMetadata (toggleIndentedMetadataAt)
import Language.Shape.Stlc.IndexSyntax (lookupSyntaxAt, modifySyntaxAt)
import Language.Shape.Stlc.Initial as Initial
import Language.Shape.Stlc.Recursion.Index as RecIndex
import Language.Shape.Stlc.Recursion.MetaContext (MetaContext, emptyMetaContext)
import Language.Shape.Stlc.Recursion.MetaContext as RecMeta
import Language.Shape.Stlc.Typing (emptyContext, flattenArrowType)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Union)
import React (ReactClass, ReactElement, ReactThis, component, getState, modifyState)
import React as React
import React.DOM.Dynamic as DOM
import React.DOM.Props as Props
import React.SyntheticEvent (stopPropagation)
import Record as Record
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe (fromJust)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event, EventType(..), preventDefault)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLElement (classList)
import Web.HTML.Window (toEventTarget)

foreign import eventCode :: Event -> String

foreign import eventKey :: Event -> String

-- foreign import setNativeEventTargetProp :: forall a. NativeEventTarget -> String -> a -> Effect Unit
-- foreign import getClassName :: NativeEventTarget -> String
foreign import setHTMLElementField :: forall a. String -> a -> HTMLElement -> Effect Unit

foreign import getElementById :: String -> Effect HTMLElement

{-
Naming conventions:
  - render*: outputs an interactive element
  - print*: outputs an inert (non-interactive) element
-}
type ReactElements
  = Array ReactElement

programClass :: ReactClass Props
programClass = component "Program" programComponent

programComponent :: ReactThis Props State -> Effect Given
programComponent this =
  pure
    { state
    , render:
        do
          st <- getState this
          pure
            $ DOM.div
                [ Props.className "editor"
                , Props.onClick \event -> do
                    -- unselect by clicking outside of the program
                    stopPropagation event
                    React.modifyState this \st -> st { ix_cursor = DownwardIndex Nil }
                    runRefreshSelection this
                ]
                (render st)
    , componentDidMount:
        do
          Console.log "componentDidMount"
          win <- window
          listener <- eventListener keyboardEventHandler
          addEventListener (EventType "keydown") listener false (toEventTarget win)
    }
  where
  state :: State
  state =
    { module_: Initial.module_
    , ix_cursor: DownwardIndex List.Nil
    , changeHistory: List.Nil
    , elements_highlighted: List.Nil
    , syntax_dragging: Nothing
    , actions: []
    , actions_keymap: Map.empty
    , environment:
        { metaGamma: Nothing
        , gamma: Nothing
        , goal: Nothing
        }
    }

  keyboardEventHandler :: Event -> Effect Unit
  keyboardEventHandler event = do
    st <- getState this
    let
      key = eventKey event
    Debug.traceM $ "===[ KeyDown: " <> key <> " ] ================================================"
    case Map.lookup key st.actions_keymap of
      Just a -> do
        preventDefault event
        a.effect
      Nothing -> do 
        if key `Array.elem` [ "ArrowLeft", "ArrowRight", "ArrowUp", "ArrowDown" ] then do
          preventDefault event
          pure unit
        else do 
          modifyState this \st ->
            st
              { module_ =
                toModule
                  $ modifySyntaxAt
                      st.ix_cursor
                      ( case _ of
                          SyntaxTypeBinding (TypeBinding typeId meta) -> SyntaxTypeBinding (TypeBinding typeId meta { name = wrap (updateNameViaKey key (unwrap meta.name)) })
                          SyntaxTermBinding (TermBinding termId meta) -> SyntaxTermBinding (TermBinding termId meta { name = wrap (updateNameViaKey key (unwrap meta.name)) })
                          syn -> syn
                      )
                      (SyntaxModule st.module_)
              }

  render :: State -> ReactElements
  render (st) =
    renderPanel (st)
      <> renderModule st.module_ emptyContext emptyMetaContext { csr: Just st.ix_cursor, ix: UpwardIndex Nil }

  renderPanel :: State -> ReactElements
  renderPanel st =
    [ DOM.div [ Props.className "panel" ]
        $ renderEnvironment st
        <> renderPalette st
    ]

  renderEnvironment :: State -> ReactElements
  renderEnvironment (st) =
    [ DOM.div [ Props.className "environment" ]
        $ ( case st.environment.gamma /\ st.environment.metaGamma of
              Just gamma /\ Just metaGamma ->
                [ DOM.div [ Props.className "context" ] $ concat
                    -- TODO: ((\(typeId /\ termIds) -> concat [ printTypeId typeId metaGamma]) <$> Map.toUnfoldable gamma.constructors) <>
                    
                    $ ((\(termId /\ type_) -> concat [ printTermId termId metaGamma, [ token.termDef_sig_sep ], printType type_ gamma metaGamma ]) <$> Map.toUnfoldable gamma.types)
                ]
              _ -> []
          )
        <> ( case st.environment.gamma /\ st.environment.metaGamma /\ st.environment.goal of
              Just gamma /\ Just metaGamma /\ Just type_ ->
                [ DOM.div [ Props.className "goal" ] $ printType type_ gamma metaGamma
                ]
              _ -> []
          )
    ]

  renderPalette :: State -> ReactElements
  renderPalette (st) =
    [ DOM.div [ Props.className "palette" ]
        $ concatMap
            ( \{ label, trigger, effect } -> case label of
                Just str ->
                  [ DOM.button
                      [ Props.className "action"
                      , Props.onClick \event -> effect
                      ]
                      [ let
                          shortcut = case trigger of
                            Trigger_Keypress { key } -> "[" <> key <> "]"
                            _ -> ""
                        in
                          DOM.text $ shortcut <> " " <> str
                      ]
                  ]
                _ -> []
            )
            st.actions
    ]

  renderModule :: RecIndex.RecModule ReactElements
  renderModule =
    RecIndex.recModule
      { module_:
          \defItems meta gamma metaGamma ixArgs ->
            createNode "module"
              defaultNodeProps -- (nodePropsFromIxArgs ixArgs)
              $ renderDefinitionItems defItems gamma metaGamma { ix_parentBlock: ixArgs.ix, ix: ixArgs.ix_defItems, csr: ixArgs.csr_defItems }
      }

  renderBlock :: RecIndex.RecBlock ReactElements
  renderBlock =
    RecIndex.recBlock
      { block:
          \defItems term meta gamma alpha metaGamma ixArgs ->
            createNode "block"
              ((nodePropsFromIxArgs ixArgs) { isIndentable = true })
              $ renderDefinitionItems defItems gamma metaGamma { ix_parentBlock: ixArgs.ix, ix: ixArgs.ix_defItems, csr: ixArgs.csr_defItems }
              <> indentation meta metaGamma
              <> [ token.space ]
              <> renderTerm term gamma alpha metaGamma { ix: ixArgs.ix_term, csr: ixArgs.csr_term }
      }

  renderDefinitionItems :: RecIndex.RecDefinitionItems ReactElements
  renderDefinitionItems =
    RecIndex.recDefinitionItems
      { definitionItems:
          \defItems gamma metaGamma ixArgs ->
            createNode "definition items"
              defaultNodeProps
              $ concat
              $ [ concat
                    $ mapWithIndex
                        ( \i (def /\ meta) ->
                            concat
                              [ indentation { indented: true } metaGamma
                              , renderDefinitionSeparator { ix_parentBlock: ixArgs.ix_parentBlock, ix: ixArgs.ix_defSep_at i, csr: ixArgs.csr_defSep_at i }
                              , renderDefinition def gamma metaGamma { ix_parentBlock: ixArgs.ix_parentBlock, ix: ixArgs.ix_def_at i, csr: ixArgs.csr_def_at i }
                              ]
                        )
                    $ (fromFoldable defItems)
                , indentation { indented: not (List.null defItems) } metaGamma
                , renderDefinitionSeparator { ix_parentBlock: ixArgs.ix_parentBlock, ix: ixArgs.ix_defSep_at (List.length defItems), csr: ixArgs.csr_defSep_at (List.length defItems) }
                ]
      }

  renderDefinitionSeparator :: RecIndex.RecDefinitionSeparator ReactElements
  renderDefinitionSeparator =
    RecIndex.recDefinitionSeparator
      { separator:
          \ixArgs ->
            createNode "definition separator"
              (nodePropsFromIxArgs ixArgs)
              [ token.defSep ]
      }

  renderDefinition :: RecIndex.RecDefinition ReactElements
  renderDefinition =
    RecIndex.recDefinition
      { term:
          \termBinding type_ term meta gamma { metaGamma_self, metaGamma_children } ixArgs ->
            concat
              [ indentation { indented: true } metaGamma_self
              , createNode "term definition"
                  (nodePropsFromIxArgs ixArgs)
                  $ concat
                      [ [ token.termDef_sig_head ]
                      , renderTermBinding termBinding gamma metaGamma_children { ix: ixArgs.ix_termBinding, csr: ixArgs.csr_termBinding }
                      , [ token.termDef_sig_sep ]
                      , indentation { indented: meta.indented_type } metaGamma_children
                      , renderType type_ gamma metaGamma_children { ix: ixArgs.ix_type, csr: ixArgs.csr_type }
                      , indentation { indented: true } metaGamma_self
                      , [ token.termDef_imp_head ]
                      , renderTermBinding termBinding gamma metaGamma_children { ix: ixArgs.ix_termBinding, csr: ixArgs.csr_termBinding }
                      , [ token.termDef_imp_sep ]
                      , indentation { indented: meta.indented_term } metaGamma_children
                      , renderTerm term gamma type_ metaGamma_children { ix: ixArgs.ix_term, csr: ixArgs.csr_term }
                      ]
              ]
      , data:
          \typeBinding@(TypeBinding typeId _) constrItems meta gamma { metaGamma_self, metaGamma_children } ixArgs ->
            concat
              [ indentation { indented: true } metaGamma_self
              , createNode "data definition" (nodePropsFromIxArgs ixArgs)
                  $ concat
                      [ [ token.dataDef_head ]
                      , renderTypeBinding typeBinding gamma metaGamma_children { ix: ixArgs.ix_typeBinding, csr: ixArgs.csr_typeBinding }
                      , [ token.dataDef_sep ]
                      , concat
                          $ mapWithIndex
                              ( \i (constr /\ meta) ->
                                  concat
                                    [ indentation meta metaGamma_children
                                    , renderConstructorSeparator { ix_parentBlock: ixArgs.ix_parentBlock, ix_parentDef: ixArgs.ix, ix: ixArgs.ix_constrSep_at i, csr: ixArgs.csr_constrSep_at i }
                                    , indentation meta metaGamma_children
                                    , renderConstructor constr typeId gamma metaGamma_children { ix_parentBlock: ixArgs.ix_parentBlock, ix_parentDef: ixArgs.ix, ix: ixArgs.ix_constr_at i, csr: ixArgs.csr_constr_at i }
                                    ]
                              )
                              (fromFoldable constrItems)
                      , indentation { indented: true } metaGamma_children
                      , renderConstructorSeparator { ix_parentBlock: ixArgs.ix_parentBlock, ix_parentDef: ixArgs.ix, ix: ixArgs.ix_constrSep_at (List.length constrItems), csr: ixArgs.csr_constrSep_at (List.length constrItems) }
                      ]
              ]
      }

  renderConstructor :: RecIndex.RecConstructor ReactElements
  renderConstructor =
    RecIndex.recConstructor
      { constructor:
          \termBinding paramItems meta typeId gamma alpha metaGamma metaGamma_param_at ixArgs ->
            createNode "constructor" (nodePropsFromIxArgs ixArgs)
              $ concat
                  [ [ token.constr_head ]
                  , renderTermBinding termBinding gamma metaGamma { ix: ixArgs.ix_termBinding, csr: ixArgs.csr_termBinding }
                  , [ token.constr_sep ]
                  , concat
                      $ mapWithIndex
                          ( \i (param /\ meta) ->
                              concat
                                [ renderParameterSeparator { ix_parentBlock: ixArgs.ix_parentBlock, ix_parentDef: ixArgs.ix_parentDef, ix_parentConstr: ixArgs.ix, ix: ixArgs.ix_paramSep_at i, csr: ixArgs.csr_paramSep_at i }
                                , renderParameter_Constructor param gamma metaGamma { ix: ixArgs.ix_param_at i, csr: ixArgs.csr_param_at i }
                                ]
                          )
                          (fromFoldable paramItems)
                  , renderParameterSeparator { ix_parentBlock: ixArgs.ix_parentBlock, ix_parentDef: ixArgs.ix_parentDef, ix_parentConstr: ixArgs.ix, ix: ixArgs.ix_paramSep_at (List.length paramItems), csr: ixArgs.csr_paramSep_at (List.length paramItems) }
                  ]
      }

  renderConstructorSeparator :: RecIndex.RecConstructorSeparator ReactElements
  renderConstructorSeparator =
    RecIndex.recConstructorSeparator
      { separator:
          \ixArgs ->
            createNode "constructor separator" (nodePropsFromIxArgs ixArgs)
              [ token.constrSep ]
      }

  renderType :: RecIndex.RecType ReactElements
  renderType =
    RecIndex.recType
      { arrow:
          \param beta meta gamma metaGamma ixArgs ->
            createNode "arrow type"
              ( (nodePropsFromIxArgs ixArgs)
                  { isIndentable = true }
              )
              -- * infixed
              
              $ concat
                  [ renderParameter_Arrow param gamma metaGamma { ix: ixArgs.ix_param, csr: ixArgs.csr_param }
                  , [ token.arrow_infix_sep ]
                  , renderType beta gamma metaGamma { ix: ixArgs.ix_type, csr: ixArgs.csr_type }
                  ]
      -- * prefixed
      -- $ concat [ [ token.lparen, token.arrow_infix_head ], renderParameter_Arrow param gamma metaGamma { ix: ixArgs.ix_param, csr: ixArgs.csr_param }, [ token.arrow_prefix_sep ], renderType beta gamma metaGamma { ix: ixArgs.ix_type, csr: ixArgs.csr_type }, [ token.rparen ] ]
      , data:
          \typeId meta gamma metaGamma ixArgs ->
            createNode "data type"
              ( (nodePropsFromIxArgs ixArgs)
                  { isIndentable = true }
              )
              $ printTypeId typeId metaGamma
      , hole:
          \holeId wkn meta gamma metaGamma ixArgs ->
            createNode "hole type"
              ( (nodePropsFromIxArgs ixArgs)
                  { isIndentable = true }
              )
              [ DOM.span [ Props.className "liner" ]
                  $ printTypeHoleId holeId metaGamma
              ]
      , proxyHole: -- unsafeCrashWith "renderType.proxyHole: should never render a proxyHole"
          \holeId gamma metaGamma ->
            createNode "proxy hole type" defaultNodeProps
              [ DOM.span [ Props.className "liner" ]
                  $ printTypeHoleId holeId metaGamma
              ]
      }

  printType :: RecMeta.RecType ReactElements
  printType =
    RecMeta.recType
      { arrow:
          \param beta meta gamma metaGamma ->
            createNode "arrow type"
              defaultNodeProps
                { isIndentable = true }
              $ concat [ printParameter param gamma metaGamma, [ token.arrow_infix_sep ], printType beta gamma metaGamma ]
      , data:
          \typeId meta gamma metaGamma ->
            createNode "data type" defaultNodeProps
              $ printTypeId typeId metaGamma
      , hole:
          \holeId wkn meta gamma metaGamma ->
            createNode "hole type" defaultNodeProps
              [ DOM.span [ Props.className "liner" ]
                  $ printTypeHoleId holeId metaGamma
              ]
      , proxyHole:
          \holeId gamma metaGamma ->
            createNode "proxy hole type" defaultNodeProps
              [ DOM.span [ Props.className "liner" ]
                  $ printTypeHoleId holeId metaGamma
              ]
      }

  renderTerm :: RecIndex.RecTerm ReactElements
  renderTerm =
    RecIndex.recTerm
      { lambda:
          \termId block meta gamma param beta metaGamma ixArgs ->
            createNode "lambda term"
              ( (nodePropsFromIxArgs ixArgs)
                  { isIndentable = true }
              )
              $ concat
                  [ [ token.lambda_head ]
                  , renderTermId termId gamma metaGamma { ix: ixArgs.ix_termId, csr: ixArgs.csr_termId }
                  , [ token.lambda_sep ]
                  , renderBlock block gamma beta metaGamma { ix: ixArgs.ix_block, csr: ixArgs.csr_block }
                  ]
      , neutral:
          \termId argItems meta gamma alpha metaGamma ixArgs ->
            createNode "neutral term"
              (nodePropsFromIxArgs ixArgs)
                { isIndentable = true }
              $ concat
                  [ renderTermId termId gamma metaGamma { ix: ixArgs.ix_termId, csr: ixArgs.csr_termId }
                  , renderArgItems argItems gamma alpha metaGamma { ix_parentNeutral: ixArgs.ix, ix: ixArgs.ix_argItems, csr: ixArgs.csr_argItems }
                  ]
      , match:
          \typeId term caseItems meta gamma alpha metaGamma constrIds ixArgs ->
            createNode "match term"
              (nodePropsFromIxArgs ixArgs)
                { isIndentable = true }
              $ concat
                  [ [ token.match_head ]
                  , renderTerm term gamma (mkData typeId) metaGamma { ix: ixArgs.ix_term, csr: ixArgs.csr_term }
                  , [ token.match_sep ]
                  , concat
                      $ mapWithIndex
                          ( \i (case_ /\ meta) ->
                              -- how to make these indentable 
                              concat
                                [ indentation meta metaGamma
                                , renderCase case_ typeId (List.index' constrIds i) gamma alpha metaGamma { ix_parentMatch: ixArgs.ix, ix: ixArgs.ix_case_at i, csr: ixArgs.csr_case_at i }
                                ]
                          )
                          (fromFoldable caseItems)
                  ]
      , hole:
          \meta gamma alpha metaGamma ixArgs ->
            createNode "hole term"
              (nodePropsFromIxArgs ixArgs)
                { isIndentable = true }
              $ concat
                  [ [ token.holeTerm_head ]
                  , printType alpha gamma metaGamma
                  ]
      }

  renderArgItems :: RecIndex.RecArgItems ReactElements
  renderArgItems =
    RecIndex.recArgItems
      { nil:
          \gamma alpha metaGamma ixArgs ->
            createNode "nil argItems"
              (nodePropsFromIxArgs ixArgs)
              -- only display token.argItems_end if function is partialy applied
              let
                alphas /\ beta = flattenArrowType alpha
              in
                if not (List.null alphas) then [ token.argItems_end ] else []
      , cons:
          \(term /\ meta) argItems gamma param@(Parameter alpha _) beta metaGamma ixArgs ->
            createNode "cons argItems"
              defaultNodeProps
              $ concat
                  [ [ token.space ]
                  , let
                      paren = \_ -> [ token.lparen ] <> renderTerm term gamma alpha metaGamma { ix: ixArgs.ix_term, csr: ixArgs.csr_term } <> [ token.rparen ]

                      nonparen = \_ -> renderTerm term gamma alpha metaGamma { ix: ixArgs.ix_term, csr: ixArgs.csr_term }
                    in
                      case term of
                        LambdaTerm _ _ _ -> paren unit
                        NeutralTerm _ Nil _ -> nonparen unit
                        NeutralTerm _ _ _ -> paren unit
                        HoleTerm _ -> nonparen unit
                        MatchTerm _ _ _ _ -> paren unit
                  , let
                      alphas /\ beta = flattenArrowType beta
                    in
                      -- only display space (before the impending token.argItems_end) if function is partialy applied 
                      if List.null argItems && not (List.null alphas) then [ token.space ] else []
                  , renderArgItems argItems gamma beta metaGamma { ix_parentNeutral: ixArgs.ix_parentNeutral, ix: ixArgs.ix_argItems, csr: ixArgs.csr_argItems }
                  ]
      }

  renderCase :: RecIndex.RecCase ReactElements
  renderCase =
    RecIndex.recCase
      { case_:
          \termIdItems block meta typeId constrId gamma alpha metaGamma ixArgs ->
            createNode "case"
              ( (nodePropsFromIxArgs ixArgs)
                  { isIndentable = true }
              )
              $ concat
                  [ [ token.case_head ]
                  , printTermId constrId metaGamma
                  , concat
                      $ mapWithIndex
                          ( \i (termId /\ meta) ->
                              concat
                                [ [ token.space ]
                                , renderTermId termId gamma metaGamma { ix: ixArgs.ix_termId_at i, csr: ixArgs.csr_termId_at i }
                                ]
                          )
                          (fromFoldable termIdItems)
                  , [ token.case_sep ]
                  , renderBlock block gamma alpha metaGamma { ix: ixArgs.ix_block, csr: ixArgs.csr_block }
                  , [ token.space ]
                  ]
      }

  renderParameter_Constructor :: RecIndex.RecParameter ReactElements
  renderParameter_Constructor =
    RecIndex.recParameter
      { parameter:
          \alpha meta gamma { metaGamma_self, metaGamma_children } ixArgs ->
            createNode "parameter"
              ( (nodePropsFromIxArgs ixArgs)
                  { isIndentable = true }
              )
              $ concat
                  [ [ token.lparen ]
                  , printTermName meta.name metaGamma_children
                  , [ token.param_sep ]
                  , renderType alpha gamma metaGamma_children { ix: ixArgs.ix_type, csr: ixArgs.csr_type }
                  , [ token.rparen ]
                  ]
      }

  renderParameter_Arrow :: RecIndex.RecParameter ReactElements
  renderParameter_Arrow =
    RecIndex.recParameter
      { parameter:
          \alpha meta gamma { metaGamma_self, metaGamma_children } ixArgs ->
            -- * prefixed and unnamed
            createNode "parameter"
              (nodePropsFromIxArgs ixArgs) case alpha of
              ArrowType _ _ _ -> concat [ [ token.lparen ], renderType alpha gamma metaGamma_children { ix: ixArgs.ix_type, csr: ixArgs.csr_type }, [ token.rparen ] ]
              _ -> renderType alpha gamma metaGamma_children { ix: ixArgs.ix_type, csr: ixArgs.csr_type }
      -- -- * infixed & named
      -- createNode "parameter"
      --   ( (nodePropsFromIxArgs ixArgs)
      --       { isIndentable = true }
      --   )
      --   $ concat
      --       [ [ token.lparen ]
      --       , printTermName meta.name metaGamma_children
      --       , [ token.param_sep ]
      --       , renderType alpha gamma metaGamma_children { ix: ixArgs.ix_type, csr: ixArgs.csr_type }
      --       , [ token.rparen ]
      --       ]
      }

  printParameter :: RecMeta.RecParameter ReactElements
  printParameter =
    RecMeta.recParameter
      { parameter:
          \alpha meta gamma { metaGamma_self, metaGamma_children } ->
            createNode "parameter"
              defaultNodeProps
              $ printType alpha gamma metaGamma_children
      }

  renderParameterSeparator :: RecIndex.RecParameterSeparator ReactElements
  renderParameterSeparator =
    RecIndex.recParameterSeparator
      { separator:
          \ixArgs ->
            createNode "parameter separator"
              (nodePropsFromIxArgs ixArgs)
              [ token.paramSep ]
      }

  renderTypeBinding :: RecIndex.RecTypeBinding ReactElements
  renderTypeBinding =
    RecIndex.recTypeBinding
      { typeBinding:
          \typeId meta gamma metaGamma ixArgs ->
            createNode "typeBinding"
              (nodePropsFromIxArgs ixArgs)
              $ printTypeId typeId metaGamma
      }

  renderTermBinding :: RecIndex.RecTermBinding ReactElements
  renderTermBinding =
    RecIndex.recTermBinding
      { termBinding:
          \termId meta gamma metaGamma ixArgs ->
            createNode "termBinding"
              (nodePropsFromIxArgs ixArgs)
              $ printTermId termId metaGamma
      }

  renderTermId :: RecIndex.RecTermId ReactElements
  renderTermId =
    RecIndex.recTermId
      { termId:
          \termId gamma metaGamma ixArgs ->
            createNode "termId"
              -- (nodePropsFromIxArgs ixArgs)
              defaultNodeProps
              $ printTermId termId metaGamma
      }

  printTypeId :: TypeId -> MetaContext -> ReactElements
  printTypeId typeId metaGamma =
    createNode "typeId" defaultNodeProps
      $ printName
          (case Map.lookup' typeId metaGamma.typeScope.names of TypeName name -> name)
          (Map.lookup' typeId metaGamma.typeScope.shadowIndices)

  printTermId :: TermId -> MetaContext -> ReactElements
  printTermId termId metaGamma =
    createNode "termId" defaultNodeProps
      $ printName
          (case Map.lookup' termId metaGamma.termScope.names of TermName name -> name)
          (Map.lookup' termId metaGamma.termScope.shadowIndices)

  printTermName :: TermName -> MetaContext -> ReactElements
  printTermName termName metaGamma =
    createNode "termName" defaultNodeProps
      $ printName
          (case termName of TermName name -> name)
          (Map.lookup' termName metaGamma.termScope.shadows - 1) -- TODO: shouldn't need to subtract 1, but for some reason I do for parameters (only place printTermName is used)

  printName :: String -> Int -> ReactElements
  printName name i =
    createNode "name" defaultNodeProps
      $ [ DOM.span [ Props.className "base" ] [ DOM.text name ] ]
      <> if i > 0 then [ DOM.span [ Props.className "shadow" ] [ DOM.text (show i) ] ] else []

  -- only for HoleType
  printTypeHoleId :: HoleId -> MetaContext -> ReactElements
  printTypeHoleId holeId@(HoleId uuid) metaGamma =
    createNode "holeId" defaultNodeProps
      [ DOM.text $ "?" <> show i ]
    where
    i = (List.length metaGamma.holes - 1) - (fromJust $ List.elemIndex holeId metaGamma.holes)

  -- where
  -- s = show uuid
  -- i = hash s
  -- w = getword_4letter i
  -- where
  -- i = case List.elemIndex holeId (unsafePerformEffect $ Ref.read metaGamma.typeHoleIds) of
  --   Just i -> i
  --   Nothing -> unsafeCrashWith $ "printTypeHoleId: holeId was not registered: " <> show holeId
  createNode :: String -> NodeProps -> ReactElements -> ReactElements
  createNode label props els = case isSelectable props of
    -- selectable
    Just (ix /\ isSelected) ->
      let
        _id = fromUpwardIndexToElementId <$> props.ix
      in
        [ DOM.span
            [ Props.className $ label <> if isSelected then " selected" else ""
            , Props._id $ fromJust _id
            , Props.onClick \event -> do
                stopPropagation event
                runSelectHere props this
            , Props.onMouseOver \event -> do
                stopPropagation event
                st <- React.getState this
                elem <- getElementById (fromJust $ _id)
                -- if parent on top of stack, then unhighlight it
                case st.elements_highlighted of
                  Nil -> pure unit
                  Cons elemParent _ -> unhighlight elemParent
                -- highlight self
                highlight elem
                -- push self to stack
                React.modifyState this \st -> st { elements_highlighted = Cons elem st.elements_highlighted }
            , Props.onMouseOut \event -> do
                stopPropagation event
                st <- React.getState this
                elem <- getElementById (fromJust $ _id)
                -- unhighlight self
                -- pop self from stack
                -- if parent on top of stack, then highlight it
                unhighlight elem
                elements_highlighted' <- case st.elements_highlighted of
                  Nil -> unsafeCrashWith "expected there to be a highlighted parent, but there isn't one"
                  Cons _ Nil -> pure Nil
                  Cons _ (Cons elemParent elements_highlighted') -> do
                    highlight elemParent
                    pure elements_highlighted'
                React.modifyState this \st -> st { elements_highlighted = elements_highlighted' }
            ]
            els
        ]
    -- not selectable
    Nothing ->
      [ DOM.span
          [ Props.className $ label ]
          els
      ]

updateNameViaKey :: String -> String -> String
updateNameViaKey key name =
  if key `Array.elem` label_keys then
    name <> key
  else if key == "Backspace" then
    String.take (String.length name - 1) name
  else
    name

-- keys that are allowed in labels (names of types and terms)
label_keys :: Array String
label_keys = String.split (Pattern " ") "a b c d e f g h i j k l m n o p q r s t u v w x y z ! @ # $ % ^ & * ( ) _ + { } : \" < > ? 1 2 3 4 5 6 7 8 9 0 - = [ ] ; ' . , /"
