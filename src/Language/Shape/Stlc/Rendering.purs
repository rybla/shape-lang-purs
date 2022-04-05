module Language.Shape.Stlc.Rendering where

import Data.Tuple.Nested
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.RenderingAux
import Language.Shape.Stlc.RenderingTypes
import Language.Shape.Stlc.Syntax
import Prelude
import Data.Array (concat, concatMap, elemIndex, fromFoldable, mapWithIndex)
import Data.Array.Unsafe as Array
import Data.Foldable (sequence_, traverse_)
import Data.List (List(..))
import Data.List.Unsafe as List
import Data.Map.Unsafe as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst)
import Debug as Debug
import Effect (Effect)
import Effect.Console as Console
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Language.Shape.Stlc.IndexMetadata (toggleIndentedMetadataAt)
import Language.Shape.Stlc.Initial as Initial
import Language.Shape.Stlc.Recursion.MetaContext (MetaContext, emptyMetaContext)
import Language.Shape.Stlc.Recursion.MetaContext as RecMeta
import Language.Shape.Stlc.Recursion.Transformation (CommonTypeTransformations, Transformation, TransformationInputs, defaultTransformationInputs)
import Language.Shape.Stlc.Recursion.Transformation as RecTrans
import Language.Shape.Stlc.Typing (emptyContext)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Union)
import React (ReactClass, ReactElement, ReactThis, component, getState, modifyState)
import React.DOM.Dynamic as DOM
import React.DOM.Props as Props
import React.SyntheticEvent as React
import Record as Record
import Undefined (undefined)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event, EventType(..), preventDefault)
import Web.Event.EventTarget (addEventListener, eventListener)
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
    , render: DOM.div' <<< render <$> getState this
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
    { ix_cursor: DownwardIndex List.Nil
    , module_: Initial.module_
    , outline_parents: List.Nil
    , syntax_dragging: Nothing
    , keyCallbacks_static: Map.empty -- TODO
    , keyCallbacks_dynamic: Map.empty
    }

  keyboardEventHandler :: Event -> Effect Unit
  keyboardEventHandler event = do
    -- preventDefault event 
    st <- getState this
    let
      key = eventKey event
    Debug.traceM $ "===[ Key Press: " <> key <> " ] ================================================"
    Debug.traceM $ "available keypress actions:"
    Debug.traceM $ "dynamic: " <> show ((fst <$> Map.toUnfoldable st.keyCallbacks_dynamic) :: Array String)
    Debug.traceM $ "static: " <> show ((fst <$> Map.toUnfoldable st.keyCallbacks_static) :: Array String)
    case Map.lookup key st.keyCallbacks_dynamic of
      Just eff -> do
        preventDefault event
        eff
      Nothing -> case Map.lookup key st.keyCallbacks_static of
        Just eff -> do
          preventDefault event
          eff
        Nothing -> pure unit

  render :: State -> ReactElements
  render st = renderModule st.module_ emptyContext emptyMetaContext { csr: Just st.ix_cursor, ix: UpwardIndex Nil }

  renderModule :: RecTrans.RecModule ReactElements
  renderModule =
    RecTrans.recModule
      { module_:
          \defItems meta gamma metaGamma ixArgs trans ->
            createNode "module"
              defaultNodeProps -- (nodePropsFromIxArgs ixArgs)
              $ renderDefinitionItems defItems gamma metaGamma { ix_parentBlock: ixArgs.ix, ix: ixArgs.ix_defItems, csr: ixArgs.csr_defItems }
      }

  renderBlock :: RecTrans.RecBlock ReactElements
  renderBlock =
    RecTrans.recBlock
      { block:
          \defItems term meta gamma alpha metaGamma ixArgs trans ->
            createNode "block"
              ((nodePropsFromIxArgs ixArgs) { isIndentable = true })
              $ renderDefinitionItems defItems gamma metaGamma { ix_parentBlock: ixArgs.ix, ix: ixArgs.ix_defItems, csr: ixArgs.csr_defItems }
              <> indentation meta metaGamma
              <> [ token.space ]
              <> renderTerm term gamma alpha metaGamma { ix: ixArgs.ix_term, csr: ixArgs.csr_term }
      }

  renderDefinitionItems :: RecTrans.RecDefinitionItems ReactElements
  renderDefinitionItems =
    RecTrans.recDefinitionItems
      { definitionItems:
          \defItems gamma metaGamma ixArgs transArgs ->
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

  renderDefinitionSeparator :: RecTrans.RecDefinitionSeparator ReactElements
  renderDefinitionSeparator =
    RecTrans.recDefinitionSeparator
      { separator:
          \ixArgs trans ->
            createNode "definition separator"
              (nodePropsFromIxArgs ixArgs)
              [ token.defSep ]
      }

  renderDefinition :: RecTrans.RecDefinition ReactElements
  renderDefinition =
    RecTrans.recDefinition
      { term:
          \termBinding type_ term meta gamma { metaGamma_self, metaGamma_children } ixArgs trans ->
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
          \typeBinding@(TypeBinding typeId _) constrItems meta gamma { metaGamma_self, metaGamma_children } ixArgs trans ->
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

  renderConstructor :: RecTrans.RecConstructor ReactElements
  renderConstructor =
    RecTrans.recConstructor
      { constructor:
          \termBinding paramItems meta typeId gamma alpha metaGamma metaGamma_param_at ixArgs trans ->
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

  renderConstructorSeparator :: RecTrans.RecConstructorSeparator ReactElements
  renderConstructorSeparator =
    RecTrans.recConstructorSeparator
      { separator:
          \ixArgs trans ->
            createNode "constructor separator" (nodePropsFromIxArgs ixArgs)
              [ token.constrSep ]
      }

  makeCommonTypeActions :: forall r. CommonTypeTransformations Poststate r -> Array Action
  makeCommonTypeActions trans = [] -- { label: Just "enArrow", trigger: Trigger_Click, transformation: trans.enArrow 

  renderType :: RecTrans.RecType ReactElements
  renderType =
    RecTrans.recType
      { arrow:
          \param beta meta gamma metaGamma ixArgs trans ->
            createNode "arrow type"
              ( (nodePropsFromIxArgs ixArgs)
                  { isIndentable = true
                  , actions = makeCommonTypeActions trans
                  }
              )
              $ concat [ renderParameter_Arrow param gamma metaGamma { ix: ixArgs.ix_param, csr: ixArgs.csr_param }, [ token.arrow_sep ], renderType beta gamma metaGamma { ix: ixArgs.ix_type, csr: ixArgs.csr_type } ]
      , data:
          \typeId meta gamma metaGamma ixArgs trans ->
            createNode "data type"
              ( (nodePropsFromIxArgs ixArgs)
                  { isIndentable = true
                  , actions = makeCommonTypeActions trans
                  }
              )
              $ printTypeId typeId metaGamma
      , hole:
          \holeId wkn meta gamma metaGamma ixArgs trans ->
            createNode "hole type"
              ( (nodePropsFromIxArgs ixArgs)
                  { isIndentable = true
                  , actions = makeCommonTypeActions trans
                  }
              )
                { actions = makeCommonTypeActions trans
                }
              $ printTypeHoleId holeId metaGamma
      , proxyHole: -- unsafeCrashWith "renderType.proxyHole: should never render a proxyHole"
          \holeId gamma metaGamma ->
            createNode "proxy hole type" defaultNodeProps
              $ printTypeHoleId holeId metaGamma
      }

  printType :: RecMeta.RecType ReactElements
  printType =
    RecMeta.recType
      { arrow:
          \param beta meta gamma metaGamma ->
            createNode "arrow type"
              defaultNodeProps
                { isIndentable = true }
              $ concat [ printParameter param gamma metaGamma, [ token.arrow_sep ], printType beta gamma metaGamma ]
      , data:
          \typeId meta gamma metaGamma ->
            createNode "data type" defaultNodeProps
              $ printTypeId typeId metaGamma
      , hole:
          \holeId wkn meta gamma metaGamma ->
            createNode "hole type" defaultNodeProps
              $ printTypeHoleId holeId metaGamma
      , proxyHole:
          \holeId gamma metaGamma ->
            createNode "proxy hole type" defaultNodeProps
              $ printTypeHoleId holeId metaGamma
      }

  renderTerm :: RecTrans.RecTerm ReactElements
  renderTerm =
    RecTrans.recTerm
      { lambda:
          \termId block meta gamma param beta metaGamma ixArgs trans ->
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
          \termId argItems meta gamma alpha metaGamma ixArgs trans ->
            createNode "neutral term"
              (nodePropsFromIxArgs ixArgs)
                { isIndentable = true }
              $ concat
                  [ renderTermId termId gamma metaGamma { ix: ixArgs.ix_termId, csr: ixArgs.csr_termId }
                  , renderArgItems argItems gamma alpha metaGamma { ix_parentNeutral: ixArgs.ix, ix: ixArgs.ix_argItems, csr: ixArgs.csr_argItems }
                  ]
      , match:
          \typeId term caseItems meta gamma alpha metaGamma constrIds ixArgs trans ->
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
          \meta gamma alpha metaGamma ixArgs trans ->
            createNode "hole term"
              (nodePropsFromIxArgs ixArgs)
                { isIndentable = true }
              $ printType alpha gamma metaGamma
      }

  renderArgItems :: RecTrans.RecArgItems ReactElements
  renderArgItems =
    RecTrans.recArgItems
      { nil:
          \gamma alpha metaGamma ixArgs trans ->
            createNode "nil argItems"
              (nodePropsFromIxArgs ixArgs)
              -- []
              [ token.argItems_end ]
      , cons:
          \(term /\ meta) argItems gamma param@(Parameter alpha _) beta metaGamma ixArgs trans ->
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
                  , if List.null argItems then [ token.space ] else []
                  , renderArgItems argItems gamma beta metaGamma { ix_parentNeutral: ixArgs.ix_parentNeutral, ix: ixArgs.ix_argItems, csr: ixArgs.csr_argItems }
                  ]
      }

  renderCase :: RecTrans.RecCase ReactElements
  renderCase =
    RecTrans.recCase
      { case_:
          \termIdItems block meta typeId constrId gamma alpha metaGamma ixArgs trans ->
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

  renderParameter_Constructor :: RecTrans.RecParameter ReactElements
  renderParameter_Constructor =
    RecTrans.recParameter
      { parameter:
          \alpha meta gamma { metaGamma_self, metaGamma_children } ixArgs trans ->
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

  renderParameter_Arrow :: RecTrans.RecParameter ReactElements
  renderParameter_Arrow =
    RecTrans.recParameter
      { parameter:
          \alpha meta gamma { metaGamma_self, metaGamma_children } ixArgs trans ->
            -- createNode "parameter"
            --   (nodePropsFromIxArgs ixArgs)
            --   $ renderType alpha gamma metaGamma_children { ix: ixArgs.ix_type, csr: ixArgs.csr_type }
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

  printParameter :: RecMeta.RecParameter ReactElements
  printParameter =
    RecMeta.recParameter
      { parameter:
          \alpha meta gamma { metaGamma_self, metaGamma_children } ->
            createNode "parameter"
              defaultNodeProps
              $ printType alpha gamma metaGamma_children
      }

  renderParameterSeparator :: RecTrans.RecParameterSeparator ReactElements
  renderParameterSeparator =
    RecTrans.recParameterSeparator
      { separator:
          \ixArgs trans ->
            createNode "parameter separator"
              (nodePropsFromIxArgs ixArgs)
              [ token.paramSep ]
      }

  renderTypeBinding :: RecTrans.RecTypeBinding ReactElements
  renderTypeBinding =
    RecTrans.recTypeBinding
      { typeBinding:
          \typeId meta gamma metaGamma ixArgs trans ->
            createNode "typeBinding"
              (nodePropsFromIxArgs ixArgs)
              $ printTypeId typeId metaGamma
      }

  renderTermBinding :: RecTrans.RecTermBinding ReactElements
  renderTermBinding =
    RecTrans.recTermBinding
      { termBinding:
          \termId meta gamma metaGamma ixArgs trans ->
            createNode "termBinding"
              (nodePropsFromIxArgs ixArgs)
              $ printTermId termId metaGamma
      }

  renderTermId :: RecTrans.RecTermId ReactElements
  renderTermId =
    RecTrans.recTermId
      { termId:
          \termId gamma metaGamma ixArgs trans ->
            createNode "termId"
              (nodePropsFromIxArgs ixArgs)
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
    createNode "typeId" defaultNodeProps
      $ printName
          (case Map.lookup' termId metaGamma.termScope.names of TermName name -> name)
          (Map.lookup' termId metaGamma.termScope.shadowIndices)

  printTermName :: TermName -> MetaContext -> ReactElements
  printTermName termName metaGamma =
    createNode "termName" defaultNodeProps
      $ printName
          (case termName of TermName name -> name)
          (Map.lookup' termName metaGamma.termScope.shadows)

  printName :: Name -> Int -> ReactElements
  printName name i =
    createNode "name" defaultNodeProps
      $ [ DOM.span [ Props.className "base" ] [ DOM.text $ maybe "~" identity name ] ]
      <> if i > 0 then [ DOM.span [ Props.className "shadow" ] [ DOM.text (show i) ] ] else []

  -- only for HoleType
  printTypeHoleId :: HoleId -> MetaContext -> ReactElements
  printTypeHoleId holeId metaGamma =
    createNode "holeId" defaultNodeProps
      [ DOM.text $ "?" <> show i ]
    where
    i = case List.elemIndex holeId (unsafePerformEffect $ Ref.read metaGamma.typeHoleIds) of
      Just i -> i
      Nothing -> unsafeCrashWith $ "printTypeHoleId: holeId was not registered: " <> show holeId

  createNode :: String -> NodeProps -> ReactElements -> ReactElements
  createNode label props els =
    [ DOM.span
        ( [ Props.className $ label <> if props.isSelected == Just true then " selected" else ""
          , Props.onClick \event -> do
              case props.ix /\ props.isSelected of
                -- selectable 
                Just ixUp /\ Just _ -> do
                  React.stopPropagation event
                  let
                    ixDw = toDownwardIndex ixUp
                  Debug.traceM $ "selecting ix: " <> show ixDw
                  modifyState this \st ->
                    st
                      { ix_cursor = ixDw
                      , keyCallbacks_dynamic =
                        if props.isEditable then
                          st.keyCallbacks_dynamic -- TODO: edit name 
                        else
                          Map.unions $ concat
                            $ [ if props.isIndentable then [ Map.singleton "Tab" do runToggleIndentedAt ixDw ] else []
                              , [ Map.fromFoldable
                                    $ concatMap
                                        ( \{ label, trigger, transformation } -> case trigger of
                                            Trigger_Keypress { key } -> [ key /\ runTransformation transformation defaultTransformationInputs ]
                                            _ -> []
                                        )
                                        props.actions
                                ]
                              ]
                      }
                -- nonselectable
                _ -> pure unit
          {-
          , Props.onClick \event ->
              traverse_
                ( \{ label, trigger, transformation } ->
                    when (trigger == Trigger_Click) do
                      Debug.traceM $ "Action.Click: " <> show label
                      modifyState this \st -> case transformation defaultTransformationInputs st of
                        Just st' -> st'
                        Nothing -> st
                )
                props.actions
          -}
          ]
        )
        els
    ]

  runTransformation :: Transformation Poststate -> TransformationInputs -> Effect Unit
  runTransformation trans inputs =
    modifyState this \st -> case trans inputs st of
      Just st' -> st'
      Nothing -> st

  runToggleIndentedAt :: DownwardIndex -> Effect Unit
  runToggleIndentedAt ix = do
    Debug.traceM $ "runToggleIndexAt: ix: " <> show ix
    modifyState this \st -> st { module_ = toModule $ toggleIndentedMetadataAt ix (SyntaxModule st.module_) }

defaultNodeProps :: NodeProps
defaultNodeProps =
  { ix: Nothing
  , isSelected: Nothing
  , actions: []
  , isIndentable: false
  , isEditable: false
  }

nodePropsFromIxArgs :: forall r. { ix :: UpwardIndex, isSelected :: Boolean | r } -> NodeProps
nodePropsFromIxArgs { ix, isSelected } = defaultNodeProps { ix = Just ix, isSelected = Just isSelected }

type NodeProps
  = { ix :: Maybe UpwardIndex
    , isSelected :: Maybe Boolean
    , actions :: Array Action
    , isIndentable :: Boolean
    , isEditable :: Boolean
    }

type Action
  = { label :: Maybe String
    , trigger :: Trigger
    , transformation :: Transformation Poststate
    }

data Trigger
  = Trigger_Drop
  | Trigger_Keypress { key :: String }
  | Trigger_Paste
  | Trigger_Hover
  | Trigger_Button

derive instance eqTrigger :: Eq Trigger

-- type Action = 
unsafeSubrecord :: forall row1 row2 row3. Union row1 row2 row3 => Record row3 -> Record row1
unsafeSubrecord = unsafeCoerce
