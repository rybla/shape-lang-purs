module Language.Shape.Stlc.Rendering where

import Data.Tuple.Nested
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.RenderingAux
import Language.Shape.Stlc.Syntax
import Prelude
import Data.Array (concat, concatMap, fromFoldable, mapWithIndex)
import Data.Array.Unsafe as Array
import Data.List (List(..))
import Data.List.Unsafe as List
import Data.Map.Unsafe as Map
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Console as Console
import Language.Shape.Stlc.Initial as Initial
import Language.Shape.Stlc.Recursion.MetaContext (MetaContext, emptyMetaContext)
import Language.Shape.Stlc.Recursion.MetaContext as RecMeta
import Language.Shape.Stlc.Recursion.Transformation (Transformation)
import Language.Shape.Stlc.Recursion.Transformation as RecTrans
import Language.Shape.Stlc.RenderingTypes (Given, Props, State)
import Language.Shape.Stlc.Typing (emptyContext)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Union)
import React (ReactClass, ReactElement, ReactThis, component, getState)
import React.DOM.Dynamic as DOM
import React.DOM.Props as Props
import Record as Record
import Undefined (undefined)
import Unsafe.Coerce (unsafeCoerce)

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
    }
  where
  state :: State
  state =
    { ix_cursor: DownwardIndex List.Nil
    , module_: Initial.module_
    , outline_parents: List.Nil
    , syntax_dragging: Nothing
    }

  render :: State -> ReactElements
  render st = renderModule st.module_ emptyContext emptyMetaContext { csr: Just st.ix_cursor, ix: UpwardIndex Nil }

  renderModule :: RecTrans.RecModule ReactElements
  renderModule =
    RecTrans.recModule
      { module_:
          \defItems meta gamma metaGamma ixArgs trans ->
            createNode "module"
              (nodePropsFromIxArgs ixArgs)
              $ renderDefinitionItems defItems gamma metaGamma { ix_parentBlock: ixArgs.ix, ix: ixArgs.ix_defItems, csr: ixArgs.csr_defItems }
      }

  renderBlock :: RecTrans.RecBlock ReactElements
  renderBlock =
    RecTrans.recBlock
      { block:
          \defItems term meta gamma alpha metaGamma ixArgs trans ->
            createNode "block"
              (nodePropsFromIxArgs ixArgs)
              $ renderDefinitionItems defItems gamma metaGamma { ix_parentBlock: ixArgs.ix, ix: ixArgs.ix_defItems, csr: ixArgs.csr_defItems }
              <> [ indent meta metaGamma ]
              <> renderTerm term gamma alpha metaGamma { ix: ixArgs.ix_term, csr: ixArgs.csr_term }
      }

  renderDefinitionItems :: RecTrans.RecDefinitionItems ReactElements
  renderDefinitionItems =
    RecTrans.recDefinitionItems
      { definitionItems:
          \defItems gamma metaGamma ixArgs transArgs ->
            createNode "definition item"
              defaultNodeProps
              $ concat
              $ mapWithIndex
                  ( \i (def /\ meta) ->
                      concat
                        [ [ indent { indented: true } metaGamma ]
                        , renderDefinitionSeparator { ix_parentBlock: ixArgs.ix_parentBlock, ix: ixArgs.ix_defSep_at i, csr: ixArgs.csr_defSep_at i }
                        , [ indent { indented: true } metaGamma ]
                        , renderDefinition def gamma metaGamma { ix_parentBlock: ixArgs.ix_parentBlock, ix: ixArgs.ix_def_at i, csr: ixArgs.csr_def_at i }
                        ]
                  )
              $ (fromFoldable defItems)
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
          \termBinding type_ term meta gamma metaGamma ixArgs trans ->
            createNode "term definition"
              (nodePropsFromIxArgs ixArgs)
              $ concat
                  [ [ indent { indented: true } metaGamma
                    , token.termDef_sig_head
                    ]
                  , renderTermBinding termBinding gamma metaGamma { ix: ixArgs.ix_termBinding, csr: ixArgs.csr_termBinding }
                  , [ token.termDef_sig_sep ]
                  , renderType type_ gamma metaGamma { ix: ixArgs.ix_type, csr: ixArgs.csr_type }
                  , [ indent { indented: true } metaGamma
                    , token.termDef_imp_head
                    ]
                  , renderTermBinding termBinding gamma metaGamma { ix: ixArgs.ix_termBinding, csr: ixArgs.csr_termBinding }
                  , [ token.termDef_imp_sep ]
                  , renderTerm term gamma type_ metaGamma { ix: ixArgs.ix_term, csr: ixArgs.csr_term }
                  ]
      , data:
          \typeBinding@(TypeBinding typeId _) constrItems meta gamma metaGamma ixArgs trans ->
            createNode "data definition" (nodePropsFromIxArgs ixArgs)
              $ concat
                  [ [ indent { indented: true } metaGamma
                    , token.dataDef_head
                    ]
                  , renderTypeBinding typeBinding gamma metaGamma { ix: ixArgs.ix_typeBinding, csr: ixArgs.csr_typeBinding }
                  , [ token.dataDef_sep ]
                  , concat
                      $ mapWithIndex
                          ( \i (constr /\ meta) ->
                              concat
                                [ [ indent meta metaGamma ]
                                , renderConstructorSeparator { ix_parentBlock: ixArgs.ix_parentBlock, ix_parentDef: ixArgs.ix, ix: ixArgs.ix_constrSep_at i, csr: ixArgs.csr_constrSep_at i }
                                , [ indent meta metaGamma ]
                                , renderConstructor constr typeId gamma metaGamma { ix_parentBlock: ixArgs.ix_parentBlock, ix_parentDef: ixArgs.ix, ix: ixArgs.ix_constr_at i, csr: ixArgs.csr_constr_at i }
                                ]
                          )
                          (fromFoldable constrItems)
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
                                , renderParameter param gamma metaGamma { ix: ixArgs.ix_param_at i, csr: ixArgs.csr_param_at i }
                                ]
                          )
                          (fromFoldable paramItems)
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

  renderType :: RecTrans.RecType ReactElements
  renderType =
    RecTrans.recType
      { arrow:
          \param beta meta gamma metaGamma ixArgs trans ->
            createNode "arrow type" (nodePropsFromIxArgs ixArgs)
              $ concat [ renderParameter param gamma metaGamma { ix: ixArgs.ix_param, csr: ixArgs.csr_param }, [ token.arrow_sep ], renderType beta gamma metaGamma { ix: ixArgs.ix_type, csr: ixArgs.csr_type } ]
      , data:
          \typeId meta gamma metaGamma ixArgs trans ->
            createNode "data type" (nodePropsFromIxArgs ixArgs)
              $ printTypeId typeId metaGamma
      , hole:
          \holeId wkn meta gamma metaGamma ixArgs trans ->
            createNode "hole type" (nodePropsFromIxArgs ixArgs)
              $ printHoleId holeId metaGamma
      , proxyHole: unsafeCrashWith "renderType.proxyHole: should never render a proxyHole"
      }

  printType :: RecMeta.RecType ReactElements
  printType =
    RecMeta.recType
      { arrow:
          \param beta meta gamma metaGamma ->
            createNode "arrow type" defaultNodeProps
              $ concat [ printParameter param gamma metaGamma, [ token.arrow_sep ], printType beta gamma metaGamma ]
      , data:
          \typeId meta gamma metaGamma ->
            createNode "data type" defaultNodeProps
              $ printTypeId typeId metaGamma
      , hole:
          \holeId wkn meta gamma metaGamma ->
            createNode "hole type" defaultNodeProps
              $ printHoleId holeId metaGamma
      , proxyHole:
          \holeId gamma metaGamma ->
            createNode "proxy hole type" defaultNodeProps
              $ printHoleId holeId metaGamma
      }

  renderTerm :: RecTrans.RecTerm ReactElements
  renderTerm =
    RecTrans.recTerm
      { lambda:
          \termId block meta gamma param beta metaGamma ixArgs trans ->
            createNode "lambda term"
              (nodePropsFromIxArgs ixArgs)
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
              $ concat
                  [ renderTermId termId gamma metaGamma { ix: ixArgs.ix_termId, csr: ixArgs.csr_termId }
                  , renderArgItems argItems gamma alpha metaGamma { ix_parentNeutral: ixArgs.ix, ix: ixArgs.ix_argItems, csr: ixArgs.csr_argItems }
                  ]
      , match:
          \typeId term caseItems meta gamma alpha metaGamma constrIds ixArgs trans ->
            createNode "match term"
              (nodePropsFromIxArgs ixArgs)
              $ concat
                  [ [ token.match_head ]
                  , renderTerm term gamma (mkData typeId) metaGamma { ix: ixArgs.ix_term, csr: ixArgs.csr_term }
                  , [ token.match_sep ]
                  , concat
                      $ mapWithIndex
                          ( \i (case_ /\ meta) ->
                              concat
                                [ [ indent meta metaGamma ]
                                , renderCase case_ typeId (List.index' constrIds i) gamma alpha metaGamma { ix_parentMatch: ixArgs.ix, ix: ixArgs.ix_case_at i, csr: ixArgs.csr_case_at i }
                                ]
                          )
                          (fromFoldable caseItems)
                  ]
      , hole:
          \meta gamma alpha metaGamma ixArgs trans ->
            createNode "hole term"
              (nodePropsFromIxArgs ixArgs)
              $ printType alpha gamma metaGamma
      }

  renderArgItems :: RecTrans.RecArgItems ReactElements
  renderArgItems =
    RecTrans.recArgItems
      { nil:
          \gamma alpha metaGamma ixArgs trans ->
            createNode "nil argItems"
              (nodePropsFromIxArgs ixArgs)
              [ token.argSep ]
      , cons:
          \(term /\ meta) argItems gamma param@(Parameter alpha _) beta metaGamma ixArgs trans ->
            createNode "cons argItems"
              (nodePropsFromIxArgs ixArgs)
              $ concat
                  [ [ token.argSep ]
                  , renderTerm term gamma alpha metaGamma { ix: ixArgs.ix_term, csr: ixArgs.csr_term }
                  , renderArgItems argItems gamma beta metaGamma { ix_parentNeutral: ixArgs.ix_parentNeutral, ix: ixArgs.ix_argItems, csr: ixArgs.csr_argItems }
                  ]
      }

  renderCase :: RecTrans.RecCase ReactElements
  renderCase =
    RecTrans.recCase
      { case_:
          \termIdItems block meta typeId constrId gamma alpha metaGamma ixArgs trans ->
            createNode "case"
              (nodePropsFromIxArgs ixArgs)
              $ concat
                  [ [ token.case_head ]
                  , concat
                      $ mapWithIndex
                          ( \i (termId /\ meta) ->
                              renderTermId termId gamma metaGamma { ix: ixArgs.ix_termId_at i, csr: ixArgs.csr_termId_at i }
                          )
                          (fromFoldable termIdItems)
                  , [ token.case_sep ]
                  , renderBlock block gamma alpha metaGamma { ix: ixArgs.ix_block, csr: ixArgs.csr_block }
                  ]
      }

  renderParameter :: RecTrans.RecParameter ReactElements
  renderParameter =
    RecTrans.recParameter
      { parameter:
          \alpha meta gamma metaGamma ixArgs trans ->
            createNode "parameter"
              (nodePropsFromIxArgs ixArgs)
              $ concat
                  [ [ token.lparen ]
                  , printTermName meta.name metaGamma
                  , [ token.param_sep ]
                  , renderType alpha gamma metaGamma { ix: ixArgs.ix_type, csr: ixArgs.csr_type }
                  , [ token.rparen ]
                  ]
      }

  printParameter :: RecMeta.RecParameter ReactElements
  printParameter =
    RecMeta.recParameter
      { parameter:
          \alpha meta gamma metaGamma ->
            createNode "parameter"
              defaultNodeProps
              $ concat
                  [ [ token.lparen ]
                  , printTermName meta.name metaGamma
                  , [ token.param_sep ]
                  , printType alpha gamma metaGamma
                  , [ token.rparen ]
                  ]
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
      $ [ DOM.span [ Props.className "base" ] [ DOM.text $ maybe "_" identity name ] ]
      <> if i > 0 then [ DOM.span [ Props.className "shadow" ] [ DOM.text (show i) ] ] else []

  -- only for HoleType
  printHoleId :: HoleId -> MetaContext -> ReactElements
  printHoleId holeId metaGamma =
    createNode "holeId" defaultNodeProps
      [ DOM.text (show holeId) ]

  createNode :: String -> NodeProps -> ReactElements -> ReactElements
  createNode label props els =
    [ DOM.span
        ( [ Props.className $ label <> if props.isSelected == Just true then " selected" else "" ]
        -- TODO: facilitate action triggers 
        )
        els
    ]

defaultNodeProps :: NodeProps
defaultNodeProps =
  { ix: Nothing
  , isSelected: Nothing
  , actions: []
  }

nodePropsFromIxArgs :: forall r. { ix :: UpwardIndex, isSelected :: Boolean | r } -> NodeProps
nodePropsFromIxArgs { ix, isSelected } = defaultNodeProps { ix = Just ix, isSelected = Just isSelected }

type NodeProps
  = { ix :: Maybe UpwardIndex
    , isSelected :: Maybe Boolean
    , actions :: Array Action
    }

type Action
  = { label :: Maybe String
    , trigger :: Trigger
    , transformation :: Transformation
    }

data Trigger
  = Trigger_Drop
  | Trigger_Keypress String
  | Trigger_Click
  | Trigger_Paste
  | Trigger_Hover

-- type Action = 
unsafeSubrecord :: forall row1 row2 row3. Union row1 row2 row3 => Record row3 -> Record row1
unsafeSubrecord = unsafeCoerce
