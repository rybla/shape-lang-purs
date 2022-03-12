module Language.Shape.Stlc.Rendering where

import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Typing
import Prelude
import Prim hiding (Type)
import Data.Array as Array
import Data.List (List)
import Data.List.Unsafe as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Language.Shape.Stlc.Metadata (defaultDataTypeMetadata)
import Language.Shape.Stlc.Recursion.Index (Cursor, checkCursorStep)
import Language.Shape.Stlc.Recursion.Index as RecIndex
import Language.Shape.Stlc.Recursion.MetaContext (MetaContext, emptyMetaContext)
import Language.Shape.Stlc.Recursion.MetaContext as RecMetaContext
import React as React
import React.DOM as ReactDOM
import React.DOM.Props as ReactProps
import Undefined (undefined)

type ProgramProps
  = {}

type ProgramState
  = { module_ :: Module
    , cursor :: Index
    }

type ProgramGiven
  = { state :: ProgramState
    , render :: Effect React.ReactElement
    }

programClass :: React.ReactClass ProgramProps
programClass = React.component "Program" programComponent

programComponent :: React.ReactThis ProgramProps ProgramState -> Effect ProgramGiven
programComponent this =
  pure
    { state
    , render: render <$> React.getState this
    }
  where
  state :: ProgramState
  state =
    { module_: undefined :: Module
    , cursor: []
    }

  render :: ProgramState -> React.ReactElement
  render st = renderModule st.module_ Map.empty emptyMetaContext [] (Just st.cursor)

  renderModule :: RecIndex.RecModule React.ReactElement
  renderModule =
    RecIndex.recModule
      { module_:
          \defs meta gamma metaGamma ix isSelected ix_def_at cursor_def_at ->
            ReactDOM.span'
              [ renderDefinitions defs gamma metaGamma ix ix_def_at cursor_def_at ]
      }

  renderBlock :: RecIndex.RecBlock React.ReactElement
  renderBlock = undefined

  renderDefinitions :: RecIndex.RecDefinitions React.ReactElement
  renderDefinitions =
    RecIndex.recDefinitions
      { definitions:
          \defs gamma metaGamma ix_mod ix_def_at cursor_def_at ->
            ReactDOM.span'
              (Array.fromFoldable $ List.mapWithIndex (\i def -> renderDefinition def gamma metaGamma ix_mod (ix_def_at i) (cursor_def_at i)) defs)
      }

  -- renderDefinitions defs gama metaGamma ix ix_def_at cursor_at =
  renderDefinition :: RecIndex.RecDefinition React.ReactElement
  renderDefinition =
    RecIndex.recDefinition
      { term:
          \termBinding alpha a meta gamma metaGamma ix_parent ix isSelected ix_termBinding cursor_termBinding ix_alpha cursor_alpha ix_a cursor_a ->
            ReactDOM.span'
              [ renderTermBinding termBinding gamma metaGamma ix_termBinding cursor_termBinding
              , renderType alpha gamma metaGamma ix_alpha cursor_alpha
              , renderTerm a gamma alpha metaGamma ix_a cursor_a
              ]
      , data:
          \typeBinding@(TypeBinding typeId _) constrs meta gamma metaGamma ix_parent ix isSelected ix_typeBinding cursor_typeBinding ix_constr_at cursor_constr_at ->
            ReactDOM.span'
              [ renderTypeBinding typeBinding gamma metaGamma ix_typeBinding cursor_typeBinding
              , ReactDOM.span'
                  (Array.fromFoldable $ List.mapWithIndex (\i constr -> renderConstructor constr typeId gamma metaGamma ix ix_parent (ix_constr_at i) (cursor_constr_at i)) constrs)
              ]
      }

  renderConstructor :: RecIndex.RecConstructor React.ReactElement
  renderConstructor =
    RecIndex.recConstructor
      { constructor:
          \termBinding prms meta typeId gamma metaGamma ix_parent ix_def ix isSelected ix_termBinding cursor_termBinding ix_prm_at cursor_prm_at ->
            ReactDOM.span'
              [ renderTermBinding termBinding gamma metaGamma ix_termBinding cursor_termBinding
              , ReactDOM.span'
                  (Array.fromFoldable $ List.mapWithIndex (\i prm -> renderParameter prm gamma metaGamma (ix_prm_at i) (cursor_prm_at i)) prms)
              , renderType' (DataType typeId defaultDataTypeMetadata) gamma metaGamma
              ]
      }

  renderType :: RecIndex.RecType React.ReactElement
  renderType =
    RecIndex.recType
      { arrow:
          \prm beta meta gamma metaGamma ix isSelected ix_prm cursor_prm ix_beta cursor_beta ->
            ReactDOM.span'
              [ renderParameter prm gamma metaGamma ix_prm cursor_prm
              , renderType beta gamma metaGamma ix_beta cursor_beta
              ]
      , data:
          \typeId meta gamma metaGamma ix isSelected ->
            ReactDOM.span'
              [ undefined ]
      , hole:
          \holeId wkn meta gamma metaGamma ix isSelected ->
            ReactDOM.span'
              [ ReactDOM.text "?" ]
      , proxyHole:
          \holeId gamma metaGamma ix isSelected ->
            ReactDOM.span'
              [ ReactDOM.text "?" ]
      }

  renderType' :: RecMetaContext.RecType React.ReactElement
  renderType' = RecMetaContext.recType undefined -- TODO: do after renderType

  renderTerm :: RecIndex.RecTerm React.ReactElement
  renderTerm =
    RecIndex.recTerm
      { lambda:
          \termId block meta gamma prm beta metaGamma ix isSelected ix_termId cursor_termId ix_block cursor_block ->
            ReactDOM.span'
              [ renderTermId termId gamma metaGamma ix_termId cursor_termId
              , renderBlock block gamma beta metaGamma ix_block cursor_block
              ]
      , neutral:
          \termId args meta gamma alpha metaGamma ix isSelected ix_termId cursor_termId ix_args cursor_args ->
            ReactDOM.span'
              [ renderTermId termId gamma metaGamma ix_termId cursor_termId
              , renderArgs args gamma alpha metaGamma ix_args cursor_args
              ]
      , match:
          \typeId a cases meta gamma alpha metaGamma constrIds ix isSelected ix_term cursor_term ix_case_at cursor_case_at ->
            ReactDOM.span'
              [ renderTerm a gamma (DataType typeId defaultDataTypeMetadata) metaGamma ix_term cursor_term
              , ReactDOM.span'
                  ( Array.fromFoldable
                      $ List.mapWithIndex
                          ( \i case_ ->
                              renderCase case_ typeId (List.index' constrIds i) gamma alpha metaGamma ix (ix_case_at i) (cursor_case_at i)
                          )
                          cases
                  )
              ]
      , hole: undefined
      }

  renderArgs :: RecIndex.RecArgs React.ReactElement
  renderArgs = undefined

  renderCase :: RecIndex.RecCase React.ReactElement
  renderCase =
    RecIndex.recCase
      { case_:
          \termIds term meta typeId constrId gamma alpha metaGamma ix_match ix isSelected ix_termId_at cursor_termId_at ix_term cursor_term ->
            ReactDOM.span'
              [ ReactDOM.span'
                  (Array.fromFoldable $ List.mapWithIndex (\i termId -> renderTermId termId gamma metaGamma (ix_termId_at i) (cursor_termId_at i)) termIds)
              , renderTerm term gamma alpha metaGamma ix_term cursor_term
              ]
      }

  renderParameter :: RecIndex.RecParameter React.ReactElement
  renderParameter =
    RecIndex.recParameter
      { parameter:
          \alpha meta gamma metaGamma ix isSelected ix_alpha cursor_alpha ->
            ReactDOM.span'
              [ renderType alpha gamma metaGamma ix_alpha cursor_alpha ] -- TODO: render meta.name::TermName
      }

  renderTypeBinding :: RecIndex.RecTypeBinding React.ReactElement
  renderTypeBinding =
    RecIndex.recTypeBinding
      { typeBinding:
          \typeId meta gamma metaGamma ix isSelected ->
            ReactDOM.span' [] -- TODO
      }

  renderTermBinding :: RecIndex.RecTermBinding React.ReactElement
  renderTermBinding =
    RecIndex.recTermBinding
      { termBinding:
          \termId meta gamma metaGamma ix isSelected ->
            ReactDOM.span' [] -- TODO
      }

  renderTermId :: RecIndex.RecTermId React.ReactElement
  renderTermId =
    RecIndex.recTermId
      { termId:
          \termId gamma metaGamma ix isSelected ->
            ReactDOM.span' [] -- TODO
      }
