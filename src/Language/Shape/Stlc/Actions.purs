module Language.Shape.Stlc.Actions where

import Data.Array
import Data.Tuple
import Data.Tuple.Nested
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.RenderingTypes
import Language.Shape.Stlc.Syntax
import Prelude

import Data.List.Unsafe (List(..))
import Data.List.Unsafe as List
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Debug as Debug
import Effect (Effect)
import Effect.Console as Console
import Language.Shape.Stlc.IndexMetadata (toggleIndentedMetadataAt)
import Language.Shape.Stlc.Recursion.MetaContext (MetaContext, emptyMetaContext)
import Language.Shape.Stlc.Recursion.Transformation (CommonDefinitionTransformations, CommonTermTransformations, CommonTypeTransformations, Transformation, TransformationInputs, defaultTransformationInputs)
import Language.Shape.Stlc.Recursion.Transformation as RecTrans
import Language.Shape.Stlc.Typing (Context, emptyContext)
import React (getState, modifyState)
import Undefined (undefined)
import Unsafe (fromJust)
import Web.DOM.DOMTokenList as DOMTokenList
import Web.HTML (HTMLElement)
import Web.HTML.HTMLElement (classList)

getActionsInModule :: RecTrans.RecModule (This -> Actions)
getActionsInModule =
  RecTrans.recModule
    { module_:
        \defItems meta gamma metaGamma ixArgs trans this ->
          if ixArgs.isSelected then
            []
          else
            getActionsInDefinitionItems defItems gamma metaGamma { ix_parentBlock: ixArgs.ix, ix: ixArgs.ix_defItems, csr: ixArgs.csr_defItems } this
    }

getActionsInBlock :: RecTrans.RecBlock (This -> Actions)
getActionsInBlock =
  RecTrans.recBlock
    { block:
        \defItems term meta gamma alpha metaGamma ixArgs trans this ->
          if ixArgs.isSelected then
            []
          else
            concat
              [ getActionsInDefinitionItems defItems gamma metaGamma { ix_parentBlock: ixArgs.ix, ix: ixArgs.ix_defItems, csr: ixArgs.csr_defItems } this
              , getActionsInTerm term gamma alpha metaGamma { ix: ixArgs.ix_term, csr: ixArgs.csr_term } this
              ]
    }

getActionsInDefinitionItems :: RecTrans.RecDefinitionItems (This -> Actions)
getActionsInDefinitionItems =
  RecTrans.recDefinitionItems
    { definitionItems:
        \defItems gamma metaGamma ixArgs transArgs this ->
          if ixArgs.isSelected then
            []
          else
            concat
              [ concat
                  $ mapWithIndex
                      ( \i (def /\ meta) ->
                          concat
                            [ getActionsInDefinitionSeparator { ix_parentBlock: ixArgs.ix_parentBlock, ix: ixArgs.ix_defSep_at i, csr: ixArgs.csr_defSep_at i } this
                            , getActionsInDefinition def gamma metaGamma { ix_parentBlock: ixArgs.ix_parentBlock, ix: ixArgs.ix_def_at i, csr: ixArgs.csr_def_at i } this
                            ]
                      )
                  $ (fromFoldable defItems)
              , getActionsInDefinitionSeparator { ix_parentBlock: ixArgs.ix_parentBlock, ix: ixArgs.ix_defSep_at (List.length defItems), csr: ixArgs.csr_defSep_at (List.length defItems) } this
              ]
    }

getActionsInDefinitionSeparator :: RecTrans.RecDefinitionSeparator (This -> Actions)
getActionsInDefinitionSeparator =
  RecTrans.recDefinitionSeparator
    { separator:
        \ixArgs trans this -> []
    }

makeCommonDefinitionActions :: forall r. CommonDefinitionTransformations r -> This -> Actions
makeCommonDefinitionActions trans this =
  [ { label: Just "rename", trigger: Trigger_Keypress { key: "r" }, effect: runTransformation trans.rename defaultTransformationInputs this }
  , { label: Just "delete", trigger: Trigger_Keypress { key: "d" }, effect: runTransformation trans.delete defaultTransformationInputs this }
  ]

getActionsInDefinition :: RecTrans.RecDefinition (This -> Actions)
getActionsInDefinition =
  RecTrans.recDefinition
    { term:
        \termBinding type_ term meta gamma { metaGamma_self, metaGamma_children } ixArgs trans this ->
          if ixArgs.isSelected then
            makeCommonDefinitionActions trans this
          else
            concat
              [ getActionsInTermBinding termBinding gamma metaGamma_children { ix: ixArgs.ix_termBinding, csr: ixArgs.csr_termBinding } this
              , getActionsInType type_ gamma metaGamma_children { ix: ixArgs.ix_type, csr: ixArgs.csr_type } this
              , getActionsInTermBinding termBinding gamma metaGamma_children { ix: ixArgs.ix_termBinding, csr: ixArgs.csr_termBinding } this
              , getActionsInTerm term gamma type_ metaGamma_children { ix: ixArgs.ix_term, csr: ixArgs.csr_term } this
              ]
    , data:
        \typeBinding@(TypeBinding typeId _) constrItems meta gamma { metaGamma_self, metaGamma_children } ixArgs trans this ->
          if ixArgs.isSelected then
            makeCommonDefinitionActions trans this
          else
            concat
              [ getActionsInTypeBinding typeBinding gamma metaGamma_children { ix: ixArgs.ix_typeBinding, csr: ixArgs.csr_typeBinding } this
              , concat
                  $ mapWithIndex
                      ( \i (constr /\ meta) ->
                          concat
                            [ getActionsInConstructorSeparator { ix_parentBlock: ixArgs.ix_parentBlock, ix_parentDef: ixArgs.ix, ix: ixArgs.ix_constrSep_at i, csr: ixArgs.csr_constrSep_at i } this
                            , getActionsInConstructor constr typeId gamma metaGamma_children { ix_parentBlock: ixArgs.ix_parentBlock, ix_parentDef: ixArgs.ix, ix: ixArgs.ix_constr_at i, csr: ixArgs.csr_constr_at i } this
                            ]
                      )
                      (fromFoldable constrItems)
              , getActionsInConstructorSeparator { ix_parentBlock: ixArgs.ix_parentBlock, ix_parentDef: ixArgs.ix, ix: ixArgs.ix_constrSep_at (List.length constrItems), csr: ixArgs.csr_constrSep_at (List.length constrItems) } this
              ]
    }

getActionsInConstructor :: RecTrans.RecConstructor (This -> Actions)
getActionsInConstructor =
  RecTrans.recConstructor
    { constructor:
        \termBinding paramItems meta typeId gamma alpha metaGamma metaGamma_param_at ixArgs trans this ->
          if ixArgs.isSelected then
            [ { label: Just "rename", trigger: Trigger_Keypress { key: "r" }, effect: runTransformation trans.rename defaultTransformationInputs this }
            , { label: Just "move", trigger: Trigger_Drop, effect: runTransformation trans.move defaultTransformationInputs this }
            ]
          else
            concat
              [ getActionsInTermBinding termBinding gamma metaGamma { ix: ixArgs.ix_termBinding, csr: ixArgs.csr_termBinding } this
              , concat
                  $ mapWithIndex
                      ( \i (param /\ meta) ->
                          concat
                            [ getActionsInParameterSeparator { ix_parentBlock: ixArgs.ix_parentBlock, ix_parentDef: ixArgs.ix_parentDef, ix_parentConstr: ixArgs.ix, ix: ixArgs.ix_paramSep_at i, csr: ixArgs.csr_paramSep_at i } this
                            , getActionsInParameter param gamma metaGamma { ix: ixArgs.ix_param_at i, csr: ixArgs.csr_param_at i } this
                            ]
                      )
                      (fromFoldable paramItems)
              , getActionsInParameterSeparator { ix_parentBlock: ixArgs.ix_parentBlock, ix_parentDef: ixArgs.ix_parentDef, ix_parentConstr: ixArgs.ix, ix: ixArgs.ix_paramSep_at (List.length paramItems), csr: ixArgs.csr_paramSep_at (List.length paramItems) } this
              ]
    }

getActionsInConstructorSeparator :: RecTrans.RecConstructorSeparator (This -> Actions)
getActionsInConstructorSeparator =
  RecTrans.recConstructorSeparator
    { separator:
        \ixArgs trans this ->
          if ixArgs.isSelected then
            [ { label: Just "insert", trigger: Trigger_Keypress { key: "i" }, effect: runTransformation trans.insert defaultTransformationInputs this }
            , { label: Just "paste", trigger: Trigger_Paste, effect: runTransformation trans.paste defaultTransformationInputs this }
            ]
          else
            []
    }

makeCommonTypeActions :: forall r. RecTrans.CommonTypeTransformations r -> This -> Array Action
makeCommonTypeActions trans this =
  [ { label: Just "enArrow", trigger: Trigger_Keypress { key: "l" }, effect: runTransformation trans.enArrow defaultTransformationInputs this }
  , { label: Just "dig", trigger: Trigger_Keypress { key: "d" }, effect: runTransformation trans.dig defaultTransformationInputs this }
  ]

getActionsInType :: RecTrans.RecType (This -> Actions)
getActionsInType =
  RecTrans.recType
    { arrow:
        \param beta meta gamma metaGamma ixArgs trans this ->
          if ixArgs.isSelected then
            makeCommonTypeActions trans this <> 
            [ {label: Just "delete", trigger: Trigger_Keypress {key: "Backspace"}, effect: runTransformation trans.delete defaultTransformationInputs this}
            ]
          else
            concat
              [ getActionsInParameter param gamma metaGamma { ix: ixArgs.ix_param, csr: ixArgs.csr_param } this
              , getActionsInType beta gamma metaGamma { ix: ixArgs.ix_type, csr: ixArgs.csr_type } this
              ]
    , data:
        \typeId meta gamma metaGamma ixArgs trans this -> if ixArgs.isSelected then makeCommonTypeActions trans this else []
    , hole:
        \holeId wkn meta gamma metaGamma ixArgs trans this -> if ixArgs.isSelected then makeCommonTypeActions trans this else []
    , proxyHole: -- unsafeCrashWith "getActionsInType.proxyHole: should never getActionsIn a proxyHole"
        \holeId gamma metaGamma this -> []
    }

makeCommonTermActions :: forall r. CommonTermTransformations r -> This -> Actions
makeCommonTermActions trans this =
  [ { label: Just "enLambda", trigger: Trigger_Keypress { key: "l" }, effect: runTransformation trans.enLambda defaultTransformationInputs this }
  , { label: Just "copy", trigger: Trigger_Keypress { key: "c" }, effect: runTransformation trans.copy defaultTransformationInputs this }
  , { label: Just "dig", trigger: Trigger_Keypress { key: "d" }, effect: runTransformation trans.dig defaultTransformationInputs this }
  ]

getActionsInTerm :: RecTrans.RecTerm (This -> Actions)
getActionsInTerm =
  RecTrans.recTerm
    { lambda:
        \termId block meta gamma param beta metaGamma ixArgs trans this ->
          if ixArgs.isSelected then
            makeCommonTermActions trans this
              <> [ { label: Just "unLambda", trigger: Trigger_Keypress { key: "Shift+l" }, effect: runTransformation trans.unLambda defaultTransformationInputs this }
                , { label: Just "etaContract", trigger: Trigger_Keypress { key: "Shift+e" }, effect: runTransformation trans.etaContract defaultTransformationInputs this }
                ]
          else
            concat
              [ getActionsInTermId termId gamma metaGamma { ix: ixArgs.ix_termId, csr: ixArgs.csr_termId } this
              , getActionsInBlock block gamma beta metaGamma { ix: ixArgs.ix_block, csr: ixArgs.csr_block } this
              ]
    , neutral:
        \termId argItems meta gamma alpha metaGamma ixArgs trans this ->
          if ixArgs.isSelected then
            makeCommonTermActions trans this
              <> [ { label: Just "etaExpand", trigger: Trigger_Keypress { key: "e" }, effect: runTransformation trans.etaExpand defaultTransformationInputs this }
                ]
          else
            concat
              [ getActionsInTermId termId gamma metaGamma { ix: ixArgs.ix_termId, csr: ixArgs.csr_termId } this
              , getActionsInArgItems argItems gamma alpha metaGamma { ix_parentNeutral: ixArgs.ix, ix: ixArgs.ix_argItems, csr: ixArgs.csr_argItems } this
              ]
    , match:
        \typeId term caseItems meta gamma alpha metaGamma constrIds ixArgs trans this ->
          if ixArgs.isSelected then
            makeCommonTermActions trans this
          else
            concat
              [ getActionsInTerm term gamma (mkData typeId) metaGamma { ix: ixArgs.ix_term, csr: ixArgs.csr_term } this
              , concat
                  $ mapWithIndex
                      ( \i (case_ /\ meta) ->
                          getActionsInCase case_ typeId (List.index' constrIds i) gamma alpha metaGamma { ix_parentMatch: ixArgs.ix, ix: ixArgs.ix_case_at i, csr: ixArgs.csr_case_at i } this
                      )
                      (fromFoldable caseItems)
              ]
    , hole:
        \meta gamma alpha metaGamma ixArgs trans this ->
          if ixArgs.isSelected then
            makeCommonTermActions trans this
              <> [ { label: Just "fill", trigger: Trigger_Drop, effect: runTransformation trans.fill defaultTransformationInputs this }
                ]
          else
            []
    }

-- TODO: add actions for this beyond here
getActionsInArgItems :: RecTrans.RecArgItems (This -> Actions)
getActionsInArgItems =
  RecTrans.recArgItems
    { nil:
        \gamma alpha metaGamma ixArgs trans this -> if ixArgs.isSelected then [] else []
    , cons:
        \(term /\ meta) argItems gamma param@(Parameter alpha _) beta metaGamma ixArgs trans this ->
          if ixArgs.isSelected then
            []
          else
            concat
              [ getActionsInTerm term gamma alpha metaGamma { ix: ixArgs.ix_term, csr: ixArgs.csr_term } this
              , getActionsInArgItems argItems gamma beta metaGamma { ix_parentNeutral: ixArgs.ix_parentNeutral, ix: ixArgs.ix_argItems, csr: ixArgs.csr_argItems } this
              ]
    }

getActionsInCase :: RecTrans.RecCase (This -> Actions)
getActionsInCase =
  RecTrans.recCase
    { case_:
        \termIdItems block meta typeId constrId gamma alpha metaGamma ixArgs trans this ->
          if ixArgs.isSelected then
            []
          else
            concat
              [ concat
                  $ mapWithIndex
                      ( \i (termId /\ meta) ->
                          concat
                            [ getActionsInTermId termId gamma metaGamma { ix: ixArgs.ix_termId_at i, csr: ixArgs.csr_termId_at i } this
                            ]
                      )
                      (fromFoldable termIdItems)
              , getActionsInBlock block gamma alpha metaGamma { ix: ixArgs.ix_block, csr: ixArgs.csr_block } this
              ]
    }

getActionsInParameter :: RecTrans.RecParameter (This -> Actions)
getActionsInParameter =
  RecTrans.recParameter
    { parameter:
        \alpha meta gamma { metaGamma_self, metaGamma_children } ixArgs trans this ->
          if ixArgs.isSelected then
            []
          else
            getActionsInType alpha gamma metaGamma_children { ix: ixArgs.ix_type, csr: ixArgs.csr_type } this
    }

getActionsInParameterSeparator :: RecTrans.RecParameterSeparator (This -> Actions)
getActionsInParameterSeparator =
  RecTrans.recParameterSeparator
    { separator:
        \ixArgs trans this ->
          if ixArgs.isSelected then
            [ { label: Just "insert", trigger: Trigger_Keypress { key: "i" }, effect: runTransformation trans.insert defaultTransformationInputs this }
            , { label: Just "paste", trigger: Trigger_Paste, effect: runTransformation trans.paste defaultTransformationInputs this }
            ]
          else
            []
    }

getActionsInTypeBinding :: RecTrans.RecTypeBinding (This -> Actions)
getActionsInTypeBinding =
  RecTrans.recTypeBinding
    { typeBinding:
        \typeId meta gamma metaGamma ixArgs trans this -> if ixArgs.isSelected then [] else []
    }

getActionsInTermBinding :: RecTrans.RecTermBinding (This -> Actions)
getActionsInTermBinding =
  RecTrans.recTermBinding
    { termBinding:
        \termId meta gamma metaGamma ixArgs trans this -> if ixArgs.isSelected then [] else []
    }

getActionsInTermId :: RecTrans.RecTermId (This -> Actions)
getActionsInTermId =
  RecTrans.recTermId
    { termId:
        \termId gamma metaGamma ixArgs trans this -> if ixArgs.isSelected then [] else []
    }

runTransformation :: Transformation -> TransformationInputs -> This -> Effect Unit
runTransformation trans inputs this = do
  Debug.traceM $ "[runTransformation]"
  modifyState this \st -> case trans inputs st of
    Just st' -> st'
    Nothing -> st
  runRefreshSelection this

runToggleIndentedAt :: DownwardIndex -> This -> Effect Unit
runToggleIndentedAt ix this = do
  Debug.traceM $ "[runToggleIndexAt] ix = " <> show ix
  modifyState this \st -> st { module_ = toModule $ toggleIndentedMetadataAt ix (SyntaxModule st.module_) }

runRefreshSelection :: This -> Effect Unit
runRefreshSelection this = do
  st <- getState this
  Debug.traceM $ "[runRefreshSelection] ix = " <> show st.ix_cursor
  modifyState this \st ->
    let
      actions = getActionsInModule st.module_ emptyContext emptyMetaContext { csr: Just st.ix_cursor, ix: UpwardIndex Nil } this

      actions_keymap =
        Map.fromFoldable $ map fromJust $ filter isJust
          $ map
              ( \action -> case action.trigger of
                  Trigger_Keypress { key } -> Just (key /\ action)
                  _ -> Nothing
              )
              actions
    in
      Debug.trace ("[runRefreshSelection] actions = " <> show ((_.label) <$> actions)) \_ ->
        st
          { actions = actions
          , actions_keymap = actions_keymap
          }

runSelectHere :: NodeProps -> This -> Effect Unit
runSelectHere props this = do
  let
    ix = toDownwardIndex $ fromJust props.ix
  -- actions =
  --   concat
  --     [ props.actions
  --     , if props.isIndentable then [ { label: Just "indent", trigger: Trigger_Keypress { key: "Tab" }, effect: runToggleIndentedAt ix this } ] else []
  --     ]
  modifyState this \st ->
    st
      { ix_cursor = ix
      , environment =
        { metaGamma: Nothing
        , gamma: Nothing
        , goal: Nothing
        }
      }
  runRefreshSelection this

defaultNodeProps :: NodeProps
defaultNodeProps =
  { ix: Nothing
  , isSelected: Nothing
  , isIndentable: false
  , isEditable: false
  }

nodePropsFromIxArgs :: forall r. { ix :: UpwardIndex, isSelected :: Boolean | r } -> NodeProps
nodePropsFromIxArgs { ix, isSelected } = defaultNodeProps { ix = Just ix, isSelected = Just isSelected }

type NodeProps
  = { ix :: Maybe UpwardIndex
    , isSelected :: Maybe Boolean
    , isIndentable :: Boolean
    , isEditable :: Boolean
    }

runAction :: Action -> Effect Unit
runAction action = do
  Debug.traceM $ "[runAction] label = " <> show (action.label)
  action.effect

isSelectable :: NodeProps -> Maybe (UpwardIndex /\ Boolean)
isSelectable props = (\ix b -> ix /\ b) <$> props.ix <*> props.isSelected

fromUpwardIndexToElementId :: UpwardIndex -> String 
fromUpwardIndexToElementId ix = show ix

highlight :: HTMLElement -> Effect Unit 
highlight elem = do
  cls <- classList elem 
  isSelected <- DOMTokenList.contains cls "selected"
  DOMTokenList.add cls "highlighted"

unhighlight :: HTMLElement -> Effect Unit
unhighlight elem = do 
  cls <- classList elem
  DOMTokenList.remove cls "highlighted"