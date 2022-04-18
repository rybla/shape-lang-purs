module Language.Shape.Stlc.Recursion.Action where

import Data.Tuple
import Data.Tuple.Nested
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.IndexMetadata
import Language.Shape.Stlc.RenderingTypes
import Language.Shape.Stlc.Syntax
import Prelude
import Data.Array (filter)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Debug as Debug
import Effect (Effect)
import Language.Shape.Stlc.Recursion.Transformation (defaultTransformationInputs)
import Language.Shape.Stlc.Recursion.Transformation as RecTrans
import React (getState, modifyState)
import Undefined (undefined)
import Unsafe (fromJust)
import Web.DOM.DOMTokenList as DOMTokenList
import Web.HTML (HTMLElement)
import Web.HTML.HTMLElement (classList)

type RecModule a
  = RecTrans.RecModule (This -> a)

type RecModule_Module a
  = RecTrans.RecModule_Module (This -> Actions -> a)

recModule ::
  forall a.
  { module_ :: RecModule_Module a } ->
  RecModule a
recModule rec =
  RecTrans.recModule
    { module_:
        \defItems meta gamma metaGamma ixArgs trans this ->
          rec.module_ defItems meta gamma metaGamma ixArgs trans this []
    }

type RecBlock a
  = RecTrans.RecBlock (This -> a)

type RecBlock_Block a
  = RecTrans.RecBlock_Block (This -> Actions -> a)

recBlock ::
  forall a.
  { block :: RecBlock_Block a } ->
  RecBlock a
recBlock rec =
  RecTrans.recBlock
    { block:
        \defItems block meta gamma alpha metaGamma ixArgs trans this ->
          rec.block defItems block meta gamma alpha metaGamma ixArgs trans this
            [ makeIndentAction ixArgs this ]
    }

type RecDefinitionItems a
  = RecTrans.RecDefinitionItems (This -> a)

type RecDefinitionItems_DefinitionItems a
  = RecTrans.RecDefinitionItems_DefinitionItems (This -> Actions -> a)

recDefinitionItems ::
  forall a.
  { definitionItems :: RecDefinitionItems_DefinitionItems a } ->
  RecDefinitionItems a
recDefinitionItems rec =
  RecTrans.recDefinitionItems
    { definitionItems:
        \defItems gamma metaGamma ixArgs trans this ->
          rec.definitionItems defItems gamma metaGamma ixArgs trans this
            []
    }

type RecDefinitionSeparator a
  = RecTrans.RecDefinitionSeparator (This -> a)

type RecDefinitionSeparator_Separator a
  = RecTrans.RecDefinitionSeparator_Separator (This -> Actions -> a)

recDefinitionSeparator ::
  forall a.
  { separator :: RecDefinitionSeparator_Separator a } ->
  RecDefinitionSeparator a
recDefinitionSeparator rec =
  RecTrans.recDefinitionSeparator
    { separator:
        \ixArgs trans this ->
          rec.separator ixArgs trans this
            []
    }

type RecDefinition a
  = RecTrans.RecDefinition (This -> a)

type RecDefinition_TermDefinition a
  = RecTrans.RecDefinition_TermDefinition (This -> Actions -> a)

type RecDefinition_DataDefinition a
  = RecTrans.RecDefinition_DataDefinition (This -> Actions -> a)

recDefinition ::
  forall a.
  { term :: RecDefinition_TermDefinition a
  , data :: RecDefinition_DataDefinition a
  } ->
  RecDefinition a
recDefinition rec =
  let
    common trans this =
      [ { label: Just "delete", trigger: Trigger_Keypress { key: "Backspace" }, effect: runTransformation trans.delete defaultTransformationInputs this }
      ]
  in
    RecTrans.recDefinition
      { term:
          \termBinding type_ term meta gamma metaGamma ixArgs trans this ->
            rec.term termBinding type_ term meta gamma metaGamma ixArgs trans this
              (common trans this)
      , data:
          \typeBinding constrItems meta gamma metaGamma ixArgs trans this ->
            rec.data typeBinding constrItems meta gamma metaGamma ixArgs trans this
              (common trans this)
      }

type RecConstructorSeparator a
  = RecTrans.RecConstructorSeparator (This -> a)

type RecConstructorSeparator_Separator a
  = RecTrans.RecConstructorSeparator_Separator (This -> Actions -> a)

recConstructorSeparator ::
  forall a.
  { separator :: RecConstructorSeparator_Separator a } ->
  RecConstructorSeparator a
recConstructorSeparator rec =
  RecTrans.recConstructorSeparator
    { separator:
        \ixArgs trans this ->
          rec.separator ixArgs trans this
            []
    }

type RecConstructor a
  = RecTrans.RecConstructor (This -> a)

type RecConstructor_Constructor a
  = RecTrans.RecConstructor_Constructor (This -> Actions -> a)

recConstructor ::
  forall a.
  { constructor :: RecConstructor_Constructor a } ->
  RecConstructor a
recConstructor rec =
  RecTrans.recConstructor
    { constructor:
        \termBinding paramItems meta typeId gamma alpha metaGamma metaGamma_param_t ixArgs trans this ->
          rec.constructor termBinding paramItems meta typeId gamma alpha metaGamma metaGamma_param_t ixArgs trans this
            []
    }

type RecParameterSeparator a
  = RecTrans.RecParameterSeparator (This -> a)

type RecParameterSeparator_Separator a
  = RecTrans.RecParameterSeparator_Separator (This -> Actions -> a)

recParameterSeparator ::
  forall a.
  { separator :: RecParameterSeparator_Separator a } ->
  RecParameterSeparator a
recParameterSeparator rec =
  RecTrans.recParameterSeparator
    { separator:
        \ixArgs trans this ->
          rec.separator ixArgs trans this
            []
    }

type RecType a
  = RecTrans.RecType (This -> a)

type RecType_Arrow a
  = RecTrans.RecType_Arrow (This -> Actions -> a)

type RecType_Data a
  = RecTrans.RecType_Data (This -> Actions -> a)

type RecType_Hole a
  = RecTrans.RecType_Hole (This -> Actions -> a)

type RecType_ProxyHole a
  = RecTrans.RecType_ProxyHole a

recType ::
  forall a.
  { arrow :: RecType_Arrow a
  , data :: RecType_Data a
  , hole :: RecType_Hole a
  , proxyHole :: RecType_ProxyHole a
  } ->
  RecType a
recType rec =
  let
    common :: forall r. { enArrow :: RecTrans.Transformation, dig :: RecTrans.Transformation | r } -> This -> Actions
    common trans this =
      [ { label: Just "enArrow", trigger: Trigger_Keypress { key: "l" }, effect: runTransformation trans.enArrow defaultTransformationInputs this }
      , { label: Just "dig", trigger: Trigger_Keypress { key: "d" }, effect: runTransformation trans.dig defaultTransformationInputs this }
      ]
  in
    RecTrans.recType
      { arrow:
          \prm beta meta gamma metaGamma ixArgs trans this ->
            rec.arrow prm beta meta gamma metaGamma ixArgs trans this
              $ common trans this
              <> [ { label: Just "delete", trigger: Trigger_Keypress { key: "Backspace" }, effect: runTransformation trans.delete defaultTransformationInputs this }
                ]
      , data:
          \typeId meta gamma metaGamma ixArgs trans this ->
            rec.data typeId meta gamma metaGamma ixArgs trans this
              $ common trans this
      , hole:
          \holeId wkn meta gamma metaGamma ixArgs trans this ->
            rec.hole holeId wkn meta gamma metaGamma ixArgs trans this
              $ common trans this
      , proxyHole:
          \holeId gamma metaGamma this ->
            undefined
      }

type RecTerm a
  = RecTrans.RecTerm (This -> a)

type RecTerm_Lambda a
  = RecTrans.RecTerm_Lambda (This -> Actions -> a)

type RecTerm_Neutral a
  = RecTrans.RecTerm_Neutral (This -> Actions -> a)

type RecTerm_Match a
  = RecTrans.RecTerm_Match (This -> Actions -> a)

type RecTerm_Hole a
  = RecTrans.RecTerm_Hole (This -> Actions -> a)

recTerm ::
  forall a.
  { lambda :: RecTerm_Lambda a
  , neutral :: RecTerm_Neutral a
  , match :: RecTerm_Match a
  , hole :: RecTerm_Hole a
  } ->
  RecTerm a
recTerm rec =
  let
    common :: forall r. { enLambda :: RecTrans.Transformation, dig :: RecTrans.Transformation | r } -> This -> Actions
    common trans this =
      [ { label: Just "enLambda", trigger: Trigger_Keypress { key: "l" }, effect: runTransformation trans.enLambda defaultTransformationInputs this }
      , { label: Just "dig", trigger: Trigger_Keypress { key: "d" }, effect: runTransformation trans.dig defaultTransformationInputs this }
      ]
  in
    RecTrans.recTerm
      { lambda:
          \termId block meta gamma param beta metaGamma ixArgs trans this ->
            rec.lambda termId block meta gamma param beta metaGamma ixArgs trans this
              $ common trans this
              <> [ { label: Just "unLambda", trigger: Trigger_Keypress { key: "Shift+l" }, effect: runTransformation trans.unLambda defaultTransformationInputs this }
                , { label: Just "etaContract", trigger: Trigger_Keypress { key: "Shift+e" }, effect: runTransformation trans.etaContract defaultTransformationInputs this }
                ]
      , neutral:
          \termId argItems meta gamma alpha metaGamma ixArgs trans this ->
            rec.neutral termId argItems meta gamma alpha metaGamma ixArgs trans this
              $ common trans this
              <> [ { label: Just "etaExpand", trigger: Trigger_Keypress { key: "e" }, effect: runTransformation trans.etaExpand defaultTransformationInputs this }
                , makeIndentAction ixArgs this
                ]
      , match:
          \typeId term caseItems meta gamma alpha metaGamma constrIds ixArgs trans this ->
            rec.match typeId term caseItems meta gamma alpha metaGamma constrIds ixArgs trans this
              $ common trans this
              <> [ makeIndentAction ixArgs this ]
      , hole:
          \meta gamma alpha metaGamma ixArgs trans this ->
            rec.hole meta gamma alpha metaGamma ixArgs trans this
              $ common trans this
              <> [ { label: Just "fill", trigger: Trigger_Drop, effect: runTransformation trans.fill defaultTransformationInputs this }
                , makeIndentAction ixArgs this
                ]
      }

type RecArgItems a
  = RecTrans.RecArgItems (This -> a)

type RecArgItems_Nil a
  = RecTrans.RecArgItems_Nil (This -> Actions -> a)

type RecArgItems_Cons a
  = RecTrans.RecArgItems_Cons (This -> Actions -> a)

recArgItems ::
  forall a.
  { nil :: RecArgItems_Nil a
  , cons :: RecArgItems_Cons a
  } ->
  RecArgItems a
recArgItems rec =
  RecTrans.recArgItems
    { nil:
        \gamma alpha metaGamma ixArgs trans this ->
          rec.nil gamma alpha metaGamma ixArgs trans this
            []
    , cons:
        \argItem argItems meta prm alpha metaGamma ixArgs trans this ->
          rec.cons argItem argItems meta prm alpha metaGamma ixArgs trans this
            [makeIndentAction ixArgs this]
    }

type RecCase a
  = RecTrans.RecCase (This -> a)

type RecCase_Case a
  = RecTrans.RecCase_Case (This -> Actions -> a)

recCase ::
  forall a.
  { case_ :: RecCase_Case a } ->
  RecCase a
recCase rec =
  RecTrans.recCase
    { case_:
        \termIdItems block meta typeId constrId gamma alpha metaGamma ixArgs trans this ->
          rec.case_ termIdItems block meta typeId constrId gamma alpha metaGamma ixArgs trans this
            [makeIndentAction ixArgs this]
    }

type RecParameter a
  = RecTrans.RecParameter (This -> a)

type RecParameter_Parameter a
  = RecTrans.RecParameter_Parameter (This -> Actions -> a)

recParameter ::
  forall a.
  { parameter :: RecParameter_Parameter a } ->
  RecParameter a
recParameter rec =
  RecTrans.recParameter
    { parameter:
        \alpha meta gamma metaGamma ixArgs trans this ->
          rec.parameter alpha meta gamma metaGamma ixArgs trans this
            []
    }

type RecTypeBinding a
  = RecTrans.RecTypeBinding (This -> a)

type RecTypeBinding_TypeBinding a
  = RecTrans.RecTypeBinding_TypeBinding (This -> Actions -> a)

recTypeBinding ::
  forall a.
  { typeBinding :: RecTypeBinding_TypeBinding a
  } ->
  RecTypeBinding a
recTypeBinding rec =
  RecTrans.recTypeBinding
    { typeBinding:
        \typeId meta gamma metaGamma ixArgs trans this ->
          rec.typeBinding typeId meta gamma metaGamma ixArgs trans this
            []
    }

type RecTermBinding a
  = RecTrans.RecTermBinding (This -> a)

type RecTermBinding_TermBinding a
  = RecTrans.RecTermBinding_TermBinding (This -> Actions -> a)

recTermBinding ::
  forall a.
  { termBinding :: RecTermBinding_TermBinding a
  } ->
  RecTermBinding a
recTermBinding rec =
  RecTrans.recTermBinding
    { termBinding:
        \typeId meta gamma metaGmma ixArgs trans this ->
          rec.termBinding typeId meta gamma metaGmma ixArgs trans this
            []
    }

type RecTermId a
  = RecTrans.RecTermId (This -> a)

type RecTermId_TermId a
  = RecTrans.RecTermId_TermId (This -> Actions -> a)

recTermId :: forall a. { termId :: RecTermId_TermId a } -> RecTermId a
recTermId rec =
  RecTrans.recTermId
    { termId:
        \termId gamma metaGamma ixArgs trans this ->
          rec.termId termId gamma metaGamma ixArgs trans this
            [makeIndentAction ixArgs this]
    }

runTransformation :: RecTrans.Transformation -> RecTrans.TransformationInputs -> This -> Effect Unit
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
      actions = undefined -- getActionsInModule st.module_ emptyContext emptyMetaContext { csr: Just st.ix_cursor, ix: UpwardIndex Nil } this

      actions_keymap =
        Map.fromFoldable $ map fromJust $ filter isJust
          $ map
              ( \action -> case action.trigger of
                  Trigger_Keypress { key } -> Just (key /\ action)
                  _ -> Nothing
              )
              actions
    in
      -- Debug.trace ("[runRefreshSelection] actions = " <> show ((_.label) <$> actions)) \_ ->
      --   st
      --     { actions = actions
      --     , actions_keymap = actions_keymap
      --     }
      undefined

runSelectHere :: NodeProps -> This -> Effect Unit
runSelectHere props this = do
  let
    ix = toDownwardIndex $ fromJust props.ix
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

makeIndentAction :: forall r. { ix :: UpwardIndex | r } -> This -> Action
makeIndentAction ixArgs this =
  { label: Just "indent"
  , trigger: Trigger_Keypress { key: "Tab" }
  , effect: modifyState this \st -> st { module_ = toModule $ toggleIndentedMetadataAt (toDownwardIndex ixArgs.ix) (SyntaxModule st.module_) }
  }
