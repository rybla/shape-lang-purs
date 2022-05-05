module Language.Shape.Stlc.Rendering.Syntax where

import Data.Tuple
import Data.Tuple.Nested
import Language.Shape.Stlc.Rendering.Token
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Control.Monad.Reader (ask)
import Control.Monad.State (StateT)
import Control.Monad.State as State
import Data.Array (concat)
import Data.Default (default)
import Data.Identity (Identity(..))
import Data.List.Unsafe (List)
import Data.List.Unsafe as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.Traversable (sequence)
import Language.Shape.Stlc.Context (Context(..))
import Language.Shape.Stlc.Metacontext (Metacontext(..))
import Language.Shape.Stlc.Recursor.Action as RecAct
import Language.Shape.Stlc.Recursor.Context as RecCtx
import Language.Shape.Stlc.Recursor.Index (Visit)
import Language.Shape.Stlc.Recursor.Index as RecIx
import Language.Shape.Stlc.Recursor.Metacontext as RecMeta
import Language.Shape.Stlc.Types (Action(..), This, getState')
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Union)
import React (ReactElement)
import React.DOM as DOM
import React.DOM.Props as Props
import Record (union)
import Undefined (undefined)

type M a
  = StateT (List HoleId) (StateT (Array Action) Identity) a

renderProgram :: This -> (Array ReactElement /\ Array Action)
renderProgram this =
  (\(Identity x) -> x)
    $ flip State.runStateT []
    $ flip State.evalStateT List.Nil
    $ renderTerm
        -- TODO: maybe pull this out into multiple files or at least somewhere else?
        { act: {}
        , ctx: { ctx: default, type_: HoleType { holeId: freshHoleId unit, weakening: Set.empty, meta: default } }
        , meta: { meta: default }
        , ix: { visit: { csr: Just st.ix, ix: mempty } }
        , syn: { term: st.term }
        }
  where
  st = getState' this

renderType :: RecAct.ProtoRec RecAct.ArgsType () Identity (Array ReactElement)
renderType =
  RecAct.recType
    { arrow:
        \args ->
          renderNode
            ( (useArgsCtx args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "ArrowType" }
            )
            [ renderType { syn: { type_: args.syn.arrow.dom }, ctx: args.ctx, ix: { visit: args.ix.dom }, meta: { meta: args.meta.dom }, act: {} }
            , pure [ token.arrowType1 ]
            , renderType { syn: { type_: args.syn.arrow.cod }, ctx: args.ctx, ix: { visit: args.ix.cod }, meta: { meta: args.meta.cod }, act: {} }
            ]
    , data_:
        \args ->
          renderNode
            ( (useArgsCtx args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "DataType" }
            )
            [ pure $ printTypeId { typeId: args.syn.data_.typeId, meta: args.meta.meta }
            ]
    , hole:
        \args ->
          renderNode
            ( (useArgsCtx args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "HoleType" }
            )
            [ printHoleId { holeId: args.syn.hole.holeId, meta: args.meta.meta }
            ]
    }

renderTerm :: RecAct.ProtoRec RecAct.ArgsTerm () Identity (Array ReactElement)
renderTerm =
  RecAct.recTerm
    { lam:
        \args ->
          renderNode
            ( (useArgsCtx_Term args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "Lam" }
            )
            [ pure [ token.lam1 ]
            , renderTermBind { syn: { termBind: args.syn.lam.termBind }, ctx: { ctx: args.ctx.ctx }, ix: { visit: args.ix.termBind }, meta: { meta: args.meta.meta }, act: {} }
            , pure [ token.lam2 ]
            , renderTerm { syn: { term: args.syn.lam.body }, ctx: args.ctx.body, ix: { visit: args.ix.body }, meta: { meta: args.meta.body }, act: {} }
            ]
    , neu:
        \args ->
          renderNode
            ( (useArgsCtx_Term args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "Neu" }
            )
            if List.length args.syn.neu.argItems == 0 then
              [ renderTermId { syn: { termId: args.syn.neu.termId }, ctx: { ctx: args.ctx.termId }, ix: { visit: args.ix.termId }, meta: { meta: args.meta.meta }, act: {} }
              ]
            else
              [ renderTermId { syn: { termId: args.syn.neu.termId }, ctx: { ctx: args.ctx.termId }, ix: { visit: args.ix.termId }, meta: { meta: args.meta.meta }, act: {} }
              , pure [ token.neu1 ]
              , renderArgItems { syn: { argItems: args.syn.neu.argItems }, ctx: args.ctx.argItems, ix: { visit: args.ix.argItems }, meta: { meta: args.meta.argItems }, act: {} }
              ]
    , let_:
        \args ->
          renderNode
            ( (useArgsCtx_Term args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "Let" }
            )
            [ pure [ token.let1 ]
            , renderTermBind { syn: { termBind: args.syn.let_.termBind }, ctx: { ctx: args.ctx.ctx }, ix: { visit: args.ix.termBind }, meta: { meta: args.meta.meta }, act: {} }
            , pure [ token.let2 ]
            , renderType { syn: { type_: args.syn.let_.type_ }, ctx: { ctx: args.ctx.ctx }, ix: { visit: args.ix.type_ }, meta: { meta: args.meta.type_ }, act: {} }
            , pure [ token.let3 ]
            , renderTerm { syn: { term: args.syn.let_.term }, ctx: args.ctx.term, ix: { visit: args.ix.term }, meta: { meta: args.meta.term }, act: {} }
            , pure [ token.let4 ]
            , renderTerm { syn: { term: args.syn.let_.body }, ctx: args.ctx.body, ix: { visit: args.ix.body }, meta: { meta: args.meta.body }, act: {} }
            ]
    , buf:
        \args ->
          renderNode
            ( (useArgsCtx_Term args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "Buf" }
            )
            [ pure [ token.buf1 ]
            , renderTerm { syn: { term: args.syn.buf.term }, ctx: args.ctx.term, ix: { visit: args.ix.term }, meta: { meta: args.meta.term }, act: {} }
            , pure [ token.buf2 ]
            , renderType { syn: { type_: args.syn.buf.type_ }, ctx: { ctx: args.ctx.ctx }, ix: { visit: args.ix.type_ }, meta: { meta: args.meta.meta }, act: {} }
            , pure [ token.buf3 ]
            , renderTerm { syn: { term: args.syn.buf.body }, ctx: args.ctx.body, ix: { visit: args.ix.body }, meta: { meta: args.meta.body }, act: {} }
            ]
    , data_:
        \args ->
          renderNode
            ( (useArgsCtx_Term args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "Data" }
            )
            [ pure [ token.data1 ]
            , renderTypeBind { syn: { typeBind: args.syn.data_.typeBind }, ctx: { ctx: args.ctx.ctx }, ix: { visit: args.ix.typeBind }, meta: { meta: args.meta.meta }, act: {} }
            , pure [ token.data2 ]
            , renderSumItems { syn: { sumItems: args.syn.data_.sumItems }, ctx: { ctx: args.ctx.ctx }, ix: { visit: args.ix.sumItems }, meta: { meta: args.meta.sumItems }, act: {} }
            , pure [ token.data3 ]
            , renderTerm { syn: { term: args.syn.data_.body }, ctx: args.ctx.body, ix: { visit: args.ix.body }, meta: { meta: args.meta.body }, act: {} }
            ]
    , match:
        \args ->
          renderNode
            ( (useArgsCtx_Term args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "Match" }
            )
            [ pure [ token.match1 ]
            , renderTerm { syn: { term: args.syn.match.term }, ctx: args.ctx.term, ix: { visit: args.ix.term }, meta: { meta: args.meta.term }, act: {} }
            , pure [ token.match2 ]
            , renderCaseItems { syn: { caseItems: args.syn.match.caseItems }, ctx: args.ctx.caseItems, ix: { visit: args.ix.caseItems }, meta: { meta: args.meta.caseItems }, act: {} }
            ]
    , hole:
        \args ->
          renderNode
            ( (useArgsCtx_Term args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "Hole" }
            )
            [ renderType { syn: { type_: args.ctx.type_ }, ctx: { ctx: args.ctx.ctx }, ix: { visit: { csr: Nothing, ix: Nothing } }, meta: { meta: args.meta.meta }, act: {} }
            ]
    }

renderArgItems :: RecAct.ProtoRec RecAct.ArgsArgItems () Identity (Array ReactElement)
renderArgItems =
  (List.foldl append [] <$> _)
    <<< RecAct.recArgItems
        { argItem:
            \args ->
              renderNode
                ( (useArgsCtx args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                    { label = Just "ArgItem" }
                )
                [ pure $ newline args.meta.meta (unwrap args.syn.argItem.meta).indented
                , renderTerm { syn: { term: args.syn.argItem.term }, ctx: args.ctx.argItem, ix: { visit: args.ix.argItem }, meta: { meta: args.meta.meta }, act: {} }
                ]
        }

renderSumItems :: RecAct.ProtoRec RecAct.ArgsSumItems () Identity (Array ReactElement)
renderSumItems =
  (List.foldl append [] <$> _)
    <<< RecAct.recSumItems
        { sumItem:
            \args ->
              renderNode
                ( (useArgsCtx args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                    { label = Just "SumItem" }
                )
                [ pure $ newline args.meta.meta (unwrap args.syn.sumItem.meta).indented
                , pure [ token.sumItem1 ]
                , renderTermBind { syn: { termBind: args.syn.sumItem.termBind }, ctx: { ctx: args.ctx.ctx }, ix: { visit: args.ix.termBind }, meta: { meta: args.meta.meta }, act: {} }
                , pure [ token.sumItem2 ]
                , renderParamItems { syn: { paramItems: args.syn.sumItem.paramItems }, ctx: { ctx: args.ctx.ctx }, ix: { visit: args.ix.paramItems }, meta: { meta: args.meta.paramItems }, act: {} }
                ]
        }

renderCaseItems :: RecAct.ProtoRec RecAct.ArgsCaseItems () Identity (Array ReactElement)
renderCaseItems =
  (List.foldl append [] <$> _)
    <<< RecAct.recCaseItems
        { caseItem:
            \args ->
              renderNode
                ( (useArgsCtx args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                    { label = Just "CaseItem" }
                )
                [ pure $ newline args.meta.meta (unwrap args.syn.caseItem.meta).indented
                , pure [ token.caseItem1 ]
                , renderTermBindItems { syn: { termBindItems: args.syn.caseItem.termBindItems }, ctx: { ctx: args.ctx.ctx }, ix: { visit: args.ix.termBindItems }, meta: { meta: args.meta.meta }, act: {} }
                , pure [ token.caseItem2 ]
                , renderTerm { syn: { term: args.syn.caseItem.body }, ctx: args.ctx.body, ix: { visit: args.ix.body }, meta: { meta: args.meta.body }, act: {} }
                ]
        }

renderParamItems :: RecAct.ProtoRec RecAct.ArgsParamItems () Identity (Array ReactElement)
renderParamItems =
  (List.foldl append [] <$> _)
    <<< RecAct.recParamItems
        { paramItem:
            \args ->
              renderNode
                ( (useArgsCtx args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                    { label = Just "ParamItem" }
                )
                [ pure $ newline args.meta.meta (unwrap args.syn.paramItem.meta).indented
                , renderType { syn: { type_: args.syn.paramItem.type_ }, ctx: { ctx: args.ctx.ctx }, ix: { visit: args.ix.type_ }, meta: { meta: args.meta.meta }, act: {} }
                ]
        }

renderTermBindItems :: RecAct.ProtoRec RecAct.ArgsTermBindItems () Identity (Array ReactElement)
renderTermBindItems =
  (List.foldl append [] <$> _)
    <<< RecAct.recTermBindItems
        { termBindItem:
            \args ->
              renderNode
                ( (useArgsCtx args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                    { label = Just "TermBindItem" }
                )
                [ pure $ newline args.meta.meta (unwrap args.syn.termBindItem.meta).indented
                , renderTermBind { syn: { termBind: args.syn.termBindItem.termBind }, ctx: { ctx: args.ctx.ctx }, ix: { visit: args.ix.termBind }, meta: { meta: args.meta.meta }, act: {} }
                ]
        }

renderTermBind :: RecAct.ProtoRec RecAct.ArgsTermBind () Identity (Array ReactElement)
renderTermBind =
  RecAct.recTermBind
    { termBind:
        \args ->
          renderNode
            ( (useArgsCtx args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "TermBind" }
            )
            [ pure $ printTermId { termId: args.syn.termBind.termId, meta: args.meta.meta } ]
    }

renderTypeBind :: RecAct.ProtoRec RecAct.ArgsTypeBind () Identity (Array ReactElement)
renderTypeBind =
  RecAct.recTypeBind
    { typeBind:
        \args ->
          renderNode
            ( (useArgsCtx args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "TypeBind" }
            )
            [ pure $ printTypeId { typeId: args.syn.typeBind.typeId, meta: args.meta.meta } ]
    }

renderTypeId :: RecAct.ProtoRec RecAct.ArgsTypeId () Identity (Array ReactElement)
renderTypeId =
  RecAct.recTypeId
    { typeId:
        \args ->
          renderNode
            ( (useArgsCtx args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "TypeId" }
            )
            [ pure $ printTypeId { typeId: args.syn.typeId, meta: args.meta.meta } ]
    }

renderTermId :: RecAct.ProtoRec RecAct.ArgsTermId () Identity (Array ReactElement)
renderTermId =
  RecAct.recTermId
    { termId:
        \args ->
          renderNode
            ( (useArgsCtx args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "TermId" }
            )
            [ pure $ printTermId { termId: args.syn.termId, meta: args.meta.meta } ]
    }

printTypeId :: { typeId :: TypeId, meta :: Metacontext } -> Array ReactElement
printTypeId { typeId, meta } =
  [ DOM.span [ Props.className "typeId" ]
      $ [ DOM.span [ Props.className "name" ] [ DOM.text (show name) ] ]
      <> if 0 < shadow_i then
          [ DOM.span [ Props.className "shadow" ] [ DOM.text (show shadow_i) ] ]
        else
          []
  ]
  where
  name = case Map.lookup typeId (unwrap meta).dataNames of
    Just name -> name
    Nothing -> unsafeCrashWith $ "could not find name of type id " <> show typeId <> " in metacontext " <> show meta

  shadow_i = case Map.lookup name (unwrap meta).dataShadows of
    Just i -> i
    Nothing -> unsafeCrashWith $ "could not find shadow of data name " <> show name <> " in metacontext " <> show meta

printTermId :: { termId :: TermId, meta :: Metacontext } -> Array ReactElement
printTermId { termId, meta } =
  [ DOM.span [ Props.className "termId" ]
      $ [ DOM.span [ Props.className "name" ] [ DOM.text (show name) ] ]
      <> if 0 < shadow_i then
          [ DOM.span [ Props.className "shadow" ] [ DOM.text (show shadow_i) ] ]
        else
          []
  ]
  where
  name = case Map.lookup termId (unwrap meta).varNames of
    Just name -> name
    Nothing -> unsafeCrashWith $ "could not find name of term id " <> show termId <> " in metacontext " <> show meta

  shadow_i = case Map.lookup name (unwrap meta).varShadows of
    Just i -> i
    Nothing -> unsafeCrashWith $ "could not find shadow of var name " <> show name <> " in metacontext " <> show meta

printHoleId :: { holeId :: HoleId, meta :: Metacontext } -> M (Array ReactElement)
printHoleId args = do
  i <- List.findIndex (args.holeId == _) <$> State.get
  pure [ DOM.span [ Props.className "holeId" ] [ DOM.text $ show i ] ]

type NodeProps
  = { label :: Maybe String
    , visit :: Maybe Visit
    , type_ :: Maybe Type
    , ctx :: Maybe Context
    , meta :: Maybe Metacontext
    , actions :: Array Action
    }

defaultNodeProps :: NodeProps
defaultNodeProps =
  { label: default
  , visit: default
  , type_: default
  , ctx: default
  , meta: default
  , actions: []
  }

maybeArray :: forall a b. Maybe a -> (a -> b) -> Array b
maybeArray ma f = case ma of
  Just a -> [ f a ]
  Nothing -> []

useArgsCtx :: forall r1 r2. Record (RecCtx.ProtoArgsType r1 r2) -> NodeProps -> NodeProps
useArgsCtx { ctx } = _ { ctx = Just ctx.ctx }

useArgsCtx_Term :: forall r1 r2. Record (RecCtx.ProtoArgsTerm r1 r2) -> NodeProps -> NodeProps
useArgsCtx_Term { ctx } = _ { ctx = Just ctx.ctx, type_ = Just ctx.type_ }

useArgsMeta :: forall r1 r2. { meta :: { meta :: Metacontext | r1 } | r2 } -> NodeProps -> NodeProps
useArgsMeta { meta } = _ { meta = Just meta.meta }

useArgsIx :: forall r1 r2. Record (RecIx.ProtoArgs r1 r2) -> NodeProps -> NodeProps
useArgsIx { ix } = _ { visit = Just (ix.visit) }

useArgsAct :: forall r1 r2. Record (RecAct.ProtoArgs ( actions :: Array Action | r1 ) r2) -> NodeProps -> NodeProps
useArgsAct { act } = _ { actions = act.actions }

renderNode :: NodeProps -> Array (M (Array ReactElement)) -> M (Array ReactElement)
renderNode props elemsM = do
  elems <- concat <$> sequence elemsM
  pure
    $ [ DOM.span
          (Props.className # maybeArray props.label)
          elems
      ]
