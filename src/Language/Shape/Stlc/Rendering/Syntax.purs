module Language.Shape.Stlc.Rendering.Syntax where

import Data.Tuple
import Data.Tuple.Nested
import Language.Shape.Stlc.Rendering.Token
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)

import Control.Monad.State (StateT, modify_)
import Control.Monad.State as State
import Control.Monad.Trans.Class (lift)
import Data.Array (concat)
import Data.Array as Array
import Data.Default (default)
import Data.Identity (Identity(..))
import Data.List.Unsafe (List(..))
import Data.List.Unsafe as List
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.Traversable (sequence)
import Effect (Effect)
import Language.Shape.Stlc.Context (Context(..))
import Language.Shape.Stlc.Index (IxDown(..), nilIxDown, toIxDown)
import Language.Shape.Stlc.Metacontext (Metacontext(..))
import Language.Shape.Stlc.Metadata (Name(..))
import Language.Shape.Stlc.Recursor.Action as RecAct
import Language.Shape.Stlc.Recursor.Context as RecCtx
import Language.Shape.Stlc.Recursor.Index (Visit)
import Language.Shape.Stlc.Recursor.Index as RecIx
import Language.Shape.Stlc.Recursor.Metacontext as RecMeta
import Language.Shape.Stlc.Types (Action(..), This)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Union)
import React (ReactElement, getState, modifyState)
import React.DOM as DOM
import React.DOM.Props as Props
import React.SyntheticEvent (stopPropagation)
import Record as Record
import Type.Proxy (Proxy(..))
import Undefined (undefined)

type St
  = { ctx :: Maybe Context
    , goal :: Maybe Type
    , actions :: Array Action
    , holeIds :: List HoleId
    }

_holeIds = Proxy :: Proxy "holeIds"

emptySt :: St
emptySt =
  { ctx: default
  , goal: default
  , actions: []
  , holeIds: Nil
  }

type StM
  = StateT St Identity

type M a
  = StateT (List HoleId) (StateT (Array Action) StM) a

renderProgram :: This -> Effect ((Array ReactElement /\ Array Action) /\ St)
renderProgram this = do
  st <- getState this
  pure $ (\(Identity x) -> x)
    $ flip State.runStateT emptySt
    $ flip State.runStateT []
    $ flip State.evalStateT List.Nil
    $ renderTerm this
        -- TODO: maybe pull this out into multiple files or at least somewhere else?
        { act: {}
        , ctx: { ctx: default, type_: HoleType { holeId: freshHoleId unit, weakening: Set.empty, meta: default } }
        , meta: { meta: default }
        , ix: { visit: { csr: Just st.ix, ix: mempty } }
        , syn: { term: st.term }
        }

renderType :: This -> RecAct.ProtoRec RecAct.ArgsType () StM (Array ReactElement)
renderType this =
  RecAct.recType
    { arrow:
        \args ->
          renderNode this
            ( (useArgsCtx args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "ArrowType" }
            )
            [ renderType this { syn: { type_: args.syn.arrow.dom }, ctx: args.ctx, ix: { visit: args.ix.dom }, meta: { meta: args.meta.dom }, act: {} }
            , pure [ token.arrowType1 ]
            , renderType this { syn: { type_: args.syn.arrow.cod }, ctx: args.ctx, ix: { visit: args.ix.cod }, meta: { meta: args.meta.cod }, act: {} }
            ]
    , data_:
        \args ->
          renderNode this
            ( (useArgsCtx args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "DataType" }
            )
            [ pure $ printTypeId { typeId: args.syn.data_.typeId, meta: args.meta.meta }
            ]
    , hole:
        \args -> do
          lift <<< lift $ modify_ (Record.modify _holeIds (Cons args.syn.hole.holeId))
          renderNode this
            ( (useArgsCtx args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "HoleType" }
            )
            [ printHoleId { holeId: args.syn.hole.holeId, meta: args.meta.meta }
            ]
    }

renderTerm :: This -> RecAct.ProtoRec RecAct.ArgsTerm () StM (Array ReactElement)
renderTerm this =
  RecAct.recTerm
    { lam:
        \args ->
          renderNode this
            ( (useArgsCtx_Term args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "Lam" }
            )
            [ pure [ token.lam1 ]
            , renderTermBind this { syn: { termBind: args.syn.lam.termBind }, ctx: { ctx: args.ctx.ctx }, ix: { visit: args.ix.termBind }, meta: { meta: args.meta.termBind }, act: {} }
            , pure [ token.lam2 ]
            , renderTerm this { syn: { term: args.syn.lam.body }, ctx: args.ctx.body, ix: { visit: args.ix.body }, meta: { meta: args.meta.body }, act: {} }
            ]
    , neu:
        \args ->
          renderNode this
            ( (useArgsCtx_Term args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "Neu" }
            )
            if List.length args.syn.neu.argItems == 0 then
              [ renderTermId this { syn: { termId: args.syn.neu.termId }, ctx: { ctx: args.ctx.termId }, ix: { visit: args.ix.termId }, meta: { meta: args.meta.meta }, act: {} }
              ]
            else
              [ renderTermId this { syn: { termId: args.syn.neu.termId }, ctx: { ctx: args.ctx.termId }, ix: { visit: args.ix.termId }, meta: { meta: args.meta.meta }, act: {} }
              , pure [ token.neu1 ]
              , renderArgItems this { syn: { argItems: args.syn.neu.argItems }, ctx: args.ctx.argItems, ix: { visit: args.ix.argItems }, meta: { meta: args.meta.argItems }, act: {} }
              ]
    , let_:
        \args ->
          renderNode this
            ( (useArgsCtx_Term args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "Let" }
            )
            [ pure [ token.let1 ]
            , renderTermBind this { syn: { termBind: args.syn.let_.termBind }, ctx: { ctx: args.ctx.ctx }, ix: { visit: args.ix.termBind }, meta: { meta: args.meta.termBind }, act: {} }
            , pure [ token.let2 ]
            , renderType this { syn: { type_: args.syn.let_.type_ }, ctx: { ctx: args.ctx.ctx }, ix: { visit: args.ix.type_ }, meta: { meta: args.meta.type_ }, act: {} }
            , pure [ token.let3 ]
            , renderTerm this { syn: { term: args.syn.let_.term }, ctx: args.ctx.term, ix: { visit: args.ix.term }, meta: { meta: args.meta.term }, act: {} }
            , pure [ token.let4 ]
            , renderTerm this { syn: { term: args.syn.let_.body }, ctx: args.ctx.body, ix: { visit: args.ix.body }, meta: { meta: args.meta.body }, act: {} }
            ]
    , buf:
        \args ->
          renderNode this
            ( (useArgsCtx_Term args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "Buf" }
            )
            [ pure [ token.buf1 ]
            , renderTerm this { syn: { term: args.syn.buf.term }, ctx: args.ctx.term, ix: { visit: args.ix.term }, meta: { meta: args.meta.term }, act: {} }
            , pure [ token.buf2 ]
            , renderType this { syn: { type_: args.syn.buf.type_ }, ctx: { ctx: args.ctx.ctx }, ix: { visit: args.ix.type_ }, meta: { meta: args.meta.meta }, act: {} }
            , pure [ token.buf3 ]
            , renderTerm this { syn: { term: args.syn.buf.body }, ctx: args.ctx.body, ix: { visit: args.ix.body }, meta: { meta: args.meta.body }, act: {} }
            ]
    , data_:
        \args ->
          renderNode this
            ( (useArgsCtx_Term args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "Data" }
            )
            [ pure [ token.data1 ]
            , renderTypeBind this { syn: { typeBind: args.syn.data_.typeBind }, ctx: { ctx: args.ctx.ctx }, ix: { visit: args.ix.typeBind }, meta: { meta: args.meta.typeBind }, act: {} }
            , pure [ token.data2 ]
            , renderSumItems this { syn: { sumItems: args.syn.data_.sumItems }, ctx: { ctx: args.ctx.ctx }, ix: { visit: args.ix.sumItems }, meta: { meta: args.meta.sumItems }, act: {} }
            , pure [ token.data3 ]
            , renderTerm this { syn: { term: args.syn.data_.body }, ctx: args.ctx.body, ix: { visit: args.ix.body }, meta: { meta: args.meta.body }, act: {} }
            ]
    , match:
        \args ->
          renderNode this
            ( (useArgsCtx_Term args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "Match" }
            )
            [ pure [ token.match1 ]
            , renderTerm this { syn: { term: args.syn.match.term }, ctx: args.ctx.term, ix: { visit: args.ix.term }, meta: { meta: args.meta.term }, act: {} }
            , pure [ token.match2 ]
            , renderCaseItems this { syn: { caseItems: args.syn.match.caseItems }, ctx: args.ctx.caseItems, ix: { visit: args.ix.caseItems }, meta: { meta: args.meta.caseItems }, act: {} }
            ]
    , hole:
        \args ->
          renderNode this
            ( (useArgsCtx_Term args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "Hole" }
            )
            [ renderType this { syn: { type_: args.ctx.type_ }, ctx: { ctx: args.ctx.ctx }, ix: { visit: { csr: Nothing, ix: Nothing } }, meta: { meta: args.meta.meta }, act: {} }
            ]
    }

renderArgItems :: This -> RecAct.ProtoRec RecAct.ArgsArgItems () StM (Array ReactElement)
renderArgItems this =
  (List.foldl append [] <$> _)
    <<< RecAct.recArgItems
        { argItem:
            \args ->
              renderNode this
                ( (useArgsCtx args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                    { label = Just "ArgItem" }
                )
                [ pure $ newline args.meta.meta (unwrap args.syn.argItem.meta).indented
                , renderTerm this { syn: { term: args.syn.argItem.term }, ctx: args.ctx.argItem, ix: { visit: args.ix.argItem }, meta: { meta: args.meta.meta }, act: {} }
                ]
        }

renderSumItems :: This -> RecAct.ProtoRec RecAct.ArgsSumItems () StM (Array ReactElement)
renderSumItems this =
  (List.foldl append [] <$> _)
    <<< RecAct.recSumItems
        { sumItem:
            \args ->
              renderNode this
                ( (useArgsCtx args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                    { label = Just "SumItem" }
                )
                [ pure $ newline args.meta.meta (unwrap args.syn.sumItem.meta).indented
                , pure [ token.sumItem1 ]
                , renderTermBind this { syn: { termBind: args.syn.sumItem.termBind }, ctx: { ctx: args.ctx.ctx }, ix: { visit: args.ix.termBind }, meta: { meta: args.meta.termBind }, act: {} }
                , pure [ token.sumItem2 ]
                , renderParamItems this { syn: { paramItems: args.syn.sumItem.paramItems }, ctx: { ctx: args.ctx.ctx }, ix: { visit: args.ix.paramItems }, meta: { meta: args.meta.paramItems }, act: {} }
                ]
        }

renderCaseItems :: This -> RecAct.ProtoRec RecAct.ArgsCaseItems () StM (Array ReactElement)
renderCaseItems this =
  (List.foldl append [] <$> _)
    <<< RecAct.recCaseItems
        { caseItem:
            \args ->
              renderNode this
                ( (useArgsCtx args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                    { label = Just "CaseItem" }
                )
                [ pure $ newline args.meta.meta (unwrap args.syn.caseItem.meta).indented
                , pure [ token.caseItem1 ]
                , renderTermBindItems this { syn: { termBindItems: args.syn.caseItem.termBindItems }, ctx: { ctx: args.ctx.ctx }, ix: { visit: args.ix.termBindItems }, meta: { meta: args.meta.termBindItems }, act: {} }
                , pure [ token.caseItem2 ]
                , renderTerm this { syn: { term: args.syn.caseItem.body }, ctx: args.ctx.body, ix: { visit: args.ix.body }, meta: { meta: args.meta.body }, act: {} }
                ]
        }

renderParamItems :: This -> RecAct.ProtoRec RecAct.ArgsParamItems () StM (Array ReactElement)
renderParamItems this =
  (List.foldl append [] <$> _)
    <<< RecAct.recParamItems
        { paramItem:
            \args ->
              renderNode this
                ( (useArgsCtx args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                    { label = Just "ParamItem" }
                )
                [ pure $ newline args.meta.meta (unwrap args.syn.paramItem.meta).indented
                , renderType this { syn: { type_: args.syn.paramItem.type_ }, ctx: { ctx: args.ctx.ctx }, ix: { visit: args.ix.type_ }, meta: { meta: args.meta.meta }, act: {} }
                ]
        }

renderTermBindItems :: This -> RecAct.ProtoRec RecAct.ArgsTermBindItems () StM (Array ReactElement)
renderTermBindItems this =
  (List.foldl append [] <$> _)
    <<< RecAct.recTermBindItems
        { termBindItem:
            \args ->
              renderNode this
                ( (useArgsCtx args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                    { label = Just "TermBindItem" }
                )
                [ pure $ newline args.meta.meta (unwrap args.syn.termBindItem.meta).indented
                , renderTermBind this { syn: { termBind: args.syn.termBindItem.termBind }, ctx: { ctx: args.ctx.ctx }, ix: { visit: args.ix.termBind }, meta: { meta: args.meta.meta }, act: {} }
                ]
        }

renderTermBind :: This -> RecAct.ProtoRec RecAct.ArgsTermBind () StM (Array ReactElement)
renderTermBind this =
  RecAct.recTermBind
    { termBind:
        \args ->
          renderNode this
            ( (useArgsCtx args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "TermBind" }
            )
            [ pure $ printTermId { termId: args.syn.termBind.termId, meta: args.meta.meta } ]
    }

renderTypeBind :: This -> RecAct.ProtoRec RecAct.ArgsTypeBind () StM (Array ReactElement)
renderTypeBind this =
  RecAct.recTypeBind
    { typeBind:
        \args ->
          renderNode this
            ( (useArgsCtx args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "TypeBind" }
            )
            [ pure $ printTypeId { typeId: args.syn.typeBind.typeId, meta: args.meta.meta } ]
    }

renderTypeId :: This -> RecAct.ProtoRec RecAct.ArgsTypeId () StM (Array ReactElement)
renderTypeId this =
  RecAct.recTypeId
    { typeId:
        \args ->
          renderNode this
            ( (useArgsCtx args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "TypeId" }
            )
            [ pure $ printTypeId { typeId: args.syn.typeId, meta: args.meta.meta } ]
    }

renderTermId :: This -> RecAct.ProtoRec RecAct.ArgsTermId () StM (Array ReactElement)
renderTermId this =
  RecAct.recTermId
    { termId:
        \args ->
          renderNode this
            ( (useArgsCtx args $ useArgsIx args $ useArgsAct args $ defaultNodeProps)
                { label = Just "TermId" }
            )
            [ pure $ printTermId { termId: args.syn.termId, meta: args.meta.meta } ]
    }

printTypeId :: { typeId :: TypeId, meta :: Metacontext } -> Array ReactElement
printTypeId { typeId, meta } =
  [ DOM.span [ Props.className "typeId" ]
      $ printName name
      <> printShadow shadow
  ]
  where
  name = case Map.lookup typeId (unwrap meta).dataNames of
    Just name -> name
    Nothing -> unsafeCrashWith $ "could not find name of type id " <> show typeId <> " in metacontext " <> show meta

  shadow = case Map.lookup name (unwrap meta).dataShadows of
    Just i -> i
    Nothing -> unsafeCrashWith $ "could not find shadow of data name " <> show name <> " in metacontext " <> show meta

printTermId :: { termId :: TermId, meta :: Metacontext } -> Array ReactElement
printTermId { termId, meta } =
  [ DOM.span [ Props.className "termId" ]
      $ printName name
      <> printShadow shadow
  ]
  where
  name = case Map.lookup termId (unwrap meta).varNames of
    Just name -> name
    Nothing -> unsafeCrashWith $ "could not find name of term id " <> show termId <> " in metacontext " <> show meta

  shadow = case Map.lookup name (unwrap meta).varShadows of
    Just i -> i
    Nothing -> unsafeCrashWith $ "could not find shadow of var name " <> show name <> " in metacontext " <> show meta

printHoleId :: { holeId :: HoleId, meta :: Metacontext } -> M (Array ReactElement)
printHoleId args = do
  mb_i <- List.findIndex (args.holeId == _) <$> State.get
  case mb_i of
    Just i -> pure [ DOM.span [ Props.className "holeId" ] [ DOM.text $ "?" <> show i ] ]
    Nothing -> unsafeCrashWith $ "count not find index of holeId " <> show args.holeId <> " in metacontext " <> show args.meta

printName :: Name -> Array ReactElement
printName (Name mb_str) = case mb_str of
  Just str -> [ DOM.span [ Props.className "name" ] [ DOM.text str ] ]
  Nothing -> [ DOM.span [ Props.className "name discarded" ] [ DOM.text "_" ] ]

printShadow :: Int -> Array ReactElement
printShadow shadow =
  if 0 < shadow then
    [ DOM.span [ Props.className "shadow" ] [ DOM.text (show shadow) ] ]
  else
    []

maybeArray :: forall a b. Maybe a -> (a -> b) -> Array b
maybeArray ma f = maybe [] (Array.singleton <<< f) ma

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

defaultNodeProps :: NodeProps
defaultNodeProps =
  { label: default
  , visit: default
  , type_: default
  , ctx: default
  , meta: default
  , actions: []
  }

type NodeProps
  = { label :: Maybe String
    , visit :: Maybe Visit
    , type_ :: Maybe Type
    , ctx :: Maybe Context
    , meta :: Maybe Metacontext
    , actions :: Array Action
    }

renderNode :: This -> NodeProps -> Array (M (Array ReactElement)) -> M (Array ReactElement)
renderNode this props elemsM = do
  -- if this node is selected
  when (join (props.visit <#> _.csr) == Just nilIxDown) do
    -- update environment
    lift <<< lift
      $ modify_
          ( _
              { goal = props.type_
              , ctx = props.ctx
              , actions = props.actions
              }
          )
  -- render children
  elems <- concat <$> sequence elemsM
  pure $ [ DOM.span propsSpan elems ]
  where
  propsSpan =
    concat
      [ maybeArray props.label (\label -> Props.className $ "node " <> label)
      , maybeArray (join $ props.visit <#> _.ix) \ix ->
          Props.onClick \event -> do
            stopPropagation event
            -- select this node
            modifyState this (_ { ix = toIxDown ix })
      ]
