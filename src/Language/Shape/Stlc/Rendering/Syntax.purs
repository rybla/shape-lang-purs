module Language.Shape.Stlc.Rendering.Syntax where

import Data.Tuple
import Data.Tuple.Nested
import Language.Shape.Stlc.Rendering.Token
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Control.Monad.State (State)
import Control.Monad.State as State
import Data.Array (concat)
import Data.Array as Array
import Data.Default (default)
import Data.List.Unsafe (List(..))
import Data.List.Unsafe as List
import Data.Map.Unsafe as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String (joinWith)
import Data.Traversable (sequence)
import Debug as Debug
import Effect (Effect)
import Effect.Console as Console
import Language.Shape.Stlc.Context (Context(..))
import Language.Shape.Stlc.Index (IxDown(..), nilIxDown, nilIxUp, toIxDown)
import Language.Shape.Stlc.Metacontext (Metacontext(..))
import Language.Shape.Stlc.Metadata (Name(..))
import Language.Shape.Stlc.Recursor.Action as Rec
import Language.Shape.Stlc.Recursor.Context as RecCtx
import Language.Shape.Stlc.Recursor.Index (Visit, nilVisit, nonVisit)
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

type RenderEnvironment
  = { ctx :: Maybe Context
    , meta :: Metacontext
    , goal :: Maybe Type
    , actions :: Array Action
    , holeIds :: List HoleId
    }

_holeIds = Proxy :: Proxy "holeIds"

emptyRenderEnvironment :: RenderEnvironment
emptyRenderEnvironment =
  { ctx: default
  , meta: default
  , goal: default
  , actions: []
  , holeIds: Nil
  }

type M a
  = State RenderEnvironment a

renderProgram :: This -> Effect (Array ReactElement /\ RenderEnvironment)
renderProgram this = do
  st <- getState this
  pure
    $ flip State.runState emptyRenderEnvironment
    $ renderTerm this
        -- TODO: maybe pull this out into multiple files or at least somewhere else?
        { term: st.term
        , ctx: default
        , goal: HoleType { holeId: freshHoleId unit, weakening: Set.empty, meta: default }
        , visit: nilVisit (Just st.ix)
        , meta: default
        }

renderType :: This -> Record (Rec.ArgsType ()) -> M (Array ReactElement)
renderType this =
  Rec.recType
    { arrowType:
        \args ->
          renderNode this
            ( (makeNodeProps args)
                { label = Just "ArrowType" }
            )
            [ -- renderType this ?a -- args.dom
            -- , pure [ token.arrowType1 ]
            -- , renderType this ?a -- args.cod
            ]
    , dataType:
        \args ->
          renderNode this
            ( (makeNodeProps args)
                { label = Just "DataType" }
            )
            [ -- pure $ printTypeId { typeId: args.data_.typeId, meta: args.typeId.meta }
            ]
    , holeType:
        \args -> do
          State.modify_ (Record.modify _holeIds (Cons args.holeType.holeId))
          renderNode this
            ( (makeNodeProps args)
                { label = Just "HoleType" }
            )
            [ -- printHoleId { holeId: args.hole.holeId, meta: args.holeId.meta }
            ]
    }
  where
  makeNodeProps :: forall r. { | r } -> NodeProps
  makeNodeProps args = undefined

renderTerm :: This -> Record (Rec.ArgsTerm ()) -> M (Array ReactElement)
renderTerm this =
  Rec.recTerm
    { lam:
        \args ->
          renderNode this
            ( (makeNodeProps args)
                { label = Just "Lam" }
            )
            [ pure [ token.lam1 ]
            , undefined -- renderTermBind this args.termBind
            , pure [ token.lam2 ]
            , renderTerm this args.body
            ]
    , neu:
        \args ->
          renderNode this
            ( (makeNodeProps args)
                { label = Just "Neu" }
            )
            if List.length args.neu.argItems == 0 then
              [ undefined -- renderTermId this args.termId
              ]
            else
              [ undefined -- renderTermId this args.termId
              , pure [ token.neu1 ]
              , undefined -- renderArgItems this args.argItems
              ]
    , let_:
        \args ->
          renderNode this
            ( (makeNodeProps args)
                { label = Just "Let" }
            )
            [ pure [ token.let1 ]
            , undefined -- renderTermBind this args.termBind
            , pure [ token.let2 ]
            , renderType this args.type_
            , pure [ token.let3 ]
            , renderTerm this args.term
            , pure [ token.let4 ]
            , renderTerm this args.body
            ]
    , buf:
        \args ->
          renderNode this
            ( (makeNodeProps args)
                { label = Just "Buf" }
            )
            [ pure [ token.buf1 ]
            , renderTerm this args.term
            , pure [ token.buf2 ]
            , renderType this args.type_
            , pure [ token.buf3 ]
            , renderTerm this args.body
            ]
    , data_:
        \args ->
          renderNode this
            ( (makeNodeProps args)
                { label = Just "Data" }
            )
            [ pure [ token.data1 ]
            , undefined -- renderTypeBind this args.typeBind
            , pure [ token.data2 ]
            , undefined -- renderSumItems this args.sumItems
            , pure [ token.data3 ]
            , renderTerm this args.body
            ]
    , match:
        \args ->
          renderNode this
            ( (makeNodeProps args)
                { label = Just "Match" }
            )
            [ pure [ token.match1 ]
            , renderTerm this args.term
            , pure [ token.match2 ]
            , undefined -- renderCaseItems this args.caseItems
            ]
    , hole:
        \args ->
          (\elems -> [ DOM.span [ Props.className "hole-container" ] elems ])
            <$> renderNode this
                ( (makeNodeProps args)
                    { label = Just "Hole" }
                )
                [ renderType this { type_: args.goal, ctx: args.ctx, visit: nonVisit, meta: args.meta }
                ]
    }
  where
  makeNodeProps :: forall r. {|r} -> NodeProps
  makeNodeProps args = undefined

-- renderArgItems :: This -> Record Rec.ArgsArgItems -> M (Array ReactElement)
-- renderArgItems this =
--   renderConcatList
--     <<< Rec.recArgItems
--         { argItem:
--             \args ->
--               renderNode this
--                 ( defaultNodeProps
--                     { label = Just "ArgItem"
--                     , visit = Just args.ix.argItem
--                     , ctx = Just args.ctx.argItem.ctx
--                     , meta = args.meta.meta
--                     , actions = args.act.actions
--                     }
--                 )
--                 [ pure $ newline args.meta.meta (unwrap args.argItem.meta).indented
--                 , renderTerm this { syn: { term: args.argItem.term }, ctx: args.ctx.argItem, ix: { visit: args.ix.argItem }, meta: { meta: args.meta.meta }, act: {} }
--                 ]
--         }
-- renderSumItems :: This -> Record Rec.ArgsSumItems -> M (Array ReactElement)
-- renderSumItems this =
--   renderConcatList
--     <<< Rec.recSumItems
--         { sumItem:
--             \args ->
--               renderNode this
--                 ( defaultNodeProps
--                     { label = Just "SumItem"
--                     , visit = Just args.ix.sumItem
--                     , ctx = Just args.ctx.ctx
--                     , meta = args.meta.meta
--                     , actions = args.act.actions
--                     }
--                 )
--                 [ pure $ newline args.meta.meta (unwrap args.sumItem.meta).indented
--                 , pure [ token.sumItem1 ]
--                 , renderTermBind this { syn: { termBind: args.sumItem.termBind }, ctx: { ctx: args.ctx.ctx }, ix: { visit: args.ix.termBind }, meta: { meta: args.meta.termBind }, act: {} }
--                 , pure [ token.sumItem2 ]
--                 , renderParamItems this { syn: { paramItems: args.sumItem.paramItems }, ctx: { ctx: args.ctx.ctx }, ix: { visit: args.ix.paramItems }, meta: { meta: args.meta.paramItems }, act: {} }
--                 ]
--         }
-- renderCaseItems :: This -> Record Rec.ArgsCaseItems -> M (Array ReactElement)
-- renderCaseItems this =
--   renderConcatList
--     <<< Rec.recCaseItems
--         { caseItem:
--             \args ->
--               renderNode this
--                 ( defaultNodeProps
--                     { label = Just "CaseItem"
--                     , visit = Just args.ix.caseItem
--                     , ctx = Just args.ctx.caseItem.ctx
--                     , meta = args.meta.meta
--                     , actions = args.act.actions
--                     }
--                 )
--                 [ pure $ newline args.meta.meta (unwrap args.caseItem.meta).indented
--                 , pure [ token.caseItem1 ]
--                 , renderTermBindItems this { syn: { termBindItems: args.caseItem.termBindItems }, ctx: { ctx: args.ctx.ctx }, ix: { visit: args.ix.termBindItems }, meta: { meta: args.meta.termBindItems }, act: {} }
--                 , pure [ token.caseItem2 ]
--                 , renderTerm this { syn: { term: args.caseItem.body }, ctx: args.ctx.body, ix: { visit: args.ix.body }, meta: { meta: args.meta.body }, act: {} }
--                 ]
--         }
-- renderParamItems :: This -> Record Rec.ArgsParamItems -> M (Array ReactElement)
-- renderParamItems this =
--   renderConcatList
--     <<< Rec.recParamItems
--         { paramItem:
--             \args ->
--               renderNode this
--                 ( defaultNodeProps
--                     { label = Just "ParamItem"
--                     , visit = Just args.ix.paramItem
--                     , ctx = Just args.ctx.paramItem
--                     , meta = args.meta.paramItem
--                     , actions = args.act.actions
--                     }
--                 )
--                 [ pure $ newline args.meta.meta (unwrap args.paramItem.meta).indented
--                 , renderType this { syn: { type_: args.paramItem.type_ }, ctx: { ctx: args.ctx.ctx }, ix: { visit: args.ix.type_ }, meta: { meta: args.meta.meta }, act: {} }
--                 ]
--         }
-- renderTermBindItems :: This -> Record Rec.ArgsTermBindItems -> M (Array ReactElement)
-- renderTermBindItems this =
--   renderConcatList
--     <<< Rec.recTermBindItems
--         { termBindItem:
--             \args ->
--               renderNode this
--                 ( defaultNodeProps
--                     { label = Just "TermBindItem"
--                     , visit = Just args.ix.termBindItem
--                     , ctx = Just args.ctx.termBindItem
--                     , meta = args.meta.meta
--                     , actions = args.act.actions
--                     }
--                 )
--                 [ pure $ newline args.meta.meta (unwrap args.termBindItem.meta).indented
--                 , renderTermBind this { syn: { termBind: args.termBindItem.termBind }, ctx: { ctx: args.ctx.ctx }, ix: { visit: args.ix.termBind }, meta: { meta: args.meta.meta }, act: {} }
--                 ]
--         }
-- renderTermBind :: This -> Record Rec.ArgsTermBind -> M (Array ReactElement)
-- renderTermBind this =
--   Rec.recTermBind
--     { termBind:
--         \args ->
--           renderNode this
--             ( (useArgsCtx args $ useArgsIx args $ useArgsMeta args $ useArgsAct args $ defaultNodeProps)
--                 { label = Just "TermBind" }
--             )
--             [ pure $ printTermId { termId: args.termBind.termId, meta: args.meta.meta } ]
--     }
-- renderTypeBind :: This -> Record Rec.ArgsTypeBind -> M (Array ReactElement)
-- renderTypeBind this =
--   Rec.recTypeBind
--     { typeBind:
--         \args ->
--           renderNode this
--             ( (useArgsCtx args $ useArgsIx args $ useArgsMeta args $ useArgsAct args $ defaultNodeProps)
--                 { label = Just "TypeBind" }
--             )
--             [ pure $ printTypeId { typeId: args.typeBind.typeId, meta: args.meta.meta } ]
--     }
-- renderTypeId :: This -> Record Rec.ArgsTypeId -> M (Array ReactElement)
-- renderTypeId this =
--   Rec.recTypeId
--     { typeId:
--         \args ->
--           renderNode this
--             ( (useArgsCtx args $ useArgsIx args $ useArgsMeta args $ useArgsAct args $ defaultNodeProps)
--                 { label = Just "TypeId" }
--             )
--             [ pure $ printTypeId { typeId: args.typeId, meta: args.meta.meta } ]
--     }
-- renderTermId :: This -> Record Rec.ArgsTermId -> M (Array ReactElement)
-- renderTermId this =
--   Rec.recTermId
--     { termId:
--         \args ->
--           renderNode this
--             ( (useArgsCtx args $ useArgsIx args $ useArgsMeta args $ useArgsAct args $ defaultNodeProps)
--                 { label = Just "TermId" }
--             )
--             [ pure $ printTermId { termId: args.termId, meta: args.meta.meta } ]
--     }
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
  mb_i <- List.findIndex (args.holeId == _) <$> State.gets _.holeIds
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

-- useArgsCtx :: forall r1 r2. Record (RecCtx.ProtoArgsType r1 r2) -> NodeProps -> NodeProps
-- useArgsCtx { ctx } = _ { ctx = Just ctx.ctx }
-- useArgsCtx_Term :: forall r1 r2. Record (RecCtx.ProtoArgsTerm r1 r2) -> NodeProps -> NodeProps
-- useArgsCtx_Term { ctx } = _ { ctx = Just ctx.ctx, type_ = Just ctx.type_ }
-- useArgsMeta :: forall r1 r2. { meta :: { meta :: Metacontext | r1 } | r2 } -> NodeProps -> NodeProps
-- useArgsMeta { meta: { meta: meta } } = _ { meta = meta }
-- useArgsIx :: forall r1 r2. Record (RecIx.ProtoArgs r1 r2) -> NodeProps -> NodeProps
-- useArgsIx { ix } = _ { visit = Just (ix.visit) }
-- useArgsAct :: forall r1 r2. Record (Rec.ProtoArgs ( actions :: Array Action | r1 ) r2) -> NodeProps -> NodeProps
-- useArgsAct { act } = _ { actions = act.actions }
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
    , meta :: Metacontext
    , actions :: Array Action
    }

renderNode :: This -> NodeProps -> Array (M (Array ReactElement)) -> M (Array ReactElement)
renderNode this props elemsM = do
  -- if this node is selected
  when isSelected do
    Debug.traceM $ "================================="
    Debug.traceM $ "renderNode isSelected"
    Debug.traceM $ "label = " <> show props.label
    Debug.traceM $ "meta  = " <> show props.meta
    Debug.traceM $ "================================="
    -- update environment
    State.modify_
      ( _
          { goal = props.type_
          , ctx = props.ctx
          , actions = props.actions
          , meta = props.meta
          }
      )
  -- render children
  elems <- concat <$> sequence elemsM
  pure $ [ DOM.span propsSpan elems ]
  where
  isSelected = (props.visit <#> _.csr) == Just (Just nilIxDown)

  propsSpan =
    concat
      [ maybeArray props.label \label ->
          Props.className $ joinWith " " [ "node", label, if isSelected then "selected" else "" ]
      , maybeArray (join $ props.visit <#> _.ix) \ix ->
          Props.onClick \event -> do
            Console.log "clicked on a node"
            stopPropagation event
            -- select this node
            modifyState this (_ { ix = toIxDown ix })
      ]

renderConcatList :: List (M (Array ReactElement)) -> M (Array ReactElement)
renderConcatList = (List.foldl append [] <$> _) <<< sequence

renderConcatArray :: Array (M (Array ReactElement)) -> M (Array ReactElement)
renderConcatArray = (Array.foldl append [] <$> _) <<< sequence

maybeArray :: forall a b. Maybe a -> (a -> b) -> Array b
maybeArray ma f = maybe [] (Array.singleton <<< f) ma
