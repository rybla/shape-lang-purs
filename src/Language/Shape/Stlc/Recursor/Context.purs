module Language.Shape.Stlc.Recursor.Context where

import Data.Function.Utility
import Data.Tuple.Nested
import Language.Shape.Stlc.Context
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Prim.Row
import Record
import Data.Default (default)
import Data.List.Unsafe (List, foldl, index', zip)
import Language.Shape.Stlc.Recursion.Syntax as Rec
import Language.Shape.Stlc.Recursor.Record (modifyHetero)
import Partial.Unsafe (unsafeCrashWith)
import Prim as Prim
import Type.Proxy (Proxy(..))
import Undefined (undefined)

-- | ProtoRec
type ProtoArgs r1 r2
  = ( argsCtx :: Record ( ctx :: Context | r1 ) | r2 )

type ProtoRec args r a
  = Rec.ProtoRec args r a

_argsCtx = Proxy :: Proxy "argsCtx"

-- | recType
type ProtoArgsType r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsType r
  = Rec.ArgsType (ProtoArgsType () r)

type ArgsArrowType r
  = Rec.ArgsArrowType (ProtoArgsType () r)

type ArgsDataType r
  = Rec.ArgsDataType (ProtoArgsType () r)

type ArgsHoleType r
  = Rec.ArgsHoleType (ProtoArgsType () r)

recType ::
  forall r a.
  Lacks "argsSyn" r =>
  Lacks "argsCtx" r =>
  { arrow :: ProtoRec ArgsArrowType r a, data_ :: ProtoRec ArgsDataType r a, hole :: ProtoRec ArgsHoleType r a } ->
  ProtoRec ArgsType r a
recType rec =
  Rec.recType
    { arrow: \args -> rec.arrow args
    , data_: \args -> rec.data_ args
    , hole: \args -> rec.hole args
    }

-- | recTerm
type ProtoArgsTerm r1 r2
  = ProtoArgs ( type_ :: Type | r1 ) r2

type ArgsTerm r
  = Rec.ArgsTerm (ProtoArgsTerm () r)

type ArgsLam r
  = Rec.ArgsLam (ProtoArgsTerm ( type_dom :: Type, ctx_body :: Context, type_body :: Type ) r)

type ArgsNeu r
  = Rec.ArgsNeu (ProtoArgsTerm ( type_id :: Type, types_args :: List Type ) r)

type ArgsLet r
  = Rec.ArgsLet (ProtoArgsTerm ( ctx_body :: Context ) r)

type ArgsBuf r
  = Rec.ArgsBuf (ProtoArgsTerm () r)

type ArgsData r
  = Rec.ArgsData (ProtoArgsTerm ( ctx_body :: Context ) r)

type ArgsMatch r
  = Rec.ArgsMatch (ProtoArgsTerm ( ctx_caseItems :: List Context ) r)

type ArgsHole r
  = Rec.ArgsHole (ProtoArgsTerm () r)

recTerm ::
  forall r a.
  Lacks "argsSyn" r =>
  Lacks "argsCtx" r =>
  { lam :: ProtoRec ArgsLam r a, neu :: ProtoRec ArgsNeu r a, let_ :: ProtoRec ArgsLet r a, buf :: ProtoRec ArgsBuf r a, data_ :: ProtoRec ArgsData r a, match :: ProtoRec ArgsMatch r a, hole :: ProtoRec ArgsHole r a } ->
  ProtoRec ArgsTerm r a
recTerm rec =
  Rec.recTerm
    { lam:
        \args@{ argsSyn: { lam }, argsCtx: { ctx, type_ } } -> case type_ of
          ArrowType { dom, cod } -> rec.lam $ modifyHetero _argsCtx (union { type_dom: dom, ctx_body: insertVarType lam.termBind.termId dom ctx, type_body: cod }) args
          _ -> unsafeCrashWith "badly typed lambda"
    , neu:
        \args@{ argsSyn: { neu }, argsCtx: { ctx } } ->
          let
            type_id = lookupVarType neu.termId ctx

            { doms: types_args } = flattenType type_id
          in
            rec.neu $ modifyHetero _argsCtx (union { type_id, types_args }) args
    , let_:
        \args@{ argsSyn: { let_ }, argsCtx: { ctx } } ->
          rec.let_ $ modifyHetero _argsCtx (union { ctx_body: insertVarType let_.termBind.termId let_.type_ ctx }) args
    , buf: rec.buf
    , data_:
        \args@{ argsSyn: { data_ }, argsCtx: { ctx } } ->
          rec.data_
            $ modifyHetero _argsCtx
                ( union
                    { ctx_body:
                        flipfoldr data_.sumItems
                          ( \sumItem ->
                              insertVarType sumItem.termBind.termId (typeOfSumItem data_.typeBind.typeId sumItem)
                                <<< insertConstrDataType sumItem.termBind.termId { typeId: data_.typeBind.typeId, meta: default }
                          )
                          $ insertData data_
                          $ ctx
                    }
                )
                args
    , match:
        \args@{ argsSyn: { match }, argsCtx: { ctx, type_ } } ->
          let
            data_ = lookupData match.typeId ctx
          in
            rec.match
              $ modifyHetero _argsCtx
                  ( union
                      { ctx_caseItems:
                          map
                            ( \(caseItem /\ sumItem) ->
                                foldl
                                  (flip \(termBindItem /\ paramItem) -> insertVarType termBindItem.termBind.termId paramItem.type_)
                                  ctx
                                  (zip caseItem.termBindItems sumItem.paramItems)
                            )
                            (zip match.caseItems (data_.sumItems))
                      }
                  )
                  args
    , hole: rec.hole
    }

-- -- | recArgItems
-- type ProtoArgsArgItems r1 r2
--   = ProtoArgs r1 r2
-- type ArgsArgItems r
--   = Rec.ArgsArgItems (ProtoArgsArgItems ( type_ :: Type ) r)
-- type ArgsArgItemsCons r
--   = Rec.ArgsArgItemsCons (ProtoArgsArgItems ( type_argItem :: Type, type_argItems :: Type ) r)
-- type ArgsArgItemsNil r
--   = Rec.ArgsArgItemsNil (ProtoArgsArgItems ( type_ :: Type ) r)
-- recArgItems ::
--   forall r a.
--   Lacks "argsSyn" r =>
--   Lacks "argsCtx" r =>
--   { cons :: ProtoRec ArgsArgItemsCons r a, nil :: ProtoRec ArgsArgItemsNil r a } ->
--   ProtoRec ArgsArgItems r a
-- recArgItems rec =
--   Rec.recArgItems
--     { cons:
--         rec.cons
--           <<< modifyHetero _argsCtx
--               ( \{ ctx, type_ } -> case type_ of
--                   ArrowType arrow -> { ctx, type_argItem: arrow.dom, type_argItems: arrow.cod }
--                   _ -> unsafeCrashWith "term of non-arrow type applied as if it was a function"
--               )
--     , nil:
--         rec.nil
--     }
-- | recArgItems
type ProtoArgsArgItems r1 r2
  = ProtoArgs ( doms :: List Type, cod :: Type | r1 ) r2

type ArgsArgItems r
  = Rec.ArgsArgItems (ProtoArgsArgItems () r)

type ArgsArgItem r
  = Rec.ArgsArgItem (ProtoArgsArgItems ( type_argItem :: Type ) r)

recArgItems ::
  forall r a.
  Lacks "argsSyn" r =>
  Lacks "argsCtx" r =>
  { argItem :: ProtoRec ArgsArgItem r a } ->
  ProtoRec ArgsArgItems r (List a)
recArgItems rec = Rec.recArgItems { argItem: \args@{ argsSyn, argsCtx } -> rec.argItem $ modifyHetero _argsCtx (union { type_argItem: index' argsCtx.doms argsSyn.i }) args }

-- | recSumItems
type ProtoArgsSumItems r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsSumItems r
  = Rec.ArgsSumItems (ProtoArgsSumItems () r)

type ArgsSumItem r
  = Rec.ArgsSumItem (ProtoArgsSumItems () r)

recSumItems ::
  forall r a.
  Lacks "argsSyn" r =>
  Lacks "argsCtx" r =>
  { sumItem :: ProtoRec ArgsSumItem r a } ->
  ProtoRec ArgsSumItems r (List a)
recSumItems = Rec.recSumItems

-- | recCaseItem
type ProtoArgsCaseItems r1 r2
  = ProtoArgs ( typeId :: TypeId | r1 ) r2

type ArgsCaseItems r
  = Rec.ArgsCaseItems (ProtoArgsCaseItems () r)

type ArgsCaseItem r
  = Rec.ArgsCaseItem (ProtoArgsCaseItems () r)

recCaseItems ::
  forall r a.
  Lacks "argsSyn" r =>
  Lacks "argsCtx" r =>
  { caseItem :: ProtoRec ArgsCaseItem r a } ->
  ProtoRec ArgsCaseItems r (List a)
-- TODO: for each caseItem, add termBindItems into context for body
recCaseItems rec =
  Rec.recCaseItems
    { caseItem:
        \args@{ argsSyn, argsCtx } ->
          rec.caseItem
            $ modifyHetero _argsCtx
                (flipfoldr (zip argsSyn.caseItem.termBindItems (lookupData argsCtx.typeId argsCtx.ctx).sumItems) (\({ termBind } /\ sumItem) -> modify (Proxy :: Proxy "ctx") (insertVarType termBind.termId (typeOfSumItem argsCtx.typeId sumItem))))
                args
    }

-- | recParamItems
type ProtoArgsParamItems r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsParamItems r
  = Rec.ArgsParamItems (ProtoArgsParamItems () r)

type ArgsParamItem r
  = Rec.ArgsParamItem (ProtoArgsParamItems () r)

recParamItems ::
  forall r a.
  Lacks "argsSyn" r =>
  Lacks "argsCtx" r =>
  { paramItem :: ProtoRec ArgsParamItem r a } ->
  ProtoRec ArgsParamItems r (List a)
recParamItems rec = Rec.recParamItems undefined

-- | recTermBindItems
type ProtoArgsTermBindItems r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsTermBindItems r
  = Rec.ArgsTermBindItems (ProtoArgsTermBindItems () r)

type ArgsTermBindItem r
  = Rec.ArgsTermBindItem (ProtoArgsTermBindItems () r)

recTermBindItems ::
  forall r a.
  Lacks "argsSyn" r =>
  Lacks "argsCtx" r =>
  { termBindItem :: ProtoRec ArgsTermBindItem r a } ->
  ProtoRec ArgsTermBindItems r (List a)
recTermBindItems = Rec.recTermBindItems

-- | recTermBind
type ArgsTermBind r
  = Rec.ArgsTermBind (ProtoArgs () r)

recTermBind ::
  forall r a.
  Lacks "argsSyn" r =>
  Lacks "argsCtx" r =>
  { termBind :: ProtoRec ArgsTermBind r a } ->
  ProtoRec ArgsTermBind r a
recTermBind rec = Rec.recTermBind { termBind: rec.termBind }

-- | recTypeBind
type ArgsTypeBind r
  = Rec.ArgsTypeBind (ProtoArgs () r)

recTypeBind ::
  forall r a.
  Lacks "argsSyn" r =>
  Lacks "argsCtx" r =>
  { typeBind :: ProtoRec ArgsTypeBind r a } ->
  ProtoRec ArgsTypeBind r a
recTypeBind rec = Rec.recTypeBind { typeBind: rec.typeBind }
