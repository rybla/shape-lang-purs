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
  = ( ctx :: Record (r1) | r2 )

type ProtoRec args r a
  = Rec.ProtoRec args r a

_ctx = Proxy :: Proxy "ctx"

type CtxWithGoal r
  = { context :: Context, type_ :: Type | r }

type CtxWithoutGoal r
  = { context :: Context | r }

toCtxWithoutGoal :: forall r. CtxWithoutGoal r -> CtxWithoutGoal ()
toCtxWithoutGoal { context } = { context }

-- | recType
type ProtoArgsType r1 r2
  = ProtoArgs ( here :: CtxWithoutGoal () | r1 ) r2

type ArgsType r
  = Rec.ArgsType (ProtoArgsType () r)

type ArgsArrowType r
  = Rec.ArgsArrowType (ProtoArgsType ( dom :: Record (ArgsType r), cod :: Record (ArgsType r) ) r)

type ArgsDataType r
  = Rec.ArgsDataType (ProtoArgsType () r)

type ArgsHoleType r
  = Rec.ArgsHoleType (ProtoArgsType () r)

recType ::
  forall r a.
  Lacks "syn" r =>
  Lacks "ctx" r =>
  { arrow :: ProtoRec ArgsArrowType r a, data_ :: ProtoRec ArgsDataType r a, hole :: ProtoRec ArgsHoleType r a } ->
  ProtoRec ArgsType r a
recType rec =
  Rec.recType
    { arrow: \args -> rec.arrow $ args {syn = ?a, ctx = ?a}
    , data_: \args -> undefined -- rec.data_ args
    , hole: \args -> undefined -- rec.hole args
    }

-- | recTerm
type ProtoArgsTerm r1 r2
  = ProtoArgs ( here :: CtxWithGoal () | r1 ) r2

type ArgsTerm r
  = Rec.ArgsTerm (ProtoArgsTerm () r)

type ArgsLam r
  = Rec.ArgsLam (ProtoArgsTerm ( dom :: Type, body :: CtxWithGoal () ) r)

type ArgsNeu r
  = Rec.ArgsNeu (ProtoArgsTerm ( termId :: CtxWithoutGoal (), argItems :: CtxWithGoal () ) r)

type ArgsLet r
  = Rec.ArgsLet (ProtoArgsTerm ( term :: CtxWithGoal (), body :: CtxWithGoal () ) r)

type ArgsBuf r
  = Rec.ArgsBuf (ProtoArgsTerm ( term :: CtxWithGoal (), body :: CtxWithGoal () ) r)

type ArgsData r
  = Rec.ArgsData (ProtoArgsTerm ( body :: CtxWithGoal () ) r)

type ArgsMatch r
  = Rec.ArgsMatch (ProtoArgsTerm ( term :: CtxWithGoal (), caseItems :: CtxWithGoal ( typeId :: TypeId ) ) r)

type ArgsHole r
  = Rec.ArgsHole (ProtoArgsTerm () r)

recTerm ::
  forall r a.
  Lacks "syn" r =>
  Lacks "ctx" r =>
  { lam :: ProtoRec ArgsLam r a, neu :: ProtoRec ArgsNeu r a, let_ :: ProtoRec ArgsLet r a, buf :: ProtoRec ArgsBuf r a, data_ :: ProtoRec ArgsData r a, match :: ProtoRec ArgsMatch r a, hole :: ProtoRec ArgsHole r a } ->
  ProtoRec ArgsTerm r a
recTerm rec = -- Rec.recTerm
  --   { lam:
  --       \args@{ syn: { lam }, ctx: { ctx, type_ } } -> case type_ of
  --         ArrowType { dom, cod } -> rec.lam $ modifyHetero _ctx (union { type_dom: dom, body: { ctx: insertVarType lam.termBind.termId dom ctx, type_: cod } }) args
  --         _ -> unsafeCrashWith "badly typed lambda"
  --   , neu:
  --       \args@{ syn: { neu }, ctx: { ctx } } ->
  --         let
  --           termId = ctx
  --           type_ = lookupVarType neu.termId ctx
  --           argItems = { ctx, type_ }
  --         in
  --           rec.neu $ modifyHetero _ctx (union { termId, argItems }) args
  --   , let_:
  --       \args@{ syn: { let_ }, ctx: { ctx, type_ } } ->
  --         let
  --           ctx' = insertVarType let_.termBind.termId let_.type_ ctx
  --         in
  --           rec.let_ $ modifyHetero _ctx (union { term: { ctx: ctx', type_: let_.type_ }, body: { ctx: ctx', type_ } }) args
  --   , buf:
  --       \args@{ syn: { buf }, ctx: { ctx, type_ } } ->
  --         rec.buf $ modifyHetero _ctx (union { term: { ctx, type_: buf.type_ }, body: { ctx, type_ } }) args
  --   , data_:
  --       \args@{ syn: { data_ }, ctx: { ctx, type_ } } ->
  --         rec.data_
  --           $ modifyHetero _ctx
  --               ( union
  --                   { body:
  --                       { ctx:
  --                           flipfoldr data_.sumItems
  --                             ( \sumItem ->
  --                                 insertVarType sumItem.termBind.termId (typeOfSumItem data_.typeBind.typeId sumItem)
  --                                   <<< insertConstrDataType sumItem.termBind.termId { typeId: data_.typeBind.typeId, meta: default }
  --                             )
  --                             $ insertData data_
  --                             $ ctx
  --                       , type_: type_
  --                       }
  --                   }
  --               )
  --               args
  --   , match:
  --       \args@{ syn: { match }, ctx: { ctx, type_ } } ->
  --         -- let
  --         --   data_ = lookupData match.typeId ctx
  --         -- in
  --         rec.match
  --           $ modifyHetero _ctx
  --               ( union
  --                   { term: { ctx, type_: DataType { typeId: match.typeId, meta: default } }
  --                   -- map
  --                   --   ( \(caseItem /\ sumItem) ->
  --                   --       { ctx:
  --                   --           foldl
  --                   --             (flip \(termBindItem /\ paramItem) -> insertVarType termBindItem.termBind.termId paramItem.type_)
  --                   --             ctx
  --                   --             (zip caseItem.termBindItems sumItem.paramItems)
  --                   --       , type_
  --                   --       }
  --                   --   )
  --                   --   (zip match.caseItems (data_.sumItems))
  --                   , caseItems: { typeId: match.typeId, ctx, type_ }
  --                   }
  --               )
  --               args
  --   , hole: undefined -- rec.hole
  --   }
  undefined

-- | recArgItems
type ProtoArgsArgItems r1 r2
  = ProtoArgs ( here :: CtxWithGoal () | r1 ) r2

type ArgsArgItems r
  = Rec.ArgsArgItems (ProtoArgsArgItems () r)

type ArgsArgItem r
  = Rec.ArgsArgItem (ProtoArgsArgItems ( doms :: List Type, cod :: Type, argItem :: CtxWithGoal () ) r)

recArgItems ::
  forall r a.
  Lacks "syn" r =>
  Lacks "ctx" r =>
  { argItem :: ProtoRec ArgsArgItem r a } ->
  ProtoRec ArgsArgItems r (List a)
recArgItems rec = -- Rec.recArgItems { argItem: \args@{ syn, ctx } -> rec.argItem $ modifyHetero _ctx (union { argItem: { ctx: ctx.ctx, type_: index' ctx.doms syn.i } }) args }
  --   <<< (\args@{ syn, ctx } -> modifyHetero _ctx (union (flattenType ctx.type_)) args)
  undefined

-- | recSumItems
type ProtoArgsSumItems r1 r2
  = ProtoArgs ( here :: CtxWithoutGoal () | r1 ) r2

type ArgsSumItems r
  = Rec.ArgsSumItems (ProtoArgsSumItems () r)

type ArgsSumItem r
  = Rec.ArgsSumItem (ProtoArgsSumItems ( sumItem :: CtxWithoutGoal (), termBind :: CtxWithoutGoal (), paramItems :: CtxWithoutGoal () ) r)

recSumItems ::
  forall r a.
  Lacks "syn" r =>
  Lacks "ctx" r =>
  { sumItem :: ProtoRec ArgsSumItem r a } ->
  ProtoRec ArgsSumItems r (List a)
recSumItems rec = -- Rec.recSumItems { sumItem: \args@{ ctx: { ctx } } -> rec.sumItem $ modifyHetero _ctx (union { sumItem: ctx, termBind: ctx, paramItems: ctx }) args }
  undefined

-- | recCaseItem
type ProtoArgsCaseItems r1 r2
  = ProtoArgs ( here :: CtxWithGoal ( typeId :: TypeId ) | r1 ) r2

type ArgsCaseItems r
  = Rec.ArgsCaseItems (ProtoArgsCaseItems () r)

type ArgsCaseItem r
  = Rec.ArgsCaseItem (ProtoArgsCaseItems ( ctxs :: List Context, caseItem :: CtxWithGoal (), termBindItems :: CtxWithoutGoal (), body :: CtxWithGoal () ) r)

recCaseItems ::
  forall r a.
  Lacks "syn" r =>
  Lacks "ctx" r =>
  { caseItem :: ProtoRec ArgsCaseItem r a } ->
  ProtoRec ArgsCaseItems r (List a)
recCaseItems rec = -- Rec.recCaseItems
  --   { caseItem:
  --       \args@{ syn: { i }, ctx: { ctxs, type_ } } ->
  --         let
  --           ctx = index' ctxs i
  --         in
  --           rec.caseItem $ modifyHetero _ctx (union { caseItem: { ctx, type_ }, termBindItems: ctx, body: { ctx, type_ } }) args
  --   }
  --   <<< ( \args@{ syn: { caseItems }, ctx: { ctx, typeId } } ->
  --         modifyHetero _ctx
  --           ( union
  --               { ctxs:
  --                   map
  --                     (\({ termBindItems } /\ { paramItems }) -> flipfoldr (zip termBindItems paramItems) (\({ termBind } /\ { type_ }) -> insertVarType termBind.termId type_) ctx)
  --                     (caseItems `zip` (lookupData typeId ctx).sumItems)
  --               }
  --           )
  --           args
  --     )
  undefined

-- | recParamItems
type ProtoArgsParamItems r1 r2
  = ProtoArgs ( here :: CtxWithoutGoal () | r1 ) r2

type ArgsParamItems r
  = Rec.ArgsParamItems (ProtoArgsParamItems () r)

type ArgsParamItem r
  = Rec.ArgsParamItem (ProtoArgsParamItems ( paramItem :: CtxWithoutGoal () ) r)

recParamItems ::
  forall r a.
  Lacks "syn" r =>
  Lacks "ctx" r =>
  { paramItem :: ProtoRec ArgsParamItem r a } ->
  ProtoRec ArgsParamItems r (List a)
recParamItems rec = -- Rec.recParamItems { paramItem: \args@{ ctx: { ctx } } -> rec.paramItem $ modifyHetero _ctx (union { paramItem: ctx }) args }
  undefined

-- | recTermBindItems
type ProtoArgsTermBindItems r1 r2
  = ProtoArgs ( here :: CtxWithoutGoal () | r1 ) r2

type ArgsTermBindItems r
  = Rec.ArgsTermBindItems (ProtoArgsTermBindItems () r)

type ArgsTermBindItem r
  = Rec.ArgsTermBindItem (ProtoArgsTermBindItems ( termBindItem :: CtxWithoutGoal (), termBind :: CtxWithoutGoal () ) r)

recTermBindItems ::
  forall r a.
  Lacks "syn" r =>
  Lacks "ctx" r =>
  { termBindItem :: ProtoRec ArgsTermBindItem r a } ->
  ProtoRec ArgsTermBindItems r (List a)
recTermBindItems rec = -- Rec.recTermBindItems { termBindItem: \args@{ ctx: { ctx } } -> rec.termBindItem $ modifyHetero _ctx (union { termBindItem: ctx, termBind: ctx }) args }
  undefined

-- | recTermBind
type ArgsTermBind r
  = Rec.ArgsTermBind (ProtoArgs ( here :: CtxWithoutGoal () ) r)

recTermBind ::
  forall r a.
  Lacks "syn" r =>
  Lacks "ctx" r =>
  { termBind :: ProtoRec ArgsTermBind r a } ->
  ProtoRec ArgsTermBind r a
recTermBind rec = Rec.recTermBind { termBind: rec.termBind }

-- | recTypeBind
type ArgsTypeBind r
  = Rec.ArgsTypeBind (ProtoArgs ( here :: CtxWithoutGoal () ) r)

recTypeBind ::
  forall r a.
  Lacks "syn" r =>
  Lacks "ctx" r =>
  { typeBind :: ProtoRec ArgsTypeBind r a } ->
  ProtoRec ArgsTypeBind r a
recTypeBind rec = Rec.recTypeBind { typeBind: rec.typeBind }

-- | recTypeId
type ArgsTypeId r
  = Rec.ArgsTypeId (ProtoArgs ( here :: CtxWithoutGoal () ) r)

recTypeId ::
  forall r a.
  Lacks "syn" r =>
  Lacks "ctx" r =>
  { typeId :: ProtoRec ArgsTypeId r a } ->
  ProtoRec ArgsTypeId r a
recTypeId = Rec.recTypeId

-- | recTermId
type ArgsTermId r
  = Rec.ArgsTermId (ProtoArgs ( here :: CtxWithoutGoal () ) r)

recTermId ::
  forall r a.
  Lacks "syn" r =>
  Lacks "ctx" r =>
  { termId :: ProtoRec ArgsTermId r a } ->
  ProtoRec ArgsTermId r a
recTermId = Rec.recTermId
