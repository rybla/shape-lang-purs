module Language.Shape.Stlc.Recursor.Context where

import Data.Tuple.Nested
import Language.Shape.Stlc.Context
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Prim.Row
import Record
import Data.List (List)
import Language.Shape.Stlc.Recursion.Syntax as Rec
import Language.Shape.Stlc.Recursor.Record (modifyHetero)
import Partial.Unsafe (unsafeCrashWith)
import Prim as Prim
import Type.Proxy (Proxy(..))
import Undefined (undefined)

-- | ProtoRec
type ProtoArgs r1 r2
  = ( argsCtx :: Record r1 | r2 )

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

-- -- | recTerm
type ProtoArgsTerm r1 r2
  = ProtoArgs ( ctx :: Context, type_ :: Type | r1 ) r2

type ArgsTerm r
  = Rec.ArgsTerm (ProtoArgsTerm () r)

type ArgsLam r
  = Rec.ArgsLam (ProtoArgsTerm ( type_dom :: Type, ctx_body :: Context, type_body :: Type ) r)

type ArgsNeu r
  = Rec.ArgsNeu (ProtoArgsTerm ( type_id :: Type, types_args :: List Type ) r)

type ArgsLet r
  = Rec.ArgsLet (ProtoArgsTerm ( type_arg :: Type, ctx_body :: Context ) r)

type ArgsBuf r
  = Rec.ArgsBuf (ProtoArgsTerm () r)

type ArgsData r
  = Rec.ArgsData (ProtoArgsTerm ( ctx_body :: Context ) r)

type ArgsMatch r
  = Rec.ArgsMatch (ProtoArgsTerm () r)

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

            (types_args /\ _) = flattenType type_id
          in
            rec.neu $ modifyHetero _argsCtx (union { type_id, types_args }) args
    , let_:
        \args@{ argsSyn: { let_ }, argsCtx: { ctx } } ->
          rec.let_ $ modifyHetero _argsCtx (union { type_arg: let_.type_, ctx_body: insertVarType let_.termBind.termId let_.type_ ctx }) args
    , buf: rec.buf
    , data_:
        \args@{ argsSyn: { data_ }, argsCtx: { ctx } } ->
          rec.data_ $ modifyHetero _argsCtx (union { ctx_body: insertData data_ ctx }) args
    , match: rec.match
    , hole: rec.hole
    }
