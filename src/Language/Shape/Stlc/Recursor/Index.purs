module Language.Shape.Stlc.Recursor.Index where

import Data.Tuple.Nested
import Language.Shape.Stlc.Context
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Prim.Row
import Record
import Data.List (List)
import Data.Maybe (Maybe)
import Language.Shape.Stlc.Recursor.MetaContext as Rec
import Language.Shape.Stlc.Recursor.Record (modifyHetero)
import Partial.Unsafe (unsafeCrashWith)
import Prim as Prim
import Type.Proxy (Proxy(..))
import Undefined (undefined)

type Cursor
  = Maybe IxUp

type Visit  -- TODO: new name
  = { ix :: IxUp, csr :: Cursor, here :: Boolean }

-- | ProtoRec
type ProtoArgs r1 r2
  = ( argsIx :: Record ( visit :: Visit | r1 ) | r2 )

type ProtoRec args r a
  = Rec.ProtoRec args r a

_argsIx = Proxy :: Proxy "argsIx"

-- | recType
type ProtoArgsType r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsType r
  = Rec.ArgsType (ProtoArgsType () r)

type ArgsArrowType r
  = Rec.ArgsArrowType (ProtoArgsType ( visit_dom :: Visit, visit_cod :: Visit ) r)

type ArgsDataType r
  = Rec.ArgsDataType (ProtoArgsType () r)

type ArgsHoleType r
  = Rec.ArgsHoleType (ProtoArgsType () r)

recType ::
  forall r a.
  Lacks "argsSyn" r =>
  Lacks "argsCtx" r =>
  Lacks "argsMeta" r =>
  Lacks "argsIx" r =>
  { arrow :: ProtoRec ArgsArrowType r a, data_ :: ProtoRec ArgsDataType r a, hole :: ProtoRec ArgsHoleType r a } ->
  ProtoRec ArgsType r a
recType rec =
  Rec.recType
    { arrow:
        \args@{ argsIx: { visit } } ->
          rec.arrow $ modifyHetero _argsIx (union { visit_dom: ?a, visit_cod: ?a }) args
    , data_: undefined
    , hole: undefined
    }

-- -- | recTerm
type ProtoArgsTerm r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsTerm r
  = Rec.ArgsTerm (ProtoArgsTerm () r)

type ArgsLam r
  = Rec.ArgsLam (ProtoArgsTerm ( visit_bind :: Visit, visit_body :: Visit ) r)

type ArgsNeu r
  = Rec.ArgsNeu (ProtoArgsTerm ( visit_args :: List Visit ) r)

type ArgsLet r
  = Rec.ArgsLet (ProtoArgsTerm ( visit_bind :: Visit, visit_type :: Visit, visit_term :: Visit, visit_body :: Visit ) r)

type ArgsBuf r
  = Rec.ArgsBuf (ProtoArgsTerm ( visit_term :: Visit, visit_body :: Visit ) r)

type ArgsData r
  = Rec.ArgsData (ProtoArgsTerm ( visit_bind :: Visit, visit_sum :: Visit, visit_body :: Visit ) r)

type ArgsMatch r
  = Rec.ArgsMatch (ProtoArgsTerm ( visit_term :: Visit, visit_cases :: Visit ) r)

type ArgsHole r
  = Rec.ArgsHole (ProtoArgsTerm () r)

recTerm ::
  forall r a.
  Lacks "argsSyn" r =>
  Lacks "argsCtx" r =>
  Lacks "argsMeta" r =>
  Lacks "argsIx" r =>
  { lam :: ProtoRec ArgsLam r a, neu :: ProtoRec ArgsNeu r a, let_ :: ProtoRec ArgsLet r a, buf :: ProtoRec ArgsBuf r a, data_ :: ProtoRec ArgsData r a, match :: ProtoRec ArgsMatch r a, hole :: ProtoRec ArgsHole r a } ->
  ProtoRec ArgsTerm r a
recTerm rec =
  Rec.recTerm
    { lam: undefined
    , neu: undefined
    , let_: undefined
    , buf: undefined
    , data_: undefined
    , match: undefined
    , hole: undefined
    }
