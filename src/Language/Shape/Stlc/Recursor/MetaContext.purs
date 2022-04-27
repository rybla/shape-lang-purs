module Language.Shape.Stlc.Recursor.MetaContext where

import Data.Tuple
import Data.Tuple.Nested
import Language.Shape.Stlc.Context
import Language.Shape.Stlc.MetaContext
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Prim.Row
import Record
import Control.Monad.State (State)
import Control.Monad.State as State
import Data.List (List)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Language.Shape.Stlc.Metadata (Name(..))
import Language.Shape.Stlc.Recursor.Context as Rec
import Language.Shape.Stlc.Recursor.Record (modifyHetero)
import Partial.Unsafe (unsafeCrashWith)
import Prim as Prim
import Record as Record
import Type.Proxy (Proxy(..))
import Undefined (undefined)

-- | ProtoRec
type ProtoArgs r1 r2
  = ( argsMeta :: Record ( meta :: MetaContext | r1 ) | r2 )

-- | the State (Set HoleId) gathers the HoleIds of HoleTypes in the program, 
-- | statefully.
type ProtoRec args r a
  = Rec.ProtoRec args r (State (Set HoleId) a)

_argsMeta = Proxy :: Proxy "argsMeta"

_meta = Proxy :: Proxy "meta"

-- | recType
type ProtoArgsType r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsType r
  = Rec.ArgsType (ProtoArgsType () r)

type ArgsArrowType r
  = Rec.ArgsArrowType (ProtoArgsType ( meta_cod :: MetaContext ) r)

type ArgsDataType r
  = Rec.ArgsDataType (ProtoArgsType () r)

type ArgsHoleType r
  = Rec.ArgsHoleType (ProtoArgsType () r)

recType ::
  forall r a.
  Lacks "argsSyn" r =>
  Lacks "argsCtx" r =>
  Lacks "argsMeta" r =>
  { arrow :: ProtoRec ArgsArrowType r a, data_ :: ProtoRec ArgsDataType r a, hole :: ProtoRec ArgsHoleType r a } ->
  ProtoRec ArgsType r a
recType rec =
  Rec.recType
    { arrow: \args@{ argsMeta: { meta } } -> rec.arrow $ modifyHetero _argsMeta (union { meta_cod: incrementIndentation meta }) args
    , data_: \args -> rec.data_ args
    , hole:
        \args@{ argsSyn: { hole } } -> do
          State.modify_ $ Set.insert hole.holeId
          rec.hole args
    }

-- -- | recTerm
type ProtoArgsTerm r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsTerm r
  = Rec.ArgsTerm (ProtoArgsTerm () r)

type ArgsLam r
  = Rec.ArgsLam (ProtoArgsTerm ( meta_body :: MetaContext ) r)

type ArgsNeu r
  = Rec.ArgsNeu (ProtoArgsTerm ( meta_args :: List MetaContext ) r)

type ArgsLet r
  = Rec.ArgsLet (ProtoArgsTerm ( meta_type :: MetaContext, meta_term :: MetaContext, meta_body :: MetaContext ) r)

type ArgsBuf r
  = Rec.ArgsBuf (ProtoArgsTerm ( meta_term :: MetaContext, meta_body :: MetaContext ) r)

type ArgsData r
  = Rec.ArgsData (ProtoArgsTerm ( meta_sum :: MetaContext, meta_body :: MetaContext ) r)

type ArgsMatch r
  = Rec.ArgsMatch (ProtoArgsTerm ( meta_term :: MetaContext, meta_cases :: MetaContext ) r)

type ArgsHole r
  = Rec.ArgsHole (ProtoArgsTerm () r)

recTerm ::
  forall r a.
  Lacks "argsSyn" r =>
  Lacks "argsCtx" r =>
  Lacks "argsMeta" r =>
  { lam :: ProtoRec ArgsLam r a, neu :: ProtoRec ArgsNeu r a, let_ :: ProtoRec ArgsLet r a, buf :: ProtoRec ArgsBuf r a, data_ :: ProtoRec ArgsData r a, match :: ProtoRec ArgsMatch r a, hole :: ProtoRec ArgsHole r a } ->
  ProtoRec ArgsTerm r a
recTerm rec =
  Rec.recTerm
    { lam:
        \args@{ argsSyn: { lam }, argsMeta: { meta } } ->
          rec.lam
            $ modifyHetero _argsMeta
                (union { meta_body: insertVarName lam.id (unwrap lam.meta).name meta })
                args
    , neu:
        \args@{ argsSyn: { neu }, argsMeta: { meta } } ->
          rec.neu
            $ modifyHetero _argsMeta
                (union { meta_args: map (\_ -> incrementIndentation meta) neu.args })
                args
    , let_:
        \args@{ argsSyn: { let_ }, argsMeta: { meta } } ->
          rec.let_
            $ modifyHetero _argsMeta
                ( let
                    meta' = incrementIndentation meta
                  in
                    union
                      { meta_type: meta'
                      , meta_term: meta'
                      , meta_body: insertVarName let_.id (unwrap let_.meta).name meta'
                      }
                )
                args
    , buf:
        \args@{ argsMeta: { meta } } ->
          rec.buf
            $ modifyHetero _argsMeta
                ( let
                    meta' = incrementIndentation meta
                  in
                    union
                      { meta_term: meta'
                      , meta_body: meta'
                      }
                )
                args
    , data_:
        \args@{ argsSyn: { data_ }, argsMeta: { meta } } ->
          rec.data_
            $ modifyHetero _argsMeta
                ( let
                    meta' = incrementIndentation meta
                  in
                    union
                      { meta_sum: meta'
                      , meta_body: insertDataName data_.id (unwrap data_.meta).name meta'
                      }
                )
                args
    , match:
        \args@{ argsMeta: { meta } } ->
          rec.match
            $ modifyHetero _argsMeta
                ( let
                    meta' = incrementIndentation meta
                  in
                    union
                      { meta_term: meta'
                      , meta_cases: meta'
                      }
                )
                args
    , hole: \args -> rec.hole args
    }
