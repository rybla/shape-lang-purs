module Language.Shape.Stlc.Recursor.Metacontext where

import Language.Shape.Stlc.Metacontext
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Prim.Row
import Record
import Control.Monad.State (StateT)
import Control.Monad.State as State
import Data.List.Unsafe (List)
import Data.List.Unsafe as List
import Data.Newtype (unwrap)
import Data.Traversable (sequence)
import Language.Shape.Stlc.Recursor.Index as Rec
import Language.Shape.Stlc.Recursor.Record (modifyHetero)
import Prim as Prim
import Type.Proxy (Proxy(..))
import Undefined (undefined)

-- | ProtoRec
type ProtoArgs r1 r2
  = ( meta :: Record ( meta :: Metacontext | r1 ) | r2 )

-- | The `State (List HoleId)` gathers the HoleIds of `HoleTypes` in the 
-- | program, statefully.
type ProtoRec args r m a
  = Rec.ProtoRec args r (StateT (List HoleId) m a)

_meta = Proxy :: Proxy "meta"

-- | recType
type ProtoArgsType r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsType r
  = Rec.ArgsType (ProtoArgsType () r)

type ArgsArrowType r
  = Rec.ArgsArrowType (ProtoArgsType ( dom :: Metacontext, cod :: Metacontext ) r)

type ArgsDataType r
  = Rec.ArgsDataType (ProtoArgsType () r)

type ArgsHoleType r
  = Rec.ArgsHoleType (ProtoArgsType () r)

recType ::
  forall r m a.
  Monad m =>
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  { arrow :: ProtoRec ArgsArrowType r m a, data_ :: ProtoRec ArgsDataType r m a, hole :: ProtoRec ArgsHoleType r m a } ->
  ProtoRec ArgsType r m a
recType rec =
  Rec.recType
    { arrow:
        \args@{ meta: { meta } } ->
          rec.arrow
            $ modifyHetero _meta (union { dom: meta, cod: incrementIndentation meta }) args
    , data_: rec.data_
    , hole:
        \args@{ syn: { hole } } -> do
          State.modify_ $ List.Cons hole.holeId
          rec.hole args
    }

-- -- | recTerm
type ProtoArgsTerm r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsTerm r
  = Rec.ArgsTerm (ProtoArgsTerm () r)

type ArgsLam r
  = Rec.ArgsLam (ProtoArgsTerm ( body :: Metacontext ) r)

type ArgsNeu r
  = Rec.ArgsNeu (ProtoArgsTerm ( argItems :: Metacontext ) r)

type ArgsLet r
  = Rec.ArgsLet (ProtoArgsTerm ( type_ :: Metacontext, term :: Metacontext, body :: Metacontext ) r)

type ArgsBuf r
  = Rec.ArgsBuf (ProtoArgsTerm ( term :: Metacontext, body :: Metacontext ) r)

type ArgsData r
  = Rec.ArgsData (ProtoArgsTerm ( sumItems :: Metacontext, body :: Metacontext ) r)

type ArgsMatch r
  = Rec.ArgsMatch (ProtoArgsTerm ( term :: Metacontext, caseItems :: Metacontext ) r)

type ArgsHole r
  = Rec.ArgsHole (ProtoArgsTerm () r)

recTerm ::
  forall r m a.
  Monad m =>
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  { lam :: ProtoRec ArgsLam r m a, neu :: ProtoRec ArgsNeu r m a, let_ :: ProtoRec ArgsLet r m a, buf :: ProtoRec ArgsBuf r m a, data_ :: ProtoRec ArgsData r m a, match :: ProtoRec ArgsMatch r m a, hole :: ProtoRec ArgsHole r m a } ->
  ProtoRec ArgsTerm r m a
recTerm rec =
  Rec.recTerm
    { lam:
        \args@{ syn: { lam }, meta: { meta } } ->
          rec.lam
            $ modifyHetero _meta
                (union { body: insertVarName lam.termBind.termId (unwrap lam.meta).name meta })
                args
    , neu:
        \args@{ syn: { neu }, meta: { meta } } ->
          rec.neu
            $ modifyHetero _meta
                (union { argItems: meta })
                args
    , let_:
        \args@{ syn: { let_ }, meta: { meta } } ->
          rec.let_
            $ modifyHetero _meta
                ( let
                    meta' = incrementIndentation meta
                  in
                    union
                      { type_: meta'
                      , term: meta'
                      , body: insertVarName let_.termBind.termId (unwrap let_.meta).name meta'
                      }
                )
                args
    , buf:
        \args@{ meta: { meta } } ->
          rec.buf
            $ modifyHetero _meta
                ( let
                    meta' = incrementIndentation meta
                  in
                    union
                      { term: meta'
                      , body: meta'
                      }
                )
                args
    , data_:
        \args@{ syn: { data_ }, meta: { meta } } ->
          rec.data_
            $ modifyHetero _meta
                ( let
                    meta' = incrementIndentation meta
                  in
                    union
                      { sumItems: meta'
                      , body: insertDataName data_.typeBind.typeId (unwrap data_.meta).name meta'
                      }
                )
                args
    , match:
        \args@{ meta: { meta } } ->
          rec.match
            $ modifyHetero _meta
                ( let
                    meta' = incrementIndentation meta
                  in
                    union
                      { term: meta'
                      , caseItems: meta'
                      }
                )
                args
    , hole: \args -> rec.hole args
    }

-- -- | recArgItems
-- type ProtoArgsArgItems r1 r2
--   = ProtoArgs r1 r2
-- type ArgsArgItems r
--   = Rec.ArgsArgItems (ProtoArgsArgItems () r)
-- type ArgsArgItemsCons r
--   = Rec.ArgsArgItemsCons (ProtoArgsArgItems () r)
-- type ArgsArgItemsNil r
--   = Rec.ArgsArgItemsNil (ProtoArgsArgItems () r)
-- recArgItems ::
--   forall r m a. Monad m =>
--   Lacks "syn" r =>
--   Lacks "ctx" r =>
--   Lacks "ix" r =>
--   Lacks "meta" r =>
--   { cons :: ProtoRec ArgsArgItemsCons r m a, nil :: ProtoRec ArgsArgItemsNil r m a } ->
--   ProtoRec ArgsArgItems r m a
-- recArgItems rec =
--   Rec.recArgItems
--     { cons: rec.cons
--     , nil: rec.nil
--     }
type ProtoArgsArgItems r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsArgItems r
  = Rec.ArgsArgItems (ProtoArgsArgItems () r)

type ArgsArgItem r
  = Rec.ArgsArgItem (ProtoArgsArgItems () r)

recArgItems ::
  forall r m a.
  Monad m =>
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  { argItem :: ProtoRec ArgsArgItem r m a } ->
  ProtoRec ArgsArgItems r m (List a)
recArgItems rec = sequence <<< Rec.recArgItems rec

-- | recCaseItem
type ProtoArgsCaseItems r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsCaseItems r
  = Rec.ArgsCaseItems (ProtoArgsCaseItems () r)

type ArgsCaseItem r
  = Rec.ArgsCaseItem (ProtoArgsCaseItems ( body :: Metacontext ) r)

recCaseItems ::
  forall r m a.
  Monad m =>
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  { caseItem :: ProtoRec ArgsCaseItem r m a } ->
  ProtoRec ArgsCaseItems r m (List a)
recCaseItems rec = sequence <<< Rec.recCaseItems { caseItem: \args@{ meta } -> rec.caseItem $ modifyHetero _meta (union { body: incrementIndentation meta.meta }) args }

-- | recSumItems
type ProtoArgsSumItems r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsSumItems r
  = Rec.ArgsSumItems (ProtoArgsSumItems () r)

type ArgsSumItem r
  = Rec.ArgsSumItem (ProtoArgsSumItems ( sumItem :: Metacontext, paramItems :: Metacontext ) r)

recSumItems ::
  forall r m a.
  Monad m =>
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  { sumItem :: ProtoRec ArgsSumItem r m a } ->
  ProtoRec ArgsSumItems r m (List a)
recSumItems rec = sequence <<< Rec.recSumItems { sumItem: \args@{ meta } -> let meta' = incrementIndentation meta.meta in rec.sumItem $ modifyHetero _meta (union { sumItem: meta', paramItems: meta' }) args }

-- | recParamItems
type ProtoArgsParamItems r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsParamItems r
  = Rec.ArgsParamItems (ProtoArgsParamItems () r)

type ArgsParamItem r
  = Rec.ArgsParamItem (ProtoArgsParamItems ( paramItem :: Metacontext, type_ :: Metacontext ) r)

recParamItems ::
  forall r m a.
  Monad m =>
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  { paramItem :: ProtoRec ArgsParamItem r m a } ->
  ProtoRec ArgsParamItems r m (List a)
recParamItems rec = sequence <<< Rec.recParamItems { paramItem: \args@{ meta: { meta } } -> rec.paramItem $ modifyHetero _meta (union { paramItem: meta, type_: meta }) args }

-- | recTermBindItems
type ProtoArgsTermBindItems r1 r2
  = ProtoArgs ( | r1 ) r2

type ArgsTermBindItems r
  = Rec.ArgsTermBindItems (ProtoArgsTermBindItems () r)

type ArgsTermBindItem r
  = Rec.ArgsTermBindItem (ProtoArgsTermBindItems () r)

recTermBindItems ::
  forall r m a.
  Monad m =>
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  { termBindItem :: ProtoRec ArgsTermBindItem r m a } ->
  ProtoRec ArgsTermBindItems r m (List a)
recTermBindItems rec = sequence <<< Rec.recTermBindItems rec

-- | recTermBind
type ArgsTermBind r
  = Rec.ArgsTermBind (ProtoArgs () r)

recTermBind ::
  forall r m a.
  Monad m =>
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  { termBind :: ProtoRec ArgsTermBind r m a } ->
  ProtoRec ArgsTermBind r m a
recTermBind rec = Rec.recTermBind { termBind: rec.termBind }

-- | recTypeBind
type ArgsTypeBind r
  = Rec.ArgsTypeBind (ProtoArgs () r)

recTypeBind ::
  forall r m a.
  Monad m =>
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  { typeBind :: ProtoRec ArgsTypeBind r m a } ->
  ProtoRec ArgsTypeBind r m a
recTypeBind rec = Rec.recTypeBind { typeBind: rec.typeBind }

-- | recTypeId
type ArgsTypeId r
  = Rec.ArgsTypeId (ProtoArgs () r)

recTypeId ::
  forall r m a.
  Monad m =>
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  { typeId :: ProtoRec ArgsTypeId r m a } ->
  ProtoRec ArgsTypeId r m a
recTypeId = Rec.recTypeId

-- | recTermId
type ArgsTermId r
  = Rec.ArgsTermId (ProtoArgs () r)

recTermId ::
  forall r m a.
  Monad m =>
  Lacks "syn" r =>
  Lacks "ctx" r =>
  Lacks "ix" r =>
  Lacks "meta" r =>
  { termId :: ProtoRec ArgsTermId r m a } ->
  ProtoRec ArgsTermId r m a
recTermId = Rec.recTermId
