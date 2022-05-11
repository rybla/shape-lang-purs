module Language.Shape.Stlc.Recursor.Syntax where

import Language.Shape.Stlc.Recursor.Proxy
import Language.Shape.Stlc.Syntax
import Prelude
import Data.List (List)
import Data.List as List
import Language.Shape.Stlc.Recursor.Base as Rec
import Prim (Int, Record, Row)
import Prim as Prim
import Prim.Row (class Lacks)
import Record as R
import Type.Proxy (Proxy(..))
import Undefined (undefined)

-- | recType
type ArgsType r
  = Rec.ArgsType ( type_ :: Type | r )

type ArgsArrowType r rType
  = Rec.ArgsArrowType ( arrowType :: ArrowType | r ) rType

type ArgsDataType r rTypeId
  = Rec.ArgsDataType ( dataType :: DataType | r ) rTypeId

type ArgsHoleType r rHoleId
  = Rec.ArgsHoleType ( holeType :: HoleType | r ) rHoleId

recType ::
  forall r a.
  Lacks "type_" r =>
  { arrowType :: Record (ArgsArrowType r (ArgsType r)) -> a
  , dataType :: Record (ArgsDataType r (ArgsTypeId r)) -> a
  , holeType :: Record (ArgsHoleType r (ArgsHoleId r)) -> a
  } ->
  Record (ArgsType r) -> a
recType rec args = case args.type_ of
  ArrowType arrowType ->
    rec.arrowType
      $ R.union
          { arrowType
          , dom: R.union { type_: arrowType.dom } $ prune args
          , cod: R.union { type_: arrowType.cod } $ prune args
          }
      $ prune args
  DataType dataType ->
    rec.dataType
      $ R.union
          { dataType
          , typeId: R.union { typeId: dataType.typeId } $ prune args
          }
      $ prune args
  HoleType holeType ->
    rec.holeType
      $ R.union
          { holeType
          , holeId: R.union { holeId: holeType.holeId } $ prune args
          }
      $ prune args
  where
  prune = R.delete _type_

-- | recTerm
type ArgsTerm r
  = Rec.ArgsTerm ( term :: Term | r )

type ArgsLam r rTermBind rTerm
  = Rec.ArgsLam ( lam :: Lam | r ) rTermBind rTerm

type ArgsNeu r rTermId rArgItems
  = Rec.ArgsNeu ( neu :: Neu | r ) rTermId rArgItems

type ArgsLet r termBind rType rTerm
  = Rec.ArgsLet ( let_ :: Let | r ) termBind rType rTerm

type ArgsBuf r rType rTerm
  = Rec.ArgsBuf ( buf :: Buf | r ) rType rTerm

type ArgsData r rTypeBind rTerm rSumItems
  = Rec.ArgsData ( data_ :: Data | r ) rTypeBind rTerm rSumItems

type ArgsMatch r rTypeId rTerm rCaseItems
  = Rec.ArgsMatch ( match :: Match | r ) rTypeId rTerm rCaseItems

type ArgsHole r
  = Rec.ArgsHole ( hole :: Hole | r )

recTerm ::
  forall r a.
  Lacks "term" r =>
  { lam :: Record (ArgsLam r (ArgsTermBind r) (ArgsTerm r)) -> a
  , neu :: Record (ArgsNeu r (ArgsTermId r) (ArgsArgItem r)) -> a
  , let_ :: Record (ArgsLet r (ArgsTermBind r) (ArgsType r) (ArgsTerm r)) -> a
  , buf :: Record (ArgsBuf r (ArgsType r) (ArgsTerm r)) -> a
  , data_ :: Record (ArgsData r (ArgsTypeBind r) (ArgsSumItem r) (ArgsTerm r)) -> a
  , match :: Record (ArgsMatch r (ArgsTypeId r) (ArgsTerm r) (ArgsCaseItem r)) -> a
  , hole :: Record (ArgsHole r) -> a
  } ->
  Record (ArgsTerm r) -> a
recTerm rec args = case args.term of
  Lam lam ->
    rec.lam
      $ R.union
          { lam
          , termBind: { termBind: lam.termBind } `R.union` prune args
          , body: { term: lam.body } `R.union` prune args
          }
      $ prune args
  Neu neu ->
    rec.neu
      $ R.union
          { neu
          , termId: { termId: neu.termId } `R.union` prune args
          , argItems: (\argItem -> { argItem: argItem } `R.union` prune args) <$> neu.argItems
          }
      $ prune args
  Let let_ ->
    rec.let_
      $ R.union
          { let_
          , termBind: { termBind: let_.termBind } `R.union` prune args
          , sign: { type_: let_.sign } `R.union` prune args
          , impl: { term: let_.impl } `R.union` prune args
          , body: { term: let_.body } `R.union` prune args
          }
      $ prune args
  Buf buf ->
    rec.buf
      $ R.union
          { buf
          , sign: { type_: buf.sign } `R.union` prune args
          , impl: { term: buf.impl } `R.union` prune args
          , body: { term: buf.body } `R.union` prune args
          }
      $ prune args
  Data data_ ->
    rec.data_
      $ R.union
          { data_
          , typeBind: { typeBind: data_.typeBind } `R.union` prune args
          , sumItems: (\sumItem -> { sumItem } `R.union` prune args) <$> data_.sumItems
          , body: { term: data_.body } `R.union` prune args
          }
      $ prune args
  Match match ->
    rec.match
      $ R.union
          { match
          , typeId: { typeId: match.typeId } `R.union` prune args
          , term: { term: match.term } `R.union` prune args
          , caseItems: map (\caseItem -> { caseItem } `R.union` prune args) match.caseItems
          }
      $ prune args
  Hole hole ->
    rec.hole
      $ R.union { hole }
      $ prune args
  where
  prune = R.delete _term

-- | recArgItems
type ArgsArgItem r
  = Rec.ArgsArgItem ( argItem :: ArgItem | r )

type ArgsArgItem_ArgItem r rTerm
  = Rec.ArgsArgItem_ArgItem ( argItem :: ArgItem | r ) rTerm

recArgItem ::
  forall r a.
  Lacks "argItem" r =>
  { argItem :: Record (ArgsArgItem_ArgItem r (ArgsTerm r)) -> a } ->
  Record (ArgsArgItem r) -> a
recArgItem rec args =
  rec.argItem
    $ R.union
        { term: { term: args.argItem.term } `R.union` prune args }
        args
  where
  prune = R.delete _argItem

-- | recSumItems
type ArgsSumItem r
  = Rec.ArgsSumItem ( sumItem :: SumItem | r )

type ArgsSumItem_SumItem r rTermBind rParamItems
  = Rec.ArgsSumItem_SumItem ( sumItem :: SumItem | r ) rTermBind rParamItems

recSumItem ::
  forall r a.
  Lacks "sumItem" r =>
  { sumItem :: Record (ArgsSumItem_SumItem r (ArgsTermBind r) (ArgsParamItem r)) -> a } ->
  Record (ArgsSumItem r) -> a
recSumItem rec args =
  rec.sumItem
    $ R.union
        { termBind: { termBind: args.sumItem.termBind } `R.union` prune args
        , paramItems: (\paramItem -> { paramItem } `R.union` prune args) <$> args.sumItem.paramItems
        }
        args
  where
  prune = R.delete _sumItem

-- | recCaseItem
type ArgsCaseItem r
  = Rec.ArgsCaseItem ( caseItem :: CaseItem | r )

type ArgsCaseItem_CaseItem r rTermBindItems rTerm
  = Rec.ArgsCaseItem_CaseItem ( caseItem :: CaseItem | r ) rTermBindItems rTerm

recCaseItem ::
  forall r a.
  Lacks "caseItem" r =>
  { caseItem :: Record (ArgsCaseItem_CaseItem r (ArgsTermBindItem r) (ArgsTerm r)) -> a } ->
  Record (ArgsCaseItem r) -> a
recCaseItem rec args =
  rec.caseItem
    $ R.union
        { termBindItems: (\termBindItem -> { termBindItem } `R.union` prune args) <$> args.caseItem.termBindItems
        , body: { term: args.caseItem.body } `R.union` prune args
        }
        args
  where
  prune = R.delete _caseItem

-- | recParamItems
type ArgsParamItem r
  = Rec.ArgsParamItem ( paramItem :: ParamItem | r )

type ArgsParamItem_ParamItem r rType
  = Rec.ArgsParamItem_ParamItem ( paramItem :: ParamItem | r ) rType

recParamItem ::
  forall r a.
  Lacks "paramItem" r =>
  { paramItem :: Record (ArgsParamItem_ParamItem r (ArgsType r)) -> a } ->
  Record (ArgsParamItem r) -> a
recParamItem rec args =
  rec.paramItem
    $ R.union
        { type_: { type_: args.paramItem.type_ } `R.union` prune args
        }
        args
  where
  prune = R.delete _paramItem

-- | recTermBindItems
type ArgsTermBindItem r
  = Rec.ArgsTermBindItem ( termBindItem :: TermBindItem | r )

type ArgsTermBindItem_TermBindItem r rTermBind
  = Rec.ArgsTermBindItem_TermBindItem ( termBindItem :: TermBindItem | r ) rTermBind

recTermBindItem ::
  forall r a.
  Lacks "termBindItem" r =>
  { termBindItem :: Record (ArgsTermBindItem_TermBindItem r (ArgsTermBind r)) -> a } ->
  Record (ArgsTermBindItem r) -> a
recTermBindItem rec args =
  rec.termBindItem
    $ R.union
        { termBind: { termBind: args.termBindItem.termBind } `R.union` prune args
        }
        args
  where
  prune = R.delete _termBindItem

-- | recTypeBind
type ArgsTypeBind r
  = Rec.ArgsTypeBind ( typeBind :: TypeBind | r )

type ArgsTypeBind_TypeBind r rTypeId
  = Rec.ArgsTypeBind_TypeBind ( typeBind :: TypeBind | r ) rTypeId

-- | recTermBind
type ArgsTermBind r
  = Rec.ArgsTermBind ( termBind :: TermBind | r )

type ArgsTermBind_TermBind r rTermId
  = Rec.ArgsTermBind_TermBind ( termBind :: TermBind | r ) rTermId

-- | recTypeId
type ArgsTypeId r
  = Rec.ArgsTypeId ( typeId :: TypeId | r )

-- | recTermId
type ArgsTermId r
  = Rec.ArgsTermId ( termId :: TermId | r )

-- | recHoleId 
type ArgsHoleId r
  = Rec.ArgsHoleId ( holeId :: HoleId | r )
