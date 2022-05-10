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
  , neu :: Record (ArgsNeu r (ArgsTermId r) (ArgsArgItems r)) -> a
  , let_ :: Record (ArgsLet r (ArgsTermBind r) (ArgsType r) (ArgsTerm r)) -> a
  , buf :: Record (ArgsBuf r (ArgsType r) (ArgsTerm r)) -> a
  , data_ :: Record (ArgsData r (ArgsTypeBind r) (ArgsSumItems r) (ArgsTerm r)) -> a
  , match :: Record (ArgsMatch r (ArgsTypeId r) (ArgsTerm r) (ArgsCaseItems r)) -> a
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
          , argItems: { argItems: neu.argItems } `R.union` prune args
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
          , sumItems: { sumItems: data_.sumItems } `R.union` prune args
          , body: { term: data_.body } `R.union` prune args
          }
      $ prune args
  Match match ->
    rec.match
      $ R.union
          { match
          , typeId: { typeId: match.typeId } `R.union` prune args
          , term: { term: match.term } `R.union` prune args
          , caseItems: { caseItems: match.caseItems } `R.union` prune args
          }
      $ prune args
  Hole hole ->
    rec.hole
      $ R.union { hole }
      $ prune args
  where
  prune = R.delete _term

-- | recArgItems
type ArgsArgItems r
  = Rec.ArgsArgItems ( argItems :: List ArgItem | r )

type ArgsArgItem r rTerm
  = Rec.ArgsArgItem ( argItems :: List ArgItem, i :: Int, argItem :: ArgItem | r ) rTerm

recArgsArgItems ::
  forall r a.
  Lacks "argItems" r =>
  { argItem :: Record (ArgsArgItem r (ArgsTerm r)) -> a } ->
  Record (ArgsArgItems r) -> List a
recArgsArgItems rec args =
  List.mapWithIndex
    ( \i argItem ->
        rec.argItem
          $ R.union
              { argItem
              , i
              , term: { term: argItem.term } `R.union` prune args
              }
          $ args
    )
    args.argItems
  where
  prune = R.delete _argItems

-- | recSumItems
type ArgsSumItems r
  = Rec.ArgsSumItems ( sumItems :: List SumItem | r )

type ArgsSumItem r rTermBind rParamItems
  = Rec.ArgsSumItem ( sumItems :: List SumItem, sumItem :: SumItem | r ) rTermBind rParamItems

recArgsSumItems ::
  forall r a.
  Lacks "sumItems" r =>
  { sumItem :: Record (ArgsSumItem r (ArgsTermBind r) (ArgsParamItems r)) -> a } ->
  Record (ArgsSumItems r) -> List a
recArgsSumItems rec args =
  map
    ( \sumItem ->
        rec.sumItem
          $ R.union
              { sumItem
              , termBind: {termBind: sumItem.termBind} `R.union` prune args
              , paramItems: {paramItems: sumItem.paramItems} `R.union` prune args
              }
          $ args
    )
    args.sumItems
  where
  prune = R.delete _sumItems

-- TODO: copy-paste this sort of form for all the *Items recursors

-- | recCaseItems
type ArgsCaseItems r
  = Rec.ArgsCaseItems ( caseItems :: List CaseItem | r )

type ArgsCaseItem r rTermBindItems rTerm
  = Rec.ArgsCaseItem ( caseItems :: List CaseItem, caseItem :: CaseItem | r ) rTermBindItems rTerm

-- | recParamItems
type ArgsParamItems r
  = Rec.ArgsParamItems ( paramItems :: List ParamItem | r )

type ArgsParamItem r rType
  = Rec.ArgsParamItem ( paramItems :: List ParamItem, paramItem :: ParamItem | r ) rType

-- | recTermBindItems
type ArgsTermBindItems r
  = Rec.ArgsTermBindItems ( termBindItems :: List TermBindItem | r )

type ArgsTermBindItem r rTermBind
  = Rec.ArgsTermBindItem ( termBindItems :: List TermBindItem, termBindItem :: TermBindItem | r ) rTermBind

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
