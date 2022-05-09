module Language.Shape.Stlc.Recursor.Syntax where

import Language.Shape.Stlc.Recursor.Proxy
import Language.Shape.Stlc.Syntax
import Prelude
import Data.List (List)
import Language.Shape.Stlc.Recursor.Base (atHere)
import Language.Shape.Stlc.Recursor.Base as Rec
import Prim (Record, Row)
import Prim as Prim
import Prim.Row (class Lacks)
import Record as R
import Undefined (undefined)

-- | recType
type ProtoArgsType r
  = ( type_ :: Type | r )

type ArgsType r
  = Rec.ArgsType (ProtoArgsType r)

type ArgsArrowType r rType
  = Rec.ArgsArrowType (ProtoArgsType ( arrowType :: ArrowType | r )) rType

type ArgsDataType r rTypeId
  = Rec.ArgsDataType (ProtoArgsType ( dataType :: DataType | r )) rTypeId

type ArgsHoleType r rHoleId
  = Rec.ArgsHoleType (ProtoArgsType ( holeType :: HoleType | r )) rHoleId

recType ::
  forall r a.
  Lacks "typeId" r =>
  Lacks "type_" r =>
  { arrowType :: Record (ArgsArrowType r (ArgsType r)) -> a
  , dataType :: Record (ArgsDataType r (ArgsTypeId r)) -> a
  , holeType :: Record (ArgsHoleType r (ArgsHoleId r)) -> a
  } ->
  Record (ArgsType r) -> a
recType rec args = case args.here.type_ of
  ArrowType arrowType ->
    rec.arrowType
      { here: R.union { arrowType } $ args.here
      , dom: { here: _ { type_ = arrowType.dom } $ args.here }
      , cod: { here: _ { type_ = arrowType.cod } $ args.here }
      }
  DataType dataType ->
    rec.dataType
      { here: R.union { dataType } $ args.here
      , typeId: { here: R.union { typeId: dataType.typeId } $ R.delete _type_ $ args.here }
      }
  HoleType holeType ->
    rec.holeType
      { here: R.union { holeType } $ args.here
      , holeId: { here: R.union { holeId: holeType.holeId } $ R.delete _type_ $ args.here }
      }

-- | recTerm
type ProtoArgsTerm r
  = ( term :: Term | r )

type ArgsTerm r
  = Rec.ArgsTerm (ProtoArgsTerm r)

type ArgsLam r rTermBind rTerm
  = Rec.ArgsLam (ProtoArgsTerm ( lam :: Lam | r )) rTermBind rTerm

type ArgsNeu r rTermId rArgItems
  = Rec.ArgsNeu (ProtoArgsTerm ( neu :: Neu | r )) rTermId rArgItems

type ArgsLet r termBind rType rTerm
  = Rec.ArgsLet (ProtoArgsTerm ( let_ :: Let | r )) termBind rType rTerm

type ArgsBuf r rType rTerm
  = Rec.ArgsBuf (ProtoArgsTerm ( buf :: Buf | r )) rType rTerm

type ArgsData r rTypeBind rTerm rSumItems
  = Rec.ArgsData (ProtoArgsTerm ( data_ :: Data | r )) rTypeBind rTerm rSumItems

type ArgsMatch r rTypeId rTerm rCaseItems
  = Rec.ArgsMatch (ProtoArgsTerm ( match :: Match | r )) rTypeId rTerm rCaseItems

type ArgsHole r
  = Rec.ArgsHole (ProtoArgsTerm ( hole :: Hole | r ))

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
recTerm rec args = case args.here.term of
  Lam lam ->
    rec.lam
      { here: { lam } `R.union` args.here
      , termBind: (R.union { termBind: lam.termBind } <<< R.delete _term) `atHere` args
      , body: (_ { term = lam.body }) `atHere` args
      }
  Neu neu ->
    rec.neu
      { here: { neu } `R.union` args.here
      , termId: (R.union { termId: neu.termId } <<< R.delete _term) `atHere` args
      , argItems: (R.union { argItems: neu.argItems } <<< R.delete _term) `atHere` args
      }
  Let let_ ->
    rec.let_
      { here: { let_ } `R.union` args.here
      , termBind: (R.union { termBind: let_.termBind } <<< R.delete _term) `atHere` args
      , type_: (R.union { type_: let_.type_ } <<< R.delete _term) `atHere` args
      , term: (R.union { term: let_.term } <<< R.delete _term) `atHere` args
      , body: (R.union { term: let_.body } <<< R.delete _term) `atHere` args
      }
  Buf buf ->
    rec.buf
      { here: { buf } `R.union` args.here
      , type_: (R.union { type_: buf.type_ } <<< R.delete _term) `atHere` args
      , term: (R.union { term: buf.term } <<< R.delete _term) `atHere` args
      , body: (R.union { term: buf.body } <<< R.delete _term) `atHere` args
      }
  Data data_ ->
    rec.data_
      { here: { data_ } `R.union` args.here
      , typeBind: (R.union { typeBind: data_.typeBind } <<< R.delete _term) `atHere` args
      , sumItems: (R.union { sumItems: data_.sumItems } <<< R.delete _term) `atHere` args
      , body: (R.union { term: data_.body } <<< R.delete _term) `atHere` args
      }
  Match match ->
    rec.match
      { here: { match } `R.union` args.here
      , typeId: (R.union { typeId: match.typeId } <<< R.delete _term) `atHere` args
      , term: (R.union { term: match.term } <<< R.delete _term) `atHere` args
      , caseItems: (R.union { caseItems: match.caseItems } <<< R.delete _term) `atHere` args
      }
  Hole hole ->
    rec.hole
      { here: { hole } `R.union` args.here
      }

-- | recArgItems
type ArgsArgItems r
  = Rec.ArgsArgItems ( argItems :: List ArgItem | r )

type ArgsArgItem r rTerm
  = Rec.ArgsArgItem ( argItems :: List ArgItem, argItem :: ArgItem | r ) rTerm

-- | recSumItems
type ArgsSumItems r
  = Rec.ArgsSumItems ( sumItems :: List SumItem | r )

type ArgsSumItem r rTermBind rParamItems
  = Rec.ArgsSumItem ( sumItems :: List SumItem, sumItem :: SumItem | r ) rTermBind rParamItems

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
