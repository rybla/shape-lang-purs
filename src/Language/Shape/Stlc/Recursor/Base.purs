module Language.Shape.Stlc.Recursor.Base where

import Prelude
import Prim (Record, Row)
import Prim as Prim
import Type.Proxy (Proxy(..))

atHere :: forall a b r. (a -> b) -> { here :: a | r } -> { here :: b }
atHere f args = { here: f args.here }

mapHere :: forall a b r. (a -> b) -> { here :: a | r } -> { here :: b | r }
mapHere f args = args { here = f args.here }

type ProtoArgs r rChildren
  = ( here :: Record r | rChildren )

type ProtoRec :: (Row Prim.Type -> Row Prim.Type) -> Row Prim.Type -> Prim.Type -> Prim.Type
type ProtoRec args r a
  = Record (args r) -> a

-- | recType
type ArgsType r
  = ProtoArgs r ()

type ArgsArrowType r rType
  = ProtoArgs r ( dom :: Record rType, cod :: Record rType )

type ArgsDataType r rTypeId
  = ProtoArgs r ( typeId :: Record rTypeId )

type ArgsHoleType r rHoleId
  = ProtoArgs r ( holeId :: Record rHoleId )

-- | recTerm
type ArgsTerm r
  = ProtoArgs r ()

type ArgsLam r rTermBind rTerm
  = ProtoArgs r ( termBind :: Record rTermBind, body :: Record rTerm )

type ArgsNeu r rTermId rArgItems
  = ProtoArgs r ( termId :: Record rTermId, argItems :: Record rArgItems )

type ArgsLet r rTermBind rType rTerm
  = ProtoArgs r ( termBind :: Record rTermBind, type_ :: Record rType, term :: Record rTerm, body :: Record rTerm )

type ArgsBuf r rType rTerm
  = ProtoArgs r ( type_ :: Record rType, term :: Record rTerm, body :: Record rTerm )

type ArgsData r rTypeBind rSumItems rTerm 
  = ProtoArgs r ( typeBind :: Record rTypeBind, sumItems :: Record rSumItems, body :: Record rTerm )

type ArgsMatch r rTypeId rTerm rCaseItems
  = ProtoArgs r ( typeId :: Record rTypeId, term :: Record rTerm, caseItems :: Record rCaseItems )

type ArgsHole r
  = ProtoArgs r ()

-- | recArgItems
type ArgsArgItems r
  = ProtoArgs r ()

type ArgsArgItem r rTerm
  = ProtoArgs r ( term :: Record rTerm )

-- | recSumItems
type ArgsSumItems r
  = ProtoArgs r ()

type ArgsSumItem r rTermBind rParamItems
  = ProtoArgs r ( termBind :: Record rTermBind, paramItems :: Record rParamItems )

-- | recCaseItems
type ArgsCaseItems r
  = ProtoArgs r ()

type ArgsCaseItem r rTermBindItems rTerm
  = ProtoArgs r ( termBindItems :: Record rTermBindItems, body :: Record rTerm )

-- | recParamItems
type ArgsParamItems r
  = ProtoArgs r ()

type ArgsParamItem r rType
  = ProtoArgs r ( type_ :: Record rType )

-- | recTermBindItems
type ArgsTermBindItems r
  = ProtoArgs r ()

type ArgsTermBindItem r rTermBind
  = ProtoArgs r ( termBind :: Record rTermBind )

-- | recTypeBind
type ArgsTypeBind r
  = ProtoArgs r ()

type ArgsTypeBind_TypeBind r rTypeId
  = ProtoArgs r ( typeId :: Record rTypeId )

-- | recTermBind
type ArgsTermBind r
  = ProtoArgs r ()

type ArgsTermBind_TermBind r rTermId
  = ProtoArgs r ( termId :: Record rTermId )

-- | recTypeId
type ArgsTypeId r
  = ProtoArgs r ()

-- | recTermId
type ArgsTermId r
  = ProtoArgs r ()

-- | recHoleId
type ArgsHoleId r
  = ProtoArgs r ()
