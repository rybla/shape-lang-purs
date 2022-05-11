module Language.Shape.Stlc.Recursor.Base where

import Prelude
import Data.List (List)
import Prim (Record, Row)
import Prim as Prim
import Type.Proxy (Proxy(..))

atHere :: forall a b r. (a -> b) -> { here :: a | r } -> { here :: b }
atHere f args = { here: f args.here }

mapHere :: forall a b r. (a -> b) -> { here :: a | r } -> { here :: b | r }
mapHere f args = args { here = f args.here }

type ProtoRec :: (Row Prim.Type -> Row Prim.Type) -> Row Prim.Type -> Prim.Type -> Prim.Type
type ProtoRec args r a
  = Record (args r) -> a

-- | recType
type ArgsType r
  = ( | r )

type ArgsArrowType r rType
  = ( dom :: Record rType, cod :: Record rType | r )

type ArgsDataType r rTypeId
  = ( typeId :: Record rTypeId | r )

type ArgsHoleType r rHoleId
  = ( holeId :: Record rHoleId | r )

-- | recTerm
type ArgsTerm r
  = ( | r )

type ArgsLam r rTermBind rTerm
  = ( termBind :: Record rTermBind, body :: Record rTerm | r )

type ArgsNeu r rTermId rArgItems
  = ( termId :: Record rTermId, argItems :: Record rArgItems | r )

type ArgsLet r rTermBind rType rTerm
  = ( termBind :: Record rTermBind, sign :: Record rType, impl :: Record rTerm, body :: Record rTerm | r )

type ArgsBuf r rType rTerm
  = ( sign :: Record rType, impl :: Record rTerm, body :: Record rTerm | r )

type ArgsData r rTypeBind rSumItems rTerm
  = ( typeBind :: Record rTypeBind, sumItems :: Record rSumItems, body :: Record rTerm | r )

type ArgsMatch r rTypeId rTerm rCaseItem
  = ( typeId :: Record rTypeId, term :: Record rTerm, caseItems :: List (Record rCaseItem) | r )

type ArgsHole r
  = ( | r )

-- | recArgItem
type ArgsArgItem r
  = ( | r )

type ArgsArgItem_ArgItem r rTerm
  = ( term :: Record rTerm | r )

-- | recSumItem
type ArgsSumItem r
  = ( | r )

type ArgsSumItem_SumItem r rTermBind rParamItems
  = ( termBind :: Record rTermBind, paramItems :: Record rParamItems | r )

-- | recCaseItem
type ArgsCaseItem r
  = ( | r )

type ArgsCaseItem_CaseItem r rTermBindItems rTerm
  = ( termBindItems :: Record rTermBindItems, body :: Record rTerm | r )

-- | recParamItem
type ArgsParamItem r
  = ( | r )

type ArgsParamItem_ParamItem r rType
  = ( type_ :: Record rType | r )

-- | recTermBindItems
type ArgsTermBindItem r
  = ( | r )

type ArgsTermBindItem_TermBindItem r rTermBind
  = ( termBind :: Record rTermBind | r )

-- | recTypeBind
type ArgsTypeBind r
  = ( | r )

type ArgsTypeBind_TypeBind r rTypeId
  = ( typeId :: Record rTypeId | r )

-- | recTermBind
type ArgsTermBind r
  = ( | r )

type ArgsTermBind_TermBind r rTermId
  = ( termId :: Record rTermId | r )

-- | recTypeId
type ArgsTypeId r
  = ( | r )

-- | recTermId
type ArgsTermId r
  = ( | r )

-- | recHoleId
type ArgsHoleId r
  = ( | r )
