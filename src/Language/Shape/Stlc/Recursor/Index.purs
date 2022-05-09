module Language.Shape.Stlc.Recursor.Index where

import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Recursor.Proxy
import Language.Shape.Stlc.Syntax
import Prelude
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap, wrap)
import Language.Shape.Stlc.Recursor.Context as Rec
import Language.Shape.Stlc.Recursor.Record as RH
import Prim (Record, Row)
import Prim as Prim
import Prim.Row (class Lacks)
import Record as R
import Type.Proxy (Proxy(..))
import Undefined (undefined)

type Visit
  = { ix :: Maybe IxUp, csr :: Maybe IxDown }

visitIxStep :: Visit -> IxStep -> Visit
visitIxStep { ix, csr } ixStep =
  { ix: over wrap (Cons ixStep) <$> ix
  , csr:
      do
        ixSteps <- unwrap <$> csr
        case ixSteps of
          Cons ixStep' ixSteps' -> if ixStep == ixStep' then Just (wrap ixSteps') else Nothing
          Nil -> Nothing
  }

visit :: forall r1 r2. IxStep -> { here :: { visit :: Visit | r1 } | r2 } -> { here :: { visit :: Visit | r1 } | r2 }
visit ixStep args = args { here = args.here { visit = visitIxStep args.here.visit ixStep } }

-- | recType
type ArgsType r
  = Rec.ArgsType ( visit :: Visit | r )

type ArgsArrowType r rType
  = Rec.ArgsArrowType ( visit :: Visit | r ) rType

type ArgsDataType r rTypeId
  = Rec.ArgsDataType ( visit :: Visit | r ) rTypeId

type ArgsHoleType r rHoleId
  = Rec.ArgsHoleType ( visit :: Visit | r ) rHoleId

recType ::
  forall r a.
  Lacks "typeId" r =>
  Lacks "type_" r =>
  { arrowType :: Record (ArgsArrowType r (ArgsType r)) -> a
  , dataType :: Record (ArgsDataType r (ArgsTypeId r)) -> a
  , holeType :: Record (ArgsHoleType r (ArgsHoleId r)) -> a
  } ->
  Record (ArgsType r) -> a
recType rec =
  Rec.recType
    { arrowType:
        \args ->
          rec.arrowType
            { here: args.here
            , dom: visit ixStepArrowType.dom args.dom
            , cod: visit ixStepArrowType.cod args.cod
            }
    , dataType:
        \args ->
          rec.dataType
            { here: args.here
            , typeId: args.typeId
            }
    , holeType:
        \args ->
          rec.holeType
            { here: args.here
            , holeId: args.holeId
            }
    }

-- | recTypeId
type ArgsTypeId r
  = Rec.ArgsTypeId ( visit :: Visit | r )

-- | recHoleId 
type ArgsHoleId r
  = Rec.ArgsHoleId ( visit :: Visit | r )
