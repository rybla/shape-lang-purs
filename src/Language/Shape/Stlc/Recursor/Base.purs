module Language.Shape.Stlc.Recursor.Base where

import Prelude
import Prim (Record, Row)
import Prim as Prim
import Type.Proxy (Proxy(..))

type ProtoArgs r1 r2
  = ( here :: Record r1 | r2 )

type ProtoRec :: (Row Prim.Type -> Row Prim.Type) -> Row Prim.Type -> Prim.Type -> Prim.Type
type ProtoRec args r a
  = Record (args r) -> a

-- | recType
type ProtoArgsType r1 r2
  = ProtoArgs r1 r2

type ArgsType r1 r2
  = ProtoArgsType ( | r1 ) ( | r2 )

type ArgsArrowType r1 r2
  = ProtoArgsType r1 (dom :: Record (ArgsType r1 r2), cod :: Record (ArgsType r1 r2) )

type ArgsDataType r1 r2
  = ProtoArgsType r1 r2 

type ArgsHoleType r1 r2
  = ProtoArgsType r1 r2

