module Foo where

import Unsafe.Coerce (unsafeCoerce)
import Type.Data.Map (class Lookup, Empty', Insert')
import Record
import Data.Variant

type T map
  = { t1 :: forall t. Lookup "t1" map t => t
    , t2 :: forall t. Lookup "t2" map t => t
    }

type Map1
  = Insert' "t1" Boolean (Insert' "t2" Int Empty')

impredicate :: forall t1. t1 -> (forall t2. t2)
impredicate t1 = unsafeCoerce t1

mkT1 :: forall t1 t2. Lookup "t1" Map1 t1 => Lookup "t2" Map1 t2 => t1 -> t2 -> T Map1
mkT1 t1 t2 = { t1: impredicate t1, t2: impredicate t2 }

testT1 :: T Map1
testT1 = mkT1 true 1
