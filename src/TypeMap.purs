module TypeMap where

import Prelude
import Record
import Data.Symbol (SProxy(..))
import Prim.Row (class Cons)
import Type.Data.Row (RProxy(..))
import Type.Proxy (Proxy(..))

type GetTM label a tail row
  = Cons label a tail row => a

type T meta
  = { t1 :: forall a tail. GetTM "t1" a tail meta
    , t2 :: forall a tail. GetTM "t2" a tail meta
    }

mkGetTM :: forall label a tail row. SProxy label -> RProxy row -> Proxy a -> a -> GetTM label a tail row
mkGetTM plabel prow pa a = a

type ARow
  = ( t1 :: Int, t2 :: Boolean )

aT :: T ARow
aT = { t1: mkGetTM (SProxy :: SProxy "t1") (RProxy :: RProxy ARow) (Proxy :: Proxy Int) ?b, t2: ?b }
