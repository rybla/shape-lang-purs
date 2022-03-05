module Type.Data.Map where

import Prelude

data Map' :: Type
data Map'

foreign import data Empty' :: Map'

foreign import data Insert' :: Symbol -> Type -> Map' -> Map'

class Lookup :: Symbol -> Map' -> Type -> Constraint
class Lookup key map value | key map -> value
instance Lookup key (Insert' key value map') value
else instance Lookup key map' value => Lookup key (Insert' key' value' map') value

