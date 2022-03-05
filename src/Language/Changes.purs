module Language.Changes where

import Prelude
import Prim hiding (Type)

import Data.List (List)
import Data.Map (Map)
import Data.Tuple (Tuple)
import Language.Shape.Stlc.Metadata (defaultArrowTypeMetadata)
import Language.Shape.Stlc.Syntax (Constructor(..), TermID(..), Type(..), TypeID(..))
import Unsafe (error)

data TypeChange
    = ArrowCh TypeChange TypeChange -- only applies to types of form (ArrowType a b _)
    | NoChange
    | InsertArg Type
    | Swap -- only applies to types of form (ArrowType a (ArrowType b c _) _)
    | RemoveArg -- only applies to types of form (ArrowType a b _)
    | Replace Type

data VarChange = VariableTypeChange TypeChange | VariableDeletion

-- DataChange is used where data type is used in type OR a value of it is matched upon
data ConstructorChange = ChangeC TypeChange | InsertConstructor Constructor
data DataChange = DataTypeDeletion | DataTypeChange TypeID (List ConstructorChange)
type Changes = Tuple (Map TermID VarChange) (Map TypeID DataChange)

chType :: Changes -> TypeChange -> Type -> Type
chType chs (ArrowCh c1 c2) (ArrowType a b md) = ArrowType (chType chs c1 a) (chType chs c2 b) md
chType chs (InsertArg a) t = ArrowType a (chType chs NoChange t) defaultArrowTypeMetadata
chType chs Swap t = ?h
chType chs RemoveArg t = ?h
chType chs (Replace a) t = a
chType chs NoChange (ArrowType a b md) = ArrowType (chType chs NoChange a) (chType chs NoChange a) md
chType chs NoChange (HoleType i w md) = ?h -- remove stuff from w
chType chs NoChange (DataType i md) = ?h -- check chs for i
chType chs _ _ = error "shouldn't get here"
-- data Type
--   = ArrowType Type Type ArrowTypeMetadata
--   | DataType TermID DataTypeMetadata
--   | HoleType HoleID TypeWeakening HoleTypeMetadata