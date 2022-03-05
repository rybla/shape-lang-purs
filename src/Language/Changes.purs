module Language.Changes where

import Prelude
import Prim hiding (Type)

import Data.List (List)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..))
import Data.Set (Set(..), difference, empty, filter, member)
import Data.Tuple (Tuple, snd)
import Language.Shape.Stlc.Metadata (defaultArrowTypeMetadata, defaultHoleTypeMetadata)
import Language.Shape.Stlc.Syntax (Constructor(..), TermID(..), Type(..), TypeID(..), freshHoleID)
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

type Changes = {
    termChanges :: Map TermID VarChange,
    matchChanges :: Map TypeID (List ConstructorChange),
    dataTypeDeletions :: Set TypeID
}

type KindChanges = Set TypeID -- set of datatypes which have been deleted

chType :: KindChanges -> TypeChange -> Type -> Type
chType chs (ArrowCh c1 c2) (ArrowType a b md) = ArrowType (chType chs c1 a) (chType chs c2 b) md
chType chs (InsertArg a) t = ArrowType a (chType chs NoChange t) defaultArrowTypeMetadata
chType chs Swap (ArrowType a (ArrowType b c md1) md2)
    = ArrowType (continue a) (ArrowType (continue b) (continue c) md1) md2
      where continue = chType chs NoChange
chType chs RemoveArg (ArrowType a b md) = chType chs NoChange b
chType chs (Replace a) t = a
chType chs NoChange (ArrowType a b md) = ArrowType (chType chs NoChange a) (chType chs NoChange a) md
chType chs NoChange (HoleType i w md) = HoleType i (difference w chs) md -- remove deleted datatypes from weakening -- TODO: is this how difference works?
chType chs NoChange (DataType i md) = if member i chs
    then HoleType (freshHoleID unit) empty defaultHoleTypeMetadata
    else DataType i md
chType chs _ _ = error "shouldn't get here"