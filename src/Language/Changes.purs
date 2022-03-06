module Language.Changes where

import Prelude
import Prim hiding (Type)

import Control.Monad.State (State, runState)
import Data.List (List(..), union)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..))
import Data.Set (Set(..), difference, empty, filter, member)
import Data.Tuple (Tuple(..), snd)
import Language.Shape.Stlc.Metadata (defaultArrowTypeMetadata, defaultHoleTermMetadata, defaultHoleTypeMetadata, defaultTermDefinitionMetadata)
import Language.Shape.Stlc.Syntax (Block(..), Constructor(..), Definition(..), Term(..), TermID(..), Type(..), TypeID(..), freshHoleID, freshTermID)
import Pipes.Prelude (mapM)
import Undefined (undefined)
import Unsafe (error)

data TypeChange
    = ArrowCh TypeChange TypeChange -- only applies to types of form (ArrowType a b _)
    | NoChange
    | InsertArg Type
    | Swap -- only applies to types of form (ArrowType a (ArrowType b c _) _)
    | RemoveArg -- only applies to types of form (ArrowType a b _)
    | Replace Type
-- Note for the future: could e.g. make Swap take a typechange which says what happens to rest of type after swap. Currently, it is implicitly NoChange.

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

-- TODO: is there a monad which is like State (List T) but only has put, and is for building a list of elements?
chTerm :: Changes -> TypeChange -> Term -> State (List Definition) Term
chTerm chs (Replace a) t = pure $ HoleTerm defaultHoleTermMetadata
chTerm chs (ArrowCh c1 d2) (LambdaTerm i b md) = undefined
chTerm chs NoChange (LambdaTerm i b md) = pure $ LambdaTerm i (chBlock chs NoChange b) md
chTerm chs (InsertArg t) (LambdaTerm i block md) = undefined
chTerm chs Swap (LambdaTerm i1 (Block defs (LambdaTerm i2 (Block defs2 t md4) md1) md2) md3)
    = pure $ LambdaTerm i2 (Block Nil (LambdaTerm i1 (chBlock chs NoChange (Block (defs <> defs2) t md4)) md3) md2) md1
chTerm chs RemoveArg (LambdaTerm i (Block defs t md) _) = chTerm undefined {-chs - i-} NoChange t -- also output defs into displaced
chTerm chs ch (ApplicationTerm i args md) = undefined
chTerm chs ch (HoleTerm md) = undefined
chTerm chs ch (MatchTerm i t cases) = undefined
chTerm chs _ _ = pure $ HoleTerm defaultHoleTermMetadata -- anything that doesn't fit a pattern just goes into a hole

chTerm2 :: Changes -> TypeChange -> Term -> State (List Definition) Term
chTerm2 chs _ _ = pure $ HoleTerm defaultHoleTermMetadata -- anything that doesn't fit a pattern just goes into a hole


chBlock :: Changes -> TypeChange -> Block -> Block
chBlock chs ch (Block defs t md)
    -- = Block (map (chDefinition chs) defs) (chTerm chs ch t) md
    = let (Tuple displaced1 t) = runState (chTerm chs ch t) Nil
      in let (Tuple displaced2 defs) = runState (map (chDefinition chs) defs) Nil
      in undefined

chDefinition :: Changes -> Definition -> State (List Definition) Definition
-- for data definitions, do nothing?
-- for term definitions, change both the type and term.
chDefinition chs = undefined

defFromTerm :: Term -> Definition
defFromTerm t = TermDefinition (freshTermID unit) undefined t defaultTermDefinitionMetadata
-- TODO: in order to get type, need to either pass type and context to all these functions, OR
-- need to go back to types on labmda parameters.


-- TODO: remember that it needs to be possible to have f : A -> B -> C, and in a buffer,
-- have f a, and change it to f a b. Also, to change f a b to f a. In other words, make sure that
-- when propagating changes upwards in wrap, this stuff all works.