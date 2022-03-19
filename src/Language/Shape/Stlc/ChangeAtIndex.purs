module Language.Shape.Stlc.ChangeAtIndex where
{-

import Data.Tuple.Nested

import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Language.Shape.Stlc.Changes (TypeChange(..))
import Language.Shape.Stlc.Index (DownwardIndex(..), IndexStep(..), StepLabel(..))
import Language.Shape.Stlc.Recursion.Context as Rec
import Language.Shape.Stlc.Syntax (Block(..), Module(..), Syntax(..), Term)
import Undefined (undefined)
import Unsafe (error)

chAtIndex m i s tc
    = case chAtIndexImpl (SyntaxModule m) i s tc of
           ((SyntaxModule m) /\ i' /\ Just NoChange) -> Tuple m i'
           _ -> error "no"

chAtIndexImpl :: Syntax -> DownwardIndex -> Syntax -> TypeChange
chAtIndexImpl = undefined

chAtModule = undefined

-- chAtTerm = undefined

-- chAtTerm :: Rec.RecTerm (Syntax -> TypeChange -> DownwardIndex -> Term /\ DownwardIndex /\ TypeChange)
-- chAtTerm = Rec.recTerm {
--     lambda : \termId block meta gamma param beta tRep sbjto -> ?h -- case _ of
--         -- (Cons (IndexStep StepBlock i) rest) -> ?h
--         -- _ -> ?h
--     , neutral : \termId argItems meta gamma alpha -> ?h
--     , hole : \meta gamma alpha -> ?h
--     , match : \dataID a cases meta gamma alpha -> ?h
-- }

chAtBlock :: Rec.RecBlock (DownwardIndex -> Syntax -> TypeChange -> Term /\ DownwardIndex /\ TypeChange)
chAtBlock = undefined

-}

-- I should use the Context recursor in order to deal with keeping track of the
-- types and context.