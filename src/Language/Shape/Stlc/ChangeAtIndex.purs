module Language.Shape.Stlc.ChangeAtIndex where

import Data.Tuple.Nested
import Prelude

import Control.Monad.State (runState)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Language.Shape.Stlc.Changes (TypeChange(..), chArgs, chBlock, emptyChanges, emptyDisplaced)
import Language.Shape.Stlc.Holes (emptyHoleSub)
import Language.Shape.Stlc.Index (DownwardIndex(..), IndexStep(..), StepLabel(..), emptyDownwardIndex, unconsDownwardIndex)
import Language.Shape.Stlc.Recursion.Context as Rec
import Language.Shape.Stlc.Syntax (Block(..), Module(..), Syntax(..), Term(..))
import Undefined (undefined)
import Unsafe (error)

chAtIndex :: Module -> DownwardIndex -> Syntax -> TypeChange -> Module /\ DownwardIndex
chAtIndex m i s tc
    = case chAtIndexImpl (SyntaxModule m) i s tc of
           ((SyntaxModule m) /\ i' /\ Just NoChange) -> Tuple m i'
           _ -> error "no"

chAtIndexImpl :: Syntax -> DownwardIndex -> Syntax -> TypeChange -> Syntax /\ DownwardIndex /\ Maybe TypeChange
chAtIndexImpl = undefined

-- chAtTerm = undefined

chAtTerm :: Rec.RecTerm (Syntax -> TypeChange -> DownwardIndex -> Term /\ DownwardIndex /\ TypeChange)
chAtTerm = Rec.recTerm {
    -- lambda : \termId block meta gamma param beta tRep sbjto idx -> case unconsDownwardIndex idx of
    --     (Just {step : IndexStep StepLambdaTerm 2, ix' : rest}) -> 
    --         let (block' /\ idx' /\ tc) = chAtBlock block gamma beta tRep sbjto rest
    --         in LambdaTerm termId block' meta /\ idx' /\ ArrowCh NoChange tc
    --     Nothing -> let block' /\ holeSub = runState (chBlock gamma beta emptyChanges sbjto block) emptyHoleSub
    --                in LambdaTerm termId block' meta /\ emptyDownwardIndex /\ ArrowCh NoChange sbjto
    --     _ -> error "no"
    --     -- I think this is wrong. I think we need to match on rest to find out if child should be changed?
    lambda : \termId block meta gamma param beta tRep sbjto -> case _ of
        (DownwardIndex (Cons (IndexStep StepLambdaTerm 2) Nil)) ->
            let block' /\ holeSub = runState (chBlock gamma beta emptyChanges sbjto block) emptyHoleSub
            in LambdaTerm termId block' meta /\ emptyDownwardIndex /\ ArrowCh NoChange sbjto
        (DownwardIndex (Cons (IndexStep StepLambdaTerm 2) rest)) -> 
            let (block' /\ idx' /\ tc) = chAtBlock block gamma beta tRep sbjto (DownwardIndex rest)
            in LambdaTerm termId block' meta /\ idx' /\ ArrowCh NoChange tc
        _ -> error "no"
    -- I don't think that there is a Nil case here.
    , neutral : \termId argItems meta gamma alpha tRep sbjto -> case _ of
        (DownwardIndex (Cons (IndexStep StepNeutralTerm 2) Nil)) ->
            let args' /\ holeSub /\ displaced = runState (chArgs gamma alpha emptyChanges sbjto argItems) (emptyDisplaced /\ emptyHoleSub) 
            in NeutralTerm termId undefined meta /\ emptyDownwardIndex /\ undefined
        (DownwardIndex (Cons (IndexStep StepNeutralTerm 2) rest)) -> undefined
        _ -> error "no"
    , hole : \meta gamma alpha -> error "no" -- holes have no children, so StepHoleTerm is never used.
    , match : \dataID a cases meta gamma alpha -> undefined
}

chAtBlock :: Rec.RecBlock (Syntax -> TypeChange -> DownwardIndex -> Block /\ DownwardIndex /\ TypeChange)
chAtBlock = undefined

-- I should use the Context recursor in order to deal with keeping track of the
-- types and context.