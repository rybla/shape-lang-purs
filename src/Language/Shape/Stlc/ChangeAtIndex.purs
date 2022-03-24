module Language.Shape.Stlc.ChangeAtIndex where

import Data.Tuple.Nested
import Prelude

import Control.Monad.State (runState)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Language.Shape.Stlc.Changes (ConstructorChange, TypeChange(..), chArgs, chBlock, emptyChanges, emptyDisplaced)
import Language.Shape.Stlc.Holes (emptyHoleSub)
import Language.Shape.Stlc.Index (DownwardIndex(..), IndexStep(..), StepLabel(..), emptyDownwardIndex, unconsDownwardIndex)
import Language.Shape.Stlc.Recursion.Context as Rec
import Language.Shape.Stlc.Syntax (ArgItem, Block(..), Constructor(..), Module(..), Syntax(..), Term(..))
import Undefined (undefined)
import Unsafe (error)

data Change
    = ChangeTypeChange TypeChange
    | ChangeConstructorChange ConstructorChange

castChangeTC :: Change -> TypeChange
castChangeTC = case _ of
    (ChangeTypeChange tc) -> tc
    _ -> error "no"

chAtIndex :: Module -> DownwardIndex -> Syntax -> Change -> Module /\ DownwardIndex
chAtIndex m i s tc
    = case chAtIndexImpl (SyntaxModule m) i s tc of
           ((SyntaxModule m) /\ i' /\ Just NoChange) -> Tuple m i'
           _ -> error "no"

chAtIndexImpl :: Syntax -> DownwardIndex -> Syntax -> Change -> Syntax /\ DownwardIndex /\ Maybe TypeChange
chAtIndexImpl = undefined

-- chAtTerm = undefined

chAtTerm :: Rec.RecTerm (Syntax -> Change -> DownwardIndex -> Term /\ DownwardIndex /\ TypeChange)
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
            let block' /\ holeSub = runState (chBlock gamma beta emptyChanges (castChangeTC sbjto) block) emptyHoleSub
            in LambdaTerm termId block' meta /\ emptyDownwardIndex /\ ArrowCh NoChange (castChangeTC sbjto)
        (DownwardIndex (Cons (IndexStep StepLambdaTerm 2) rest)) -> 
            let (block' /\ (DownwardIndex idx') /\ tc) = chAtBlock block gamma beta tRep sbjto (DownwardIndex rest)
            in LambdaTerm termId block' meta /\ (DownwardIndex (Cons (IndexStep StepLambdaTerm 2) idx')) /\ ArrowCh NoChange tc
        _ -> error "no"
    -- I don't think that there is a Nil case here.
    , neutral : \termId argItems meta gamma alpha tRep sbjto -> case _ of
        -- (DownwardIndex (Cons (IndexStep StepNeutralTerm 2) Nil)) ->
        --     let args' /\ holeSub /\ displaced = runState (chArgs gamma alpha emptyChanges sbjto argItems) (emptyDisplaced /\ emptyHoleSub) 
        --     in NeutralTerm termId undefined meta /\ emptyDownwardIndex /\ undefined
        (DownwardIndex (Cons (IndexStep StepNeutralTerm 2) rest)) ->
            let (argItems' /\ (DownwardIndex idx')) = chAtArgs argItems gamma alpha tRep sbjto (DownwardIndex rest)
            in (NeutralTerm termId argItems' meta /\ (DownwardIndex (Cons (IndexStep StepNeutralTerm 2) idx')) /\ NoChange)
        _ -> error "no"
    , hole : \meta gamma alpha -> error "no" -- holes have no children, so StepHoleTerm is never used.
    , match : \dataID a cases meta gamma alpha -> undefined
}

chAtBlock :: Rec.RecBlock (Syntax -> Change -> DownwardIndex -> Block /\ DownwardIndex /\ TypeChange)
chAtBlock = undefined

chAtArgs :: Rec.RecArgItems (Syntax -> Change -> DownwardIndex -> List ArgItem /\ DownwardIndex)
chAtArgs = undefined

chAtConstructor :: Rec.RecConstructor (Syntax -> Change -> DownwardIndex -> (Constructor /\ DownwardIndex /\ ConstructorChange))
chAtConstructor = undefined


{-
Issue: you shouldn't be able to put a TypeChange for a term to the right of an application.
So Change should have a constructor called ChangeNoChange, and this is checked when doing something on the right?
No, that doesn't handle recursive things correctly.

All chAt* need to return a Maybe (bla /\ bla /\ ..), and then chAtArgs needs to check that the typechange of it's
terms are NoChange. If not, then return Nothing.
-}