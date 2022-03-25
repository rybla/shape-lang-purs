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
import Language.Shape.Stlc.Syntax (Block(..), Case(..), Constructor(..), Definition, DefinitionItem, Module(..), Parameter(..), Syntax(..), Term(..), ArgItem)
import Undefined (undefined)
import Unsafe (error)

data Change
    = ChangeTypeChange TypeChange
    | ChangeConstructorChange ConstructorChange
    | ChangeNone

castChangeTC :: Change -> TypeChange
castChangeTC = case _ of
    (ChangeTypeChange tc) -> tc
    _ -> error "no"

-- do these two need to exist?
-- chAtIndex :: Module -> DownwardIndex -> Syntax -> Change -> Module /\ DownwardIndex
-- chAtIndex m i s tc
--     = case chAtIndexImpl (SyntaxModule m) i s tc of
--            ((SyntaxModule m) /\ i' /\ Just NoChange) -> Tuple m i'
--            _ -> error "no"

-- chAtIndexImpl :: Syntax -> DownwardIndex -> Syntax -> Change -> Syntax /\ DownwardIndex /\ Maybe TypeChange
-- chAtIndexImpl = undefined

-- chAtTerm = undefined

chAtModule :: Rec.RecModule (Syntax -> Change -> DownwardIndex -> Maybe (Module /\ DownwardIndex))
chAtModule = undefined

chAtDefinitionItems :: Rec.RecDefinitionItems (Syntax -> Change -> DownwardIndex
    -> Maybe (List DefinitionItem /\ DownwardIndex))
chAtDefinitionItems = undefined

chAtDefinition :: Rec.RecDefinition (Syntax -> Change -> DownwardIndex -> Maybe (Definition /\ DownwardIndex))
chAtDefinition = undefined

chAtTerm :: Rec.RecTerm (Syntax -> Change -> DownwardIndex -> Maybe (Term /\ DownwardIndex /\ TypeChange))
chAtTerm = Rec.recTerm {
    lambda : \termId block meta gamma param beta tRep sbjto -> case _ of
        (DownwardIndex (Cons (IndexStep StepLambdaTerm 2) Nil)) ->
            let block' /\ holeSub = runState (chBlock gamma beta emptyChanges (castChangeTC sbjto) block) emptyHoleSub
            in pure $ LambdaTerm termId block' meta /\ emptyDownwardIndex /\ ArrowCh NoChange (castChangeTC sbjto)
        (DownwardIndex (Cons (IndexStep StepLambdaTerm 2) rest)) -> 
            let (block' /\ (DownwardIndex idx') /\ tc) = chAtBlock block gamma beta tRep sbjto (DownwardIndex rest)
            in pure $ LambdaTerm termId block' meta /\ (DownwardIndex (Cons (IndexStep StepLambdaTerm 2) idx')) /\ ArrowCh NoChange tc
        _ -> error "no"
    , neutral : \termId argItems meta gamma alpha tRep sbjto -> case _ of
        (DownwardIndex (Cons (IndexStep StepNeutralTerm 2) rest)) ->
            do (argItems' /\ (DownwardIndex idx')) <- chAtArgs argItems gamma alpha tRep sbjto (DownwardIndex rest)
               pure $ (NeutralTerm termId argItems' meta /\ (DownwardIndex (Cons (IndexStep StepNeutralTerm 2) idx')) /\ NoChange)
        _ -> error "no"
    , hole : \meta gamma alpha -> error "no" -- holes have no children, so StepHoleTerm is never used.
    , match : \dataID a cases meta gamma alpha tRep sbjto -> case _ of
        (DownwardIndex (Cons (IndexStep StepMatchTerm 1) rest)) ->
            -- do (cases' /\ (DownwardIndex idx')) <- chAtCase cases gamma alpha tRep sbjto rest
               undefined
        _ -> error "no"
}

chAtBlock :: Rec.RecBlock (Syntax -> Change -> DownwardIndex -> Block /\ DownwardIndex /\ TypeChange)
chAtBlock = undefined

chAtArgs :: Rec.RecArgItems (Syntax -> Change -> DownwardIndex -> Maybe (List ArgItem /\ DownwardIndex))
chAtArgs = Rec.recArgItems {
    nil : \ty tRep sbjto idx -> error "can't change the end of an args list using chAtIndex"
    , cons : \(t /\ md) argItems gamma (Parameter ty tymd) alpha tRep sbjto -> case _ of
        (DownwardIndex (Cons (IndexStep StepArgItem 0) rest)) ->
            do (t' /\ (DownwardIndex idx') /\ ch) <- chAtTerm t gamma ty tRep sbjto (DownwardIndex rest)
               case ch of
                  NoChange -> Just (Cons (t' /\ md) argItems /\ (DownwardIndex (Cons (IndexStep StepArgItem 0) idx')))
                  _ -> Nothing -- can't change type of something on right of an application.
        (DownwardIndex (Cons (IndexStep StepArgItem 1) rest)) -> 
            do (argItems' /\ DownwardIndex idx') <- chAtArgs argItems gamma alpha tRep sbjto (DownwardIndex rest)
               pure $ (Cons (t /\ md) argItems') /\ DownwardIndex (Cons (IndexStep StepArgItem 1) idx')
        _ -> error "no"

}

chAtConstructor :: Rec.RecConstructor (Syntax -> Change -> DownwardIndex -> Maybe (Constructor /\ DownwardIndex /\ ConstructorChange))
chAtConstructor = undefined

chAtType :: Rec.RecType (Syntax -> Change -> DownwardIndex -> Maybe (Type /\ DownwardIndex /\ TypeChange))
chAtType = undefined

-- doesn't return a TypeChange for the same reason that chAtArgs doesn't
chAtCase :: Rec.RecCase (Syntax -> Change -> DownwardIndex -> Maybe (Case /\ DownwardIndex))
chAtCase = undefined

-- does this need to return a TypeChange? Consider once I know where it is called.
chAtParameter :: Rec.RecParameter (Syntax -> Change -> DownwardIndex -> Maybe (Parameter /\ DownwardIndex /\ TypeChange))
chAtParameter = undefined


{-
Issue: you shouldn't be able to put a TypeChange for a term to the right of an application.
So Change should have a constructor called ChangeNoChange, and this is checked when doing something on the right?
No, that doesn't handle recursive things correctly.

All chAt* need to return a Maybe (bla /\ bla /\ ..), and then chAtArgs needs to check that the typechange of it's
terms are NoChange. If not, then return Nothing.
-}