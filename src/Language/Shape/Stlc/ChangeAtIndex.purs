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

chAtTerm' :: Rec.RecTerm (Syntax -> Change -> DownwardIndex -> Maybe (Term /\ DownwardIndex /\ TypeChange))
chAtTerm' t gamma ty (SyntaxTerm newTerm) (ChangeTypeChange tc) (DownwardIndex Nil)
    = Just (newTerm /\ DownwardIndex Nil /\ tc)
chAtTerm' t gamma ty tRep sbjto idx = Rec.recTerm {
    lambda : \termId block meta gamma param beta tRep sbjto -> case _ of
        -- NOTE: HERE
        (DownwardIndex (Cons (IndexStep StepLambdaTerm 1) Nil)) ->
            -- TODO: this is wrong. I should use tRep!!! Thats why it exists!
            let block' /\ holeSub = runState (chBlock gamma beta emptyChanges (castChangeTC sbjto) block) emptyHoleSub
            in pure $ LambdaTerm termId block' meta /\ emptyDownwardIndex /\ ArrowCh NoChange (castChangeTC sbjto)
        (DownwardIndex (Cons (IndexStep StepLambdaTerm 1) rest)) -> 
            do (block' /\ (DownwardIndex idx') /\ tc) <- chAtBlock block gamma beta tRep sbjto (DownwardIndex rest)
               pure $ LambdaTerm termId block' meta /\ (DownwardIndex (Cons (IndexStep StepLambdaTerm 2) idx')) /\ ArrowCh NoChange tc
        _ -> error "no"
    , neutral : \termId argItems meta gamma alpha tRep sbjto -> case _ of
        (DownwardIndex (Cons (IndexStep StepNeutralTerm 1) rest)) ->
            do (argItems' /\ (DownwardIndex idx')) <- chAtArgs argItems gamma alpha tRep sbjto (DownwardIndex rest)
               pure $ (NeutralTerm termId argItems' meta /\ (DownwardIndex (Cons (IndexStep StepNeutralTerm 2) idx')) /\ NoChange)
        _ -> error "no"
    , hole : \meta gamma alpha -> error "no" -- holes have no children, so StepHoleTerm is never used.
    , match : \dataID a cases meta gamma alpha tRep sbjto -> case _ of
        (DownwardIndex (Cons (IndexStep StepMatchTerm 1) rest)) ->
            -- do (cases' /\ (DownwardIndex idx')) <- chAtCase cases gamma alpha tRep sbjto rest
            -- Here, need to ask Henry about how to deal with cases and index.
            -- Morally there should be a Rec.CaseItemList recursor, but that seems annoying.
               undefined
        _ -> error "no"
} t gamma ty tRep sbjto idx


chAtTerm :: Rec.RecTerm (Syntax -> Change -> DownwardIndex -> Maybe (Term /\ DownwardIndex /\ TypeChange))
chAtTerm = Rec.recTerm {
    lambda : \termId block meta gamma param beta tRep sbjto -> case _ of
        -- NOTE: HERE
        (DownwardIndex (Cons (IndexStep StepLambdaTerm 1) Nil)) ->
            -- TODO: this is wrong. I should use tRep!!! Thats why it exists!
            let block' /\ holeSub = runState (chBlock gamma beta emptyChanges (castChangeTC sbjto) block) emptyHoleSub
            in pure $ LambdaTerm termId block' meta /\ emptyDownwardIndex /\ ArrowCh NoChange (castChangeTC sbjto)
        (DownwardIndex (Cons (IndexStep StepLambdaTerm 1) rest)) -> 
            do (block' /\ (DownwardIndex idx') /\ tc) <- chAtBlock block gamma beta tRep sbjto (DownwardIndex rest)
               pure $ LambdaTerm termId block' meta /\ (DownwardIndex (Cons (IndexStep StepLambdaTerm 2) idx')) /\ ArrowCh NoChange tc
        _ -> error "no"
    , neutral : \termId argItems meta gamma alpha tRep sbjto -> case _ of
        (DownwardIndex (Cons (IndexStep StepNeutralTerm 1) rest)) ->
            do (argItems' /\ (DownwardIndex idx')) <- chAtArgs argItems gamma alpha tRep sbjto (DownwardIndex rest)
               pure $ (NeutralTerm termId argItems' meta /\ (DownwardIndex (Cons (IndexStep StepNeutralTerm 2) idx')) /\ NoChange)
        _ -> error "no"
    , hole : \meta gamma alpha -> error "no" -- holes have no children, so StepHoleTerm is never used.
    , match : \dataID a cases meta gamma alpha tRep sbjto -> case _ of
        (DownwardIndex (Cons (IndexStep StepMatchTerm 1) rest)) ->
            -- do (cases' /\ (DownwardIndex idx')) <- chAtCase cases gamma alpha tRep sbjto rest
            -- Here, need to ask Henry about how to deal with cases and index.
            -- Morally there should be a Rec.CaseItemList recursor, but that seems annoying.
               undefined
        _ -> error "no"
}

chAtBlock :: Rec.RecBlock (Syntax -> Change -> DownwardIndex -> Maybe (Block /\ DownwardIndex /\ TypeChange))
chAtBlock = Rec.recBlock {
    block : \ defs t meta gamma ty tRep sbjto -> case _ of
        (DownwardIndex (Cons (IndexStep StepBlock 0) rest)) -> undefined -- definitions
        -- NOTE: HERE
        (DownwardIndex (Cons (IndexStep StepBlock 1) Nil)) -> undefined -- index points to term at end of block
        (DownwardIndex (Cons (IndexStep StepBlock 1) rest)) -> undefined -- into term at end of block
        _ -> error "no"
}
chAtBlock' :: Rec.RecBlock (Syntax -> Change -> DownwardIndex -> Maybe (Block /\ DownwardIndex /\ TypeChange))
chAtBlock' block gamma ty (SyntaxBlock newBlock) (ChangeTypeChange tc) (DownwardIndex Nil) =
    Just (newBlock /\ (DownwardIndex Nil) /\ tc)
chAtBlock' block gamma ty tRep sbjto idx = Rec.recBlock {
    block : \ defs t meta gamma ty tRep sbjto -> case _ of
        (DownwardIndex (Cons (IndexStep StepBlock 0) rest)) -> undefined -- definitions
        (DownwardIndex (Cons (IndexStep StepBlock 1) rest)) -> undefined -- into term at end of block
        _ -> error "no"
} block gamma ty tRep sbjto idx


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
chAtConstructor = Rec.recConstructor {
    constructor : \binding parameters meta typeId gamma ty tRep sbjto -> case _ of
        (DownwardIndex (Cons (IndexStep StepConstructor 1) rest)) -> undefined -- inside parameter list of constructor
        _ -> error "no"
}

chAtType :: Rec.RecType (Syntax -> Change -> DownwardIndex -> Maybe (Type /\ DownwardIndex /\ TypeChange))
chAtType = Rec.recType {
    arrow : \param out meta gamma tRep sbjto -> case _ of
        (DownwardIndex (Cons (IndexStep StepArrowType 0) rest)) -> undefined -- input param
        (DownwardIndex (Cons (IndexStep StepArrowType 1) rest)) -> undefined -- output type
        _ -> error "no"
    , data : \typeId meta gamma tRep sbjto idx -> error "no" -- shouldn't get here, data type has no children (also, StepDataType doesn't need to exists in Index.purs)
    , hole : \holeId weakenedBy meta gamma tRep sbjto -> error "no"-- shouldn't get here, hole types have no children (also, StepHoleType doesn't need to exists in Index.purs)
    , proxyHole : \holeId gamma tRep sbjto -> error "no"-- shouldn't get here, proxy hole types have no children
}

-- doesn't return a TypeChange for the same reason that chAtArgs doesn't
chAtCase :: Rec.RecCase (Syntax -> Change -> DownwardIndex -> Maybe (Case /\ DownwardIndex))
chAtCase = Rec.recCase {
    --                                  v Question for henry: what is this termId???
    case_ : \bindings block meta typeId termId  gamma ty tRep sbjTo -> case _ of
        (DownwardIndex (Cons (IndexStep StepCase 0) rest)) -> undefined -- term in the case
        _ -> error "no"
}

-- does this need to return a TypeChange? Consider once I know where it is called.
chAtParameter :: Rec.RecParameter (Syntax -> Change -> DownwardIndex -> Maybe (Parameter /\ DownwardIndex /\ TypeChange))
chAtParameter = undefined


{-
Issue : when a change is done at a hole (and other situations?) a HoleSub needs to be outputted as well.
Do I need all of these functions to also output a HoleSub in the Maybe?

Another Issue: look at the two places that I wrote NOTE: HERE.
Should I really deal with a change in a term or block at the parent of each place that has
a term or block? That seems redundant.
Instead, chAtTerm and chAtBlock could have a case where they see that the idx is Nil, and then
just call chTerm (or chBlock) on themselves right there.
But, this seems to conflict with the stucture that I get from using a recursor.
I need to check this BEFORE I match on whether its a lambda,match,hole, ...
-}