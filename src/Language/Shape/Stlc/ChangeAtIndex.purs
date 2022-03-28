module Language.Shape.Stlc.ChangeAtIndex where

import Data.Tuple.Nested
import Prelude
import Prim hiding (Type)

import Control.Monad.State (runState)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Language.Shape.Stlc.Changes (ConstructorChange, TypeChange(..), chArgs, chBlock, emptyChanges, emptyDisplaced)
import Language.Shape.Stlc.Holes (emptyHoleSub)
import Language.Shape.Stlc.Index (DownwardIndex(..), IndexStep(..), StepLabel(..), emptyDownwardIndex, unconsDownwardIndex)
import Language.Shape.Stlc.Recursion.Context as Rec
import Language.Shape.Stlc.Syntax (Block(..), Case(..), Constructor(..), Definition, DefinitionItem, Module(..), Parameter(..), Syntax(..), Term(..), ArgItem, Type(..))
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
chAtModule = Rec.recModule {
    module_ : \defs meta gamma tRep sbjto -> case _ of
        (DownwardIndex (Cons (IndexStep StepModule 0) rest)) -> undefined
        _ -> error "no"
}

chAtDefinitionItems :: Rec.RecDefinitionItems (Syntax -> Change -> DownwardIndex
    -> Maybe (List DefinitionItem /\ DownwardIndex))
chAtDefinitionItems = Rec.recDefinitionItems {
    definitionItems : \defs gamma tRep sbjto -> undefined -- case _ of
        -- TODO: what Step* goes in the undefined here?
        -- (Downwardindex (Cons (IndexStep undefined 1) rest)) -> undefined
}

-- TODO: should be able to change either a data definition or constructor list to add/remove/change constructors
chAtDefinition :: Rec.RecDefinition (Syntax -> Change -> DownwardIndex -> Maybe (Definition /\ DownwardIndex))
chAtDefinition = Rec.recDefinition {
    data : \binding constructors meta gamma tRep sbjto -> case _ of
        (DownwardIndex (Cons (IndexStep StepDataDefinition 1) rest)) -> undefined -- I think this is the list of constructors
        _ -> error "no"
    , term : \binding ty t meta gamma tRep sbjto -> case _ of
        (DownwardIndex (Cons (IndexStep StepTermDefinition 1) rest)) -> undefined -- type
        (DownwardIndex (Cons (IndexStep StepTermDefinition 2) rest)) -> undefined -- term
        _ -> error "no"
}

chAtTerm :: Rec.RecTerm (Syntax -> Change -> DownwardIndex -> Maybe (Term /\ DownwardIndex /\ TypeChange))
chAtTerm t gamma ty (SyntaxTerm newTerm) (ChangeTypeChange tc) (DownwardIndex Nil)
    = Just (newTerm /\ DownwardIndex Nil /\ tc)
chAtTerm t gamma ty tRep sbjto idx = Rec.recTerm {
    lambda : \termId block meta gamma param beta tRep sbjto -> case _ of
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


chAtBlock :: Rec.RecBlock (Syntax -> Change -> DownwardIndex -> Maybe (Block /\ DownwardIndex /\ TypeChange))
-- TODO: unsure if I need this Nil case, see note at end of file.
chAtBlock block gamma ty (SyntaxBlock newBlock) (ChangeTypeChange tc) (DownwardIndex Nil) =
    Just (newBlock /\ (DownwardIndex Nil) /\ tc)
chAtBlock block gamma ty tRep sbjto idx = Rec.recBlock {
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
chAtType ty gamma (SyntaxType newTy) (ChangeTypeChange tc) (DownwardIndex Nil)
    = Just (newTy /\ DownwardIndex Nil /\ tc)
chAtType ty gamma tRep sbjto idx = Rec.recType {
    arrow : \param out meta gamma tRep sbjto -> case _ of
        (DownwardIndex (Cons (IndexStep StepArrowType 0) rest)) -> undefined -- input param
        (DownwardIndex (Cons (IndexStep StepArrowType 1) rest)) -> undefined -- output type
        _ -> error "no"
    , data : \typeId meta gamma tRep sbjto idx -> error "no" -- shouldn't get here, data type has no children (also, StepDataType doesn't need to exists in Index.purs)
    , hole : \holeId weakenedBy meta gamma tRep sbjto -> error "no"-- shouldn't get here, hole types have no children (also, StepHoleType doesn't need to exists in Index.purs)
    , proxyHole : \holeId gamma tRep sbjto -> error "no"-- shouldn't get here, proxy hole types have no children
} ty gamma tRep sbjto idx

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
chAtParameter = Rec.recParameter {
    parameter : \ty meta gamma tRep sbjto -> case _ of
        (DownwardIndex (Cons (IndexStep StepParameter 0) rest)) -> undefined -- the type in the parameter
        _ -> error "no"
}


{-
Issue : when a change is done at a hole (and other situations?) a HoleSub needs to be outputted as well.
Do I need all of these functions to also output a HoleSub in the Maybe?

A question: in our interface, do we ever need to call chAtIndex where the index is pointing to
anything other than a Term or a Type? Aren't these the only thing which can be
copy/cut/pasted?
If so, then only chAtType and chAtTerm need to have the special case where they don't just
use the recursor.

Another TODO: shouldn't Parameter have two children, the first of which is the name of the variable?
-}