module Language.Shape.Stlc.ChangeAtIndex where

import Data.Tuple.Nested
import Prelude
import Prim hiding (Type)

import Control.Monad.State (State, runState)
import Data.List (List(..), foldl, mapWithIndex, singleton)
import Data.List.Unsafe (index')
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Language.Shape.Stlc.Changes (ConstructorChange, TypeChange(..), chArgs, chBlock, chDefinition, emptyChanges, emptyDisplaced, varChange)
import Language.Shape.Stlc.Holes (HoleSub, emptyHoleSub)
import Language.Shape.Stlc.Index (DownwardIndex(..), IndexStep(..), StepLabel(..), emptyDownwardIndex, unconsDownwardIndex)
import Language.Shape.Stlc.Metadata (DefinitionItemMetadata, defaultDefinitionItemMetadata)
import Language.Shape.Stlc.Recursion.Context as Rec
import Language.Shape.Stlc.Syntax (ArgItem, Block(..), Case(..), Constructor(..), Definition(..), DefinitionItem, Module(..), Parameter(..), Syntax(..), Term(..), TermBinding(..), Type(..), TypeBinding(..))
import Partial.Unsafe (unsafePartial)
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

-- chAtModule module index (? -> T) (ChangeTypeChange (InsertArg ?))

chAtModule :: Rec.RecModule (Syntax -> Change -> DownwardIndex -> Maybe (Module /\ DownwardIndex /\ HoleSub))
chAtModule = Rec.recModule {
    module_ : \defs meta gamma tRep sbjto -> case _ of
        (DownwardIndex (Cons (IndexStep StepModule 0) rest)) -> undefined
        _ -> error "no"
}

convertIndexAtList :: DownwardIndex -> (Int /\ DownwardIndex)
convertIndexAtList (DownwardIndex (Cons (IndexStep StepCons 0) idx)) = 0 /\ (DownwardIndex idx)
convertIndexAtList (DownwardIndex (Cons (IndexStep StepCons 1) idx))
    = let (n /\ idx') = convertIndexAtList (DownwardIndex idx)
      in (n + 1) /\ (DownwardIndex idx)
convertIndexAtList _ = error "no"

mapExceptAt :: forall a b. Int -> b -> (a -> b) -> List a -> List b
mapExceptAt n atN f xs = mapWithIndex (\m x -> if m == n then atN else f x) xs


chAtDefinitionItems :: Rec.RecDefinitionItems (Syntax -> Change -> DownwardIndex
    -> Maybe (List DefinitionItem /\ DownwardIndex /\ HoleSub))
chAtDefinitionItems = Rec.recDefinitionItems {
    definitionItems : unsafePartial \defs gamma tRep sbjto idx -> do
        let (n /\ (DownwardIndex (Cons (IndexStep StepDefinitionItem 0) idx'))) = convertIndexAtList idx
        let (def /\ md) = index' defs n
        (def' /\ (DownwardIndex idx'') /\ tc /\ holeSub)
            <- chAtDefinition def gamma tRep sbjto (DownwardIndex idx')
        case def' of
            (TermDefinition (TermBinding x _) _ _ _) -> do
                let changes = varChange emptyChanges x tc
                let intermediateComputation1 :: List (State (Tuple (List Definition) HoleSub) Definition /\ DefinitionItemMetadata)
                    intermediateComputation1 = mapExceptAt n (pure def' /\ md)
                        (\(d /\ md) -> chDefinition gamma changes d /\ md)
                        defs
                let defs' /\ holeSub = foldl (
                        \ (defItemsAcc /\ holeSubAcc) (state /\ md) ->
                            let def /\ displaced /\ holeSub = runState state (Nil /\ holeSubAcc)
                            in (defItemsAcc <> (map (_ /\ defaultDefinitionItemMetadata) displaced) <> singleton (def /\ md)) /\ holeSub
                    ) (Nil /\ emptyHoleSub) intermediateComputation1
                Just (defs' /\ (DownwardIndex (Cons (IndexStep StepDefinitionItem 0) idx'')) /\ holeSub)
            (DataDefinition _ _ _) -> error "not implemented yet whoops"
}
-- chAtDefinitionItems = Rec.recDefinitionItems {
--     definitionItems : unsafePartial \(Cons def defs) gamma tRep sbjto -> case _ of -- will never be Nil, because if there is an Index into the definitions then it has to point to somewhere
--         (DownwardIndex (Cons (IndexStep StepCons 0) rest)) -> undefined
--         (DownwardIndex (Cons (IndexStep StepCons 1) rest)) -> do
--             (Cons rest restOfDefs /\ newIdx /\ holeSub) <- chAtDefinitionItems defs gamma tRep sbjto (DownwardIndex rest)
--             undefined
--         _ -> error "no"
--         -- TODO: what Step* goes in the undefined here?
--         -- (Downwardindex (Cons (IndexStep undefined 1) rest)) -> undefined
-- }

-- TODO: should be able to change either a data definition or constructor list to add/remove/change constructors
chAtDefinition :: Rec.RecDefinition (Syntax -> Change -> DownwardIndex -> Maybe (Definition /\ DownwardIndex /\ TypeChange /\ HoleSub))
chAtDefinition = Rec.recDefinition {
    data : \binding constructors meta gamma tRep sbjto -> case _ of
        (DownwardIndex (Cons (IndexStep StepDataDefinition 1) rest)) -> undefined -- I think this is the list of constructors
        _ -> error "no"
    , term : \binding ty t meta gamma tRep sbjto -> case _ of
        (DownwardIndex (Cons (IndexStep StepTermDefinition 1) rest)) -> undefined -- type
        (DownwardIndex (Cons (IndexStep StepTermDefinition 2) rest)) -> undefined -- term
        _ -> error "no"
}

chAtTerm :: Rec.RecTerm (Syntax -> Change -> DownwardIndex -> Maybe (Term /\ DownwardIndex /\ TypeChange /\ HoleSub))
chAtTerm t gamma ty (SyntaxTerm newTerm) (ChangeTypeChange tc) (DownwardIndex Nil)
    = Just (newTerm /\ DownwardIndex Nil /\ tc /\ emptyHoleSub)
chAtTerm t gamma ty tRep sbjto idx = Rec.recTerm {
    lambda : \termId block meta gamma param beta tRep sbjto -> case _ of
        (DownwardIndex (Cons (IndexStep StepLambdaTerm 1) rest)) -> 
            do (block' /\ (DownwardIndex idx') /\ tc /\ holeSub) <- chAtBlock block gamma beta tRep sbjto (DownwardIndex rest)
               pure $ LambdaTerm termId block' meta /\ (DownwardIndex (Cons (IndexStep StepLambdaTerm 2) idx')) /\ ArrowCh NoChange tc /\ holeSub
        _ -> error "no"
    , neutral : \termId argItems meta gamma alpha tRep sbjto -> case _ of
        (DownwardIndex (Cons (IndexStep StepNeutralTerm 1) rest)) ->
            do (argItems' /\ (DownwardIndex idx') /\ holeSub) <- chAtArgs argItems gamma alpha tRep sbjto (DownwardIndex rest)
               pure $ (NeutralTerm termId argItems' meta /\ (DownwardIndex (Cons (IndexStep StepNeutralTerm 2) idx')) /\ NoChange /\ holeSub)
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


chAtBlock :: Rec.RecBlock (Syntax -> Change -> DownwardIndex -> Maybe (Block /\ DownwardIndex /\ TypeChange /\ HoleSub))
-- TODO: unsure if I need this Nil case, see note at end of file.
chAtBlock block gamma ty (SyntaxBlock newBlock) (ChangeTypeChange tc) (DownwardIndex Nil) =
    Just (newBlock /\ (DownwardIndex Nil) /\ tc /\ emptyHoleSub)
chAtBlock block gamma ty tRep sbjto idx = Rec.recBlock {
    block : \ defs t meta gamma ty tRep sbjto -> case _ of
        (DownwardIndex (Cons (IndexStep StepBlock 0) rest)) -> undefined -- definitions
        (DownwardIndex (Cons (IndexStep StepBlock 1) rest)) -> undefined -- into term at end of block
        _ -> error "no"
} block gamma ty tRep sbjto idx


chAtArgs :: Rec.RecArgItems (Syntax -> Change -> DownwardIndex -> Maybe (List ArgItem /\ DownwardIndex /\ HoleSub))
chAtArgs = Rec.recArgItems {
    nil : \ty tRep sbjto idx -> error "can't change the end of an args list using chAtIndex"
    , cons : \(t /\ md) argItems gamma (Parameter ty tymd) alpha tRep sbjto -> case _ of
        (DownwardIndex (Cons (IndexStep StepArgItem 0) rest)) ->
            do (t' /\ (DownwardIndex idx') /\ ch /\ holeSub) <- chAtTerm t gamma ty tRep sbjto (DownwardIndex rest)
               case ch of
                  NoChange -> Just (Cons (t' /\ md) argItems /\ (DownwardIndex (Cons (IndexStep StepArgItem 0) idx')) /\ holeSub)
                  _ -> Nothing -- can't change type of something on right of an application.
        (DownwardIndex (Cons (IndexStep StepArgItem 1) rest)) -> 
            do (argItems' /\ DownwardIndex idx' /\ holeSub) <- chAtArgs argItems gamma alpha tRep sbjto (DownwardIndex rest)
               pure $ (Cons (t /\ md) argItems') /\ DownwardIndex (Cons (IndexStep StepArgItem 1) idx') /\ holeSub
        _ -> error "no"

}

chAtConstructor :: Rec.RecConstructor (Syntax -> Change -> DownwardIndex -> Maybe (Constructor /\ DownwardIndex /\ ConstructorChange /\ HoleSub))
chAtConstructor = Rec.recConstructor {
    constructor : \binding parameters meta typeId gamma ty tRep sbjto -> case _ of
        (DownwardIndex (Cons (IndexStep StepConstructor 1) rest)) -> undefined -- inside parameter list of constructor
        _ -> error "no"
}

chAtType :: Rec.RecType (Syntax -> Change -> DownwardIndex -> Maybe (Type /\ DownwardIndex /\ TypeChange /\ HoleSub))
chAtType ty gamma (SyntaxType newTy) (ChangeTypeChange tc) (DownwardIndex Nil)
    = Just (newTy /\ DownwardIndex Nil /\ tc /\ emptyHoleSub)
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
chAtCase :: Rec.RecCase (Syntax -> Change -> DownwardIndex -> Maybe (Case /\ DownwardIndex /\ HoleSub))
chAtCase = Rec.recCase {
    --                                  v Question for henry: what is this termId???
    case_ : \bindings block meta typeId termId  gamma ty tRep sbjTo -> case _ of
        (DownwardIndex (Cons (IndexStep StepCase 0) rest)) -> undefined -- term in the case
        _ -> error "no"
}

-- does this need to return a TypeChange? Consider once I know where it is called.
chAtParameter :: Rec.RecParameter (Syntax -> Change -> DownwardIndex -> Maybe (Parameter /\ DownwardIndex /\ TypeChange /\ HoleSub))
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