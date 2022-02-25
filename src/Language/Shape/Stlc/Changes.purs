module Language.Shape.Stlc.Model
  ( Changes
  , DataChange(..)
  , InputChange(..)
  , TypeChange(..)
  , VarChange(..)
  , chType
  , searchType
  )
  where


import Data.Maybe
import Data.Symbol
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)

import Data.List (List(..), (!!), filter)
import Data.Map (Map, lookup)
import Data.Map as Map
import Data.Tuple (Tuple(..), fst, snd)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)
import Prim.Boolean (True)
import Undefined (undefined)

data TypeChange = TypeReplace Type | NoChange
  | ArrowChange (List InputChange) TypeChange
-- data BaseTypeChange = TypeReplace Type | NoChange
data InputChange = Change TypeChange Int | Insert Type

-- Example:
-- (A,B,C)->D
-- (A,C,?)->D
-- ArrowChange [Change NoChange 0, Change NoChange 2, Insert] NoChange

data VarChange = VariableTypeChange TypeChange | VariableDeletion

data DataChange = DataTypeDeletion

type Changes = Tuple (Map TermId VarChange) (Map TypeId DataChange)

saneIndex :: forall a . List a -> Int -> a
saneIndex l i = unsafePartial $ fromJust (l !! i)

error :: forall a . String -> a
error s = unsafePartial (crashWith s)

infixr 5 saneIndex as !!!

chType :: Changes -> TypeChange -> Type -> Type
chType gamma c t =
  let chTypeImpl :: TypeChange -> Type -> Type
      chTypeImpl NoChange t = t
      chTypeImpl (TypeReplace t2) t1 = t2
      chTypeImpl (ArrowChange ics out) (ArrowType as b)
        = ArrowType (map mapper ics) bNew
        where bNew = case out of
                TypeReplace (BaseType t) -> t
                NoChange -> b
                _ -> error "shouldn't happen"
              mapper :: InputChange -> Tuple TermName Type
              mapper (Change tc n) = Tuple (fst $ as !!! n) (chTypeImpl tc (snd $ as !!! n))
              mapper (Insert t) = Tuple (VariableName "_") t
      chTypeImpl (ArrowChange _ _) (BaseType bt) = error "Can't ArrowChange a base type"
  in searchType gamma (chTypeImpl c t)

searchType :: Changes -> Type -> Type
searchType gamma (ArrowType ins out)
  = ArrowType (map (\(Tuple x t) -> (Tuple x (searchType gamma t))) ins) (searchBaseType gamma out)
searchType gamma (BaseType bt) = BaseType (searchBaseType gamma bt)

searchBaseType :: Changes -> BaseType -> BaseType
searchBaseType gamma (DataType x) = case lookup x (snd gamma) of
  Nothing -> DataType x
  Just dc -> case dc of
    DataTypeDeletion -> HoleType (freshHoleId unit) Nil
searchBaseType gamma (HoleType sym syms)
  = HoleType sym (filter (deleted gamma) syms) -- remove deleted datatypes from list of weakenings

deleted :: Changes -> TypeId -> Boolean
deleted (Tuple _ tc) x = case lookup x tc of
  Just DataTypeDeletion -> true 
  _ -> false

chArgs :: Changes -> (List InputChange) -> (List Term) -> (List Term)
chArgs gamma c args = map mapper c
  where mapper :: InputChange -> Term
        mapper (Change tc n) = searchTerm gamma $ chTerm gamma tc (args !! n)
        mapper (Insert t) = newTerm t

searchTerm = undefined
chTerm = undefined
newTerm = undefined
{-

newTerm :: Type -> Term
newTerm (ArrowType args out)
  = LambdaTerm (map (\_ -> newSymbol ()) args) (Block [] [] HoleTerm)
newTerm (BaseType bt) = NeutralTerm HoleTerm

searchArgs :: Changes -> [Term] -> [Term]
searchArgs gamma = map (searchTerm gamma)

chTerm :: Changes -> TypeChange -> Term -> Term
-- TODO: need to thread context and type through all these function in order to deal with this undefined.
chTerm gamma (TypeReplace ty) t = NeutralTerm HoleTerm
chTerm gamma NoChange t = searchTerm gamma t
chTerm gamma (ArrowChange ics tc) (LambdaTerm syms bl)
  = LambdaTerm (map symMapper ics)
    (chBlock (fst gamma `union` newChanges, snd gamma) tc bl)
  where symMapper :: InputChange -> Id
        symMapper (Change tc' n) = syms !! n
        symMapper (Insert t) = newSymbol () -- In order for changes to be in gamma, Insert will need info about the new type.
        changeMapper :: InputChange -> Map Id VarChange
        changeMapper (Change tc' n) = singleton (syms !! n) VariableDeletion
        changeMapper (Insert t) = mempty
        newChanges :: Map Id VarChange
        newChanges = unions (map changeMapper ics)
chTerm gamma (ArrowChange ics tc) _ = error "only a lambda should be an arrow type"

searchTerm :: Changes -> Term -> Term
searchTerm gamma (LambdaTerm syms bl) = LambdaTerm syms (searchBlock gamma bl)
searchTerm gamma (NeutralTerm t) = NeutralTerm (searchNeutral gamma t)

listAcc :: [Either a [a]] -> [a]
listAcc [] = []
listAcc ((Left a) : es) = a : listAcc es
listAcc ((Right as) : es) = as ++ listAcc es

-- Returns either new term or the term was deleted
searchNeutral :: Changes -> NeutralTerm -> NeutralTerm
searchNeutral gamma (Neutral x args) = case lookup x (fst gamma) of
  Nothing -> Neutral x (searchArgs gamma args)
  Just ch -> case ch of
    VariableDeletion -> HoleTerm
    VariableTypeChange tc -> case tc of
      NoChange -> Neutral x (searchArgs gamma args)
      (TypeReplace ty) -> case ty of
        (ArrowType ins out) -> Neutral x (map (newTerm . snd) ins)
        (BaseType bt) -> HoleTerm
      (ArrowChange ics outc) ->
        let newArgs = map (\case
              Change c n -> chTerm gamma c (args !! n)
              (Insert t) -> newTerm t) ics
        in case outc of
          TypeReplace ty -> Neutral x newArgs
          _ -> HoleTerm
searchNeutral gamma HoleTerm = HoleTerm

-- chNeutral gamma (MatchTerm sym te cas) = _wF gamma
-- TODO: create BaseTypeChange which is what applies to Neutrals as they have BaseType.
chNeutral :: Changes -> TypeChange -> NeutralTerm -> NeutralTerm
chNeutral gamma (TypeReplace ty) (Neutral sym tes) = HoleTerm
chNeutral gamma NoChange (Neutral sym tes) = searchNeutral gamma (Neutral sym tes)
chNeutral gamma (ArrowChange ics tc) (Neutral sym tes) = error "shouldn't happen, neutral is only of base type"
chNeutral gamma c HoleTerm = HoleTerm

chBlock :: Changes -> TypeChange -> Block -> Block
chBlock gamma c (Block defs buffer t)
  = Block (map (searchDefinition gamma) defs)
      (map (searchNeutral gamma) buffer) (chNeutral gamma c t)

searchBlock :: Changes -> Block -> Block
searchBlock gamma (Block defs buffer t)
  = Block (map (searchDefinition gamma) defs)
    (map (searchNeutral gamma) buffer) (searchNeutral gamma t)

searchDefinition :: Changes -> Definition -> Definition
searchDefinition gamma (TermDefinition x ty t)
  = TermDefinition x (searchType gamma ty) (searchTerm gamma t)
searchDefinition gamma (DataDefinition name typeId indId ctrs)
  = DataDefinition name typeId indId (map (searchConstructor gamma) ctrs)

searchConstructor :: Changes -> Constructor -> Constructor
searchConstructor gamma (Constructor x args)
  = Constructor x (map (\(x,t) -> (x, searchType gamma t)) args)

-- TODO: why am I calling it gamma?
-- TODO: how does moving stuff from block into hole work?
-- The thing in the block is a Term, but the hole expects a Neutral?
-- I suppose that it simply checks if it is a neutral and if not doesn't let you
-- move it?
-}