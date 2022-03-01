module Language.Shape.Stlc.Model where

import Data.Maybe
import Data.Symbol
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Data.Either (Either(..))
import Data.List (List(..), (!!), filter)
import Data.List.Lazy as List
import Data.Map (Map, empty, lookup, unionWith, unions, singleton)
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
-- Example:  (A,B,C)->D changes to (A,C,?)->D
-- is: ArrowChange [Change NoChange 0, Change NoChange 2, Insert] NoChange
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
      chTypeImpl (ArrowChange ics out) (ArrowType as b md)
        = ArrowType (map mapper ics) bNew md
        where bNew = case out of
                TypeReplace (BaseType t) -> t
                NoChange -> b
                _ -> error "shouldn't happen"
              mapper :: InputChange -> Parameter
            --   mapper (Change tc n) = Parameter (fst $ as !!! n) (chTypeImpl tc (snd $ as !!! n))
              mapper (Change tc n) = case as !!! n of
                (Parameter name tp mdp) -> Parameter name (chTypeImpl tc tp) mdp
              mapper (Insert newT) = makeParameter (TermName "_") newT
      chTypeImpl (ArrowChange _ _) (BaseType bt) = error "Can't ArrowChange a base type"
  in searchType gamma (chTypeImpl c t)
searchType :: Changes -> Type -> Type
searchType gamma (ArrowType ins out md)
  = ArrowType (map (case _ of (Parameter x t md) -> (Parameter x (searchType gamma t) md)) ins)
    (searchBaseType gamma out) md
searchType gamma (BaseType bt) = BaseType (searchBaseType gamma bt)
searchBaseType :: Changes -> BaseType -> BaseType
searchBaseType gamma (DataType x md@{indented : i}) = case lookup x (snd gamma) of
  Nothing -> DataType x md
  Just dc -> case dc of
    DataTypeDeletion -> HoleType (freshHoleId unit) Nil {indented: i, cursor : false}
searchBaseType gamma (HoleType sym syms md)
  = HoleType sym (filter (deleted gamma) syms) md -- remove deleted datatypes from list of weakenings
deleted :: Changes -> TypeId -> Boolean
deleted (Tuple _ tc) x = case lookup x tc of
  Just DataTypeDeletion -> true 
  _ -> false
chArgs :: Changes -> (List InputChange) -> (List Term) -> (List Term)
chArgs gamma c args = map mapper c
  where mapper :: InputChange -> Term
        mapper (Change tc n) = searchTerm gamma $ chTerm gamma tc (args !!! n)
        mapper (Insert t) = newTerm t
newTerm :: Type -> Term
newTerm (ArrowType args out md)
  = makeLambdaTerm (map (\_ -> makeTermBinding (freshTermId unit)) args) (makeBlock Nil Nil makeHoleTerm)
newTerm (BaseType bt) = makeNeutralTerm makeHoleTerm
searchArgs :: Changes -> (List Term) -> (List Term)
searchArgs gamma = map (searchTerm gamma)
chTerm :: Changes -> TypeChange -> Term -> Term
-- TODO: need to thread context and type through all these function in order to deal with this undefined.
chTerm gamma (TypeReplace ty) t = makeNeutralTerm makeHoleTerm
chTerm gamma NoChange t = searchTerm gamma t
chTerm gamma (ArrowChange ics tc) (LambdaTerm syms bl md)
  = LambdaTerm (map symMapper ics)
    (chBlock (Tuple (fst gamma `unionWith (\v1 v2 -> error "no")` newChanges) (snd gamma)) tc bl)
    md
  where symMapper :: InputChange -> TermBinding
        symMapper (Change tc' n) = syms !!! n
        symMapper (Insert t) = makeTermBinding (freshTermId unit) -- In order for changes to be in gamma, Insert will need info about the new type.
        changeMapper :: InputChange -> Map TermId VarChange
        changeMapper (Change tc' n)
            = case (syms !!! n) of (TermBinding x _) -> singleton x VariableDeletion
        changeMapper (Insert t) = empty
        newChanges :: Map TermId VarChange
        newChanges = unions (map changeMapper ics)
chTerm gamma (ArrowChange ics tc) _ = error "only a lambda should be an arrow type"
searchTerm :: Changes -> Term -> Term
searchTerm gamma (LambdaTerm syms bl md) = LambdaTerm syms (searchBlock gamma bl) md
searchTerm gamma (NeutralTerm t) = NeutralTerm (searchNeutral gamma t)
searchNeutral :: Changes -> NeutralTerm -> NeutralTerm
searchNeutral gamma (ApplicationTerm r@(TermReference x _) args md) = case lookup x (fst gamma) of
  Nothing -> ApplicationTerm r (searchArgs gamma args) md
  Just ch -> case ch of
    VariableDeletion -> makeHoleTerm
    VariableTypeChange tc -> case tc of
      NoChange -> ApplicationTerm r (searchArgs gamma args) md
      (TypeReplace ty) -> case ty of
        (ArrowType ins out _) -> ApplicationTerm r (map (case _ of (Parameter _ t _) -> newTerm t) ins) md
        (BaseType bt) -> makeHoleTerm
      (ArrowChange ics outc) ->
        let newArgs = map (case _ of
              Change c n -> chTerm gamma c (args !!! n)
              (Insert t) -> newTerm t) ics
        in case outc of
          TypeReplace ty -> ApplicationTerm r newArgs md
          _ -> makeHoleTerm
searchNeutral gamma (HoleTerm md) = HoleTerm md
searchNeutral gamma (MatchTerm _ _ _ md) = undefined -- TODO
-- TODO: create BaseTypeChange which is what applies to Neutrals as they have BaseType.
chNeutral :: Changes -> TypeChange -> NeutralTerm -> NeutralTerm
chNeutral gamma (TypeReplace ty) (ApplicationTerm sym tes md) = makeHoleTerm -- TODO: incorporate indentation from input input output
chNeutral gamma NoChange (ApplicationTerm sym tes md) = searchNeutral gamma (ApplicationTerm sym tes md)
chNeutral gamma (ArrowChange ics tc) (ApplicationTerm sym tes md) = error "shouldn't happen, neutral is only of base type"
chNeutral gamma c (HoleTerm md) = HoleTerm md
chNeutral gamma c (MatchTerm sym te cas md) = undefined
chBlock :: Changes -> TypeChange -> Block -> Block
chBlock gamma c (Block defs buffer t md)
  = Block (map (searchDefinition gamma) defs)
      (map (searchNeutral gamma) buffer) (chNeutral gamma c t) md
searchBlock :: Changes -> Block -> Block
searchBlock gamma (Block defs buffer t md)
  = Block (map (searchDefinition gamma) defs)
    (map (searchNeutral gamma) buffer) (searchNeutral gamma t) md
searchDefinition :: Changes -> Definition -> Definition
searchDefinition gamma (TermDefinition b ty t md)
  = TermDefinition b (searchType gamma ty) (searchTerm gamma t) md
searchDefinition gamma (DataDefinition b ctrs md)
  = DataDefinition b (map (searchConstructor gamma) ctrs) md
searchConstructor :: Changes -> Constructor -> Constructor
searchConstructor gamma (Constructor b args md)
  = Constructor b (map (\(Parameter x t pmd) -> (Parameter x (searchType gamma t) pmd)) args) md
-- TODO: searchConstructor needs to actually apply constructor changes once I define them!

  
-- {-
-- -- TODO: why am I calling it gamma?
-- -- TODO: how does moving stuff from block into hole work?
-- -- The thing in the block is a Term, but the hole expects a Neutral?
-- -- I suppose that it simply checks if it is a neutral and if not doesn't let you
-- -- move it?
-- -}
