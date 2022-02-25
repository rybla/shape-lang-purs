module Language.Shape.Stlc.Model where

import Prim hiding (Type)
import Prelude
import Data.Map (Map, lookup)
-- import Data.Symbol
import Data.Tuple
import Data.List
import Data.Map
import Undefined
import Data.Maybe
import Language.Shape.Stlc.Syntax

-- type Permutation = List Int

-- -- Change to an argument list
-- data TypeChange =
--     Output TypeChange |
--     TypeReplace Type |
--     InputChange InputChange

-- data InputChange = Input Int TypeChange | Insert Int Parameter | Delete Int | Permute Permutation

-- data DataConstructorChange = ConstructorPermute Permutation
--     | DeleteConstructor Int | NewConstructor Int Constructor

-- -- data Change = VariableTypeChange TypeChange | VariableDeletion | DataTypeDeletion
--     -- | DataTypeChange DataChange -- figure out
-- data VarChange = VariableTypeChange TypeChange | VariableDeletion
-- data DataChange = DataTypeDeletion | DataTypeChange DataChange

-- type Changes = Tuple (Map Binding VarChange) (Map Binding DataChange)

-- -- -- Why arent these basic function in haskell's standard library?
-- -- deleteAt :: List a -> Int -> List a
-- -- deleteAt xs i = take i xs `concat` drop (i + 1) xs

-- -- insertAt :: List a -> Int -> a -> List a
-- -- insertAt xs i x = take i xs `concat` [x] `concat` drop i xs

-- -- modifyAt :: List a -> Int -> (a -> a) -> List a
-- -- modifyAt xs i f = take i xs `concat` [f (xs !! i)] `concat` drop (i + 1) xs

-- modifyAt' :: forall a. Partial => Int -> (a -> a) -> List a -> List a
-- modifyAt' i f xs = fromJust (modifyAt i f xs)

-- insertAt' :: forall a. Partial => Int -> a -> List a -> List a
-- insertAt' i x xs = fromJust (insertAt i x xs)

-- deleteAt' :: forall a. Partial => Int -> List a -> List a
-- deleteAt' i xs = fromJust (deleteAt i xs)

-- chType :: Partial => Type -> TypeChange -> Type
-- chType (ArrowType params out) (InputChange (Input i c))
--     = ArrowType (modifyAt' i (\(Tuple name t) -> Tuple name (chType t c)) params) out
-- chType (ArrowType params out) (Output (TypeReplace (BaseType b))) = ArrowType params b -- Should only TypeChange output to a base type!
-- chType (ArrowType params out) (InputChange (Insert i t)) = ArrowType (insertAt' i t params) out
-- chType (ArrowType params out) (InputChange (Delete i)) = ArrowType (deleteAt' i params) out
-- chType (ArrowType params out) (InputChange (Permute p)) = undefined
-- chType _ (TypeReplace t) = t
-- chType _ _ = undefined -- "shouldn't get here"

-- -- convert arguments to a function of type T into arguments to a function of type (chType T change)
-- chArgs :: Partial => List Term -> Changes -> InputChange -> List Term
-- chArgs args gamma (Insert i p) =
--   insertAt' 
--     i
--     (HoleTerm
--       undefined -- (newSymbol ())
--       undefined
--       undefined)
--     (searchArgs gamma args)
-- chArgs args gamma (Input i c) = searchArgs gamma (modifyAt' i (\t -> chTerm t gamma c) args)
-- chArgs args gamma (Delete i) = searchArgs gamma (deleteAt' i args)
-- chArgs args gamma (Permute p) = undefined

-- searchArgs :: Partial => Changes -> List Term -> List Term
-- searchArgs gamma = map (searchTerm gamma)

-- -- Convert a term of type T into a term of type (chType T change)
-- chTerm :: Term -> Changes -> TypeChange -> Term
-- chTerm = undefined
-- -- chTerm is what will actually introduce stuff into Changes

-- searchTerm :: Partial => Changes -> Term -> Term
-- searchTerm gamma (LambdaTerm binds block) = undefined
-- searchTerm gamma (NeutralTerm x args) = case lookup x (fst gamma) of
--   Nothing -> NeutralTerm x (searchArgs gamma args)
--   Just ch -> case ch of
--     (VariableTypeChange tc) -> case tc of
--       (Output tc') -> freshHole unit -- Really, old args should go in hole?
--       (TypeReplace ty) -> freshHole unit  -- Really, old args should go in hole?
--       (InputChange ic) -> NeutralTerm x (chArgs args gamma ic)
--     VariableDeletion -> freshHole unit
-- searchTerm gamma (MatchTerm ty t cases) = case lookup ty (snd gamma) of
--   Nothing -> undefined -- use searchCases on cases and also searchTerm on t
--   Just dc -> case dc of
--     DataTypeDeletion -> freshHole unit
--     (DataTypeChange dc') -> undefined -- call chCases?
-- searchTerm gamma (HoleTerm h _ _) = undefined -- should gamma also have hole substitutions?

-- -- TODO: reorder args of functions to make mapping easier
-- -- TODO: Holes need to have values in them sometimes, e.g. when output is changed
-- --      Whatever we do with buffers/holes/whatever, this should be handled by the model.
-- --      This is because stuff in a buffer needs to be updated when an argument is added to a 
-- --      function or whatever.

-- {-

-- Problem: suppose that we have a program like

-- Data A
-- let g : A -> B
-- let f : B
--     f = g a

-- Then the user deltes the type A.

-- The algorithm will discover that g has a VarChange which is (Input 0 (TypeReplace (Hole ...)))
-- So everywhere that g is called, this change will be in gamma, and it will cause the first arg
-- of g to be dug.
-- BUT then this needs to be applied inside the body of f.
-- Even worse, g and f could have been defined in the opposite order, so g could be above f.

-- searchBlock will have to be designed to deal with this case. It will need to first find all
-- of the changes on each declaration, and only afterwards call searchTerm on the body of each
-- declaration with those changes in gamma.

-- -}