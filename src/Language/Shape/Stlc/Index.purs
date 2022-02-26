module Language.Shape.Stlc.Index where

import Data.List
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Data.Array (index)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple)
import Undefined (undefined)

type Index
  = List IndexStep

data IndexStep
  = IndexStep_Block
  | IndexStep_Definition Int
  | IndexStep_Buffer Int
  | IndexStep_Type
  | IndexStep_Term
  | IndexStep_Constructor Int
  | IndexStep_Parameter Int
  | IndexStep_Output
  | IndexStep_Argument Int
  | IndexStep_Case Int
  | IndexStep_UniqueTermBinding
  | IndexStep_TermBinding Int
  | IndexStep_TermReference
  | IndexStep_TermLabel
  | IndexStep_UniqueTypeBinding
  | IndexStep_TypeReference

data IndexResult
  = IndexResult_Module Module
  | IndexResult_Block Block
  | IndexResult_Definition Definition
  | IndexResult_Constructor Constructor
  | IndexResult_Buffer NeutralTerm
  | IndexResult_Type Type
  | IndexResult_Term Term
  | IndexResult_Parameter TermName Type
  | IndexResult_UniqueTermBinding TermName TermId
  | IndexResult_TermBinding TermId
  | IndexResult_TermLabel TermName
  | IndexResult_UniqueTypeBinding TypeName TypeId
  | IndexResult_TypeReference TypeId

pushIndex :: Index -> IndexStep -> Index
pushIndex ix istep = append ix (singleton istep)

appendIndex :: Index -> List IndexStep -> Index
appendIndex ix isteps = append ix isteps

-- indexModule :: Partial => Module -> Index -> IndexResult
-- indexModule module_@(Module definitions) ix =
--   case head ix of 
--     Nothing -> IndexResult_Module module_
--     Just (IndexStep_Definition i) -> indexDefinition (fromJust (index definitions i)) (fromJust (tail ix))
-- indexDefinition :: Partial => Definition -> Index -> IndexResult
-- indexDefinition definition ix = 
--   case head ix of 
--     Nothing -> IndexResult_Definition definition
--     Just istep ->
--       case definition of
--         TermDefinition uniqueTermBinding type_ term -> 
--           case istep of
--             IndexStep_UniqueTermBinding -> IndexResult_UniqueTermBinding uniqueTermBinding
--             IndexStep_Type -> indexType type_ (fromJust (tail ix))
--             IndexStep_Term -> indexTerm term (fromJust (tail ix))
--         DataDefinition uniqueTypeBinding constructors -> undefined -- TODO
-- indexType :: Partial => Type -> Index -> IndexResult
-- indexType = undefined
-- indexTerm :: Partial => Term -> Index -> IndexResult
-- indexTerm = undefined
