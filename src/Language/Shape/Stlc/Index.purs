module Language.Shape.Stlc.Index where

import Prelude

import Data.Array as Array
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)

type Index
  = Array IndexStep

data IndexStep
  = Module_Definition Int
  | Block_Definition Int
  | Block_Term
  | TermDefinition_TermBinding
  | TermDefinition_Type
  | TermDefinition_Term
  | DataDefinition_TypeBinding
  | DataDefinition_Constructor Int
  | Constructor_TermBinding
  | Constructor_Parameter Int
  | LambdaTerm_TermId
  | LambdaTerm_Block
  | HoleTerm
  | MatchTerm_Term
  | MatchTerm_Case Int
  | NeutralTerm_TermId
  | NeutralTerm_Args
  | NoneArgs
  | ConsArgs_Term
  | ConsArgs_Args
  | Case_TermId Int
  | Case_Term
  | ArrowType_Parameter
  | ArrowType_Type
  | Parameter_Type

derive instance Generic IndexStep _
instance Eq IndexStep where eq step step' = genericEq step step' 

pushIndex :: Index -> IndexStep -> Index
pushIndex = Array.snoc

infix 5 pushIndex as :>
