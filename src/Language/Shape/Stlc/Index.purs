module Language.Shape.Stlc.Index where

import Prelude

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
  | MatchTerm_TypeId
  | MatchTerm_Term
  | MatchTerm_Case Int
  | NeutralTerm_TermId
  | NeutralTerm_Args
  | NoneArgs
  | ConsArgs_Term
  | ConsArgs_Args
  | Case_TermId
  | Case_Term
  | ArrowType_Parameter
  | ArrowType_Type
  | Parameter_Type
  | Here

pushIndex :: Index -> IndexStep -> Index
pushIndex ix step = ix <> [ step ]
