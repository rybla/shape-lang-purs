module Language.Shape.Stlc.Index where

data ModuleIndex
  = ModuleIndex_Here
  | ModuleIndex_Definition Int DefinitionIndex

data BlockIndex
  = BlockIndex_Here
  | BlockIndex_Definition Int DefinitionIndex
  | BlockIndex_Term TermIndex

data DefinitionIndex
  = DefinitionIndex_Here
  | TermDefinitionIndex_TermBinding
  | TermDefinitionIndex_Type TypeIndex
  | TermDefinitionIndex_Term TermIndex
  | DataDefinitionIndex_TypeBinding
  | DataDefinitionIndex_Constructor Int ConstructorIndex

data ConstructorIndex
  = ConstructorIndex_TermBinding
  | ConstructorIndex_Parameter Int ParameterIndex

data TermIndex
  = TermIndex_Here
  | LambdaTermIndex_TermId
  | LambdaTermIndex_Block BlockIndex
  | HoleTermIndex
  | MatchTermIndex_TypeId
  | MatchTermIndex_Term TermIndex
  | MatchTermIndex_Case Int CaseIndex
  | NeutralTermIndex_TermId
  | NeutralTermIndex_Args ArgsIndex

data ArgsIndex
  = NoneArgsIndex
  | ConsArgsIndex_Term TermIndex
  | ConsArgsIndex_Args ArgsIndex

data CaseIndex
  = CaseIndex_Here
  | CaseIndex_TermId Int
  | CaseIndex_Term TermIndex

data TypeIndex
  = TypeIndex_Here
  | ArrowTypeIndex_Parameter ParameterIndex
  | ArrowTypeIndex_Type TypeIndex
  | DataTypeIndex_Here
  | HoleTypeIndex_Here
  | ProxyHoleTypeIndex_Here

data ParameterIndex
  = ParameterIndex_Here
  | ParameterIndex_Type TypeIndex
