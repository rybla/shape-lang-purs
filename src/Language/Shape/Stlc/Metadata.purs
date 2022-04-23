module Language.Shape.Stlc.Metadata where

import Prelude

-- | Type Metadata
type ArrowMetadata
  = {}

type BaseMetadata
  = {}

type HoleTypeMetadata
  = {}

-- | Term Metadata
type LamMetadata
  = { indentBody :: Boolean }

type AppMetadata
  = { indentArg :: Boolean }

type VarMetadata
  = {}

type LetMetadata
  = { indentArg :: Boolean, indentBody :: Boolean }

type BufMetadata
  = {}

type DataMetadata
  = { indentSum :: Boolean }

type MatchMetadata
  = { indentCases :: Boolean }

type HoleTermMetadata
  = {}

-- | Sum and Prod Metadata
type SumMetadata
  = { indentSum :: Boolean }

type ProdMetadata
  = { indentProd :: Boolean }

-- Case Sum and Prod Metadata
type CaseSumMetadata
  = { indentProd :: Boolean, indentSum :: Boolean }

type CaseProdMetadata
  = { indentProd :: Boolean }
