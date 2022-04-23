module Language.Shape.Stlc.Metadata where

import Data.Tuple
import Data.Tuple.Nested
import Prelude
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

-- | Type Metadata
type ArrowTypeMetadata
  = {}

type DataTypeMetadata
  = {}

type HoleTypeMetadata
  = {}

-- | Term Metadata
type LamMetadata
  = { name :: Name, indentBody :: Boolean }

type AppMetadata
  = { indentArg :: Boolean }

type VarMetadata
  = {}

type LetMetadata
  = { indentArg :: Boolean, indentBody :: Boolean }

type BufMetadata
  = {}

type DataMetadata
  = { name :: Name, indentSum :: Boolean }

type MatchMetadata
  = { indentCases :: Boolean }

type HoleMetadata
  = {}

-- | Sum and Prod Metadata
type ZeroMetadata
  = {}

type AddMetadata
  = { indentSum :: Boolean }

type OneMetadata
  = {}

type MulMetadata
  = { indentProd :: Boolean }

-- Case Sum and Prod Metadata
type ZeroCaseMetadata
  = {}

type AddCaseMetadata
  = { indentProd :: Boolean, indentSum :: Boolean }

type OneCaseMetadata
  = {}

type MulCaseMetadata
  = { indentProd :: Boolean }

-- | Name 
newtype Name
  = Name (Maybe String)

derive instance newTypeName :: Newtype Name _

derive newtype instance eqName :: Eq Name
