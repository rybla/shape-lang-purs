module Language.Shape.Stlc.Metadata where

import Data.Tuple
import Data.Tuple.Nested
import Prelude
import Data.Default (class Default, default)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)

-- | Type Metadata
newtype ArrowTypeMetadata
  = ArrowTypeMetadata {}

instance defaultArrowTypeMetadata :: Default ArrowTypeMetadata where
  default = ArrowTypeMetadata {}

derive newtype instance showArrowTypeMetadata :: Show ArrowTypeMetadata

newtype DataTypeMetadata
  = DataTypeMetadata {}

instance defaultDataTypeMetadata :: Default DataTypeMetadata where
  default = DataTypeMetadata {}

derive newtype instance showDataTypeMetadata :: Show DataTypeMetadata

newtype HoleTypeMetadata
  = HoleTypeMetadata {}

instance defaultHoleTypeMetadata :: Default HoleTypeMetadata where
  default = HoleTypeMetadata {}

derive newtype instance showHoleTypeMetadata :: Show HoleTypeMetadata

-- | Term Metadata
newtype LamMetadata
  = LamMetadata { name :: Name, indentBody :: Boolean }

instance defaultLamMetadata :: Default LamMetadata where
  default = LamMetadata { name: default, indentBody: false }

derive newtype instance showLamMetadata :: Show LamMetadata

newtype NeuMetadata
  = NeuMetadata {}

derive newtype instance showNeuMetadata :: Show NeuMetadata

instance defaultNeuMetadata :: Default NeuMetadata where
  default = NeuMetadata {}

newtype ArgMetadata
  = ArgMetadata {}

derive newtype instance showArgMetadata :: Show ArgMetadata

instance defaultArgMetadata :: Default ArgMetadata where
  default = ArgMetadata {}

newtype LetMetadata
  = LetMetadata { indentArg :: Boolean, indentBody :: Boolean }

instance defaultLetMetadata :: Default LetMetadata where
  default = LetMetadata { indentArg: false, indentBody: true }

derive newtype instance showLetMetadata :: Show LetMetadata

newtype BufMetadata
  = BufMetadata {}

instance defaultBufMetadata :: Default BufMetadata where
  default = BufMetadata {}

derive newtype instance showBufMetadata :: Show BufMetadata

newtype DataMetadata
  = DataMetadata { name :: Name, indentSum :: Boolean }

instance defaultDataMetadata :: Default DataMetadata where
  default = DataMetadata { name: default, indentSum: false }

derive newtype instance showDataMetadata :: Show DataMetadata

newtype MatchMetadata
  = MatchMetadata { indentCases :: Boolean }

instance defaultMatchMetadata :: Default MatchMetadata where
  default = MatchMetadata { indentCases: true }

derive newtype instance showMatchMetadata :: Show MatchMetadata

newtype HoleMetadata
  = HoleMetadata {}

instance defaultHoleMetadata :: Default HoleMetadata where
  default = HoleMetadata {}

derive newtype instance showHoleMetadata :: Show HoleMetadata

-- -- | Sum and Prod Metadata
-- type ZeroMetadata
--   = {}
-- type AddMetadata
--   = { indentSum :: Boolean }
-- type OneMetadata
--   = {}
-- type MulMetadata
--   = { indentProd :: Boolean }
-- -- Case Sum and Prod Metadata
-- type ZeroCaseMetadata
--   = {}
-- type AddCaseMetadata
--   = { indentProd :: Boolean, indentSum :: Boolean }
-- type OneCaseMetadata
--   = {}
-- type MulCaseMetadata
--   = { indentProd :: Boolean }
-- | Name 
newtype Name
  = Name (Maybe String)

derive instance newTypeName :: Newtype Name _

derive newtype instance eqName :: Eq Name

derive newtype instance showName :: Show Name

instance defaultName :: Default Name where
  default = Name Nothing
