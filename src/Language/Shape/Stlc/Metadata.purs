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

derive instance newTypeArrowTypeMetadata :: Newtype ArrowTypeMetadata _

derive newtype instance showArrowTypeMetadata :: Show ArrowTypeMetadata

newtype DataTypeMetadata
  = DataTypeMetadata {}

instance defaultDataTypeMetadata :: Default DataTypeMetadata where
  default = DataTypeMetadata {}

derive instance newTypeDataTypeMetadata :: Newtype DataTypeMetadata _

derive newtype instance showDataTypeMetadata :: Show DataTypeMetadata

newtype HoleTypeMetadata
  = HoleTypeMetadata {}

instance defaultHoleTypeMetadata :: Default HoleTypeMetadata where
  default = HoleTypeMetadata {}

derive instance newTypeHoleTypeMetadata :: Newtype HoleTypeMetadata _

derive newtype instance showHoleTypeMetadata :: Show HoleTypeMetadata

-- | Term Metadata
newtype LamMetadata
  = LamMetadata { name :: Name, indentBody :: Boolean }

instance defaultLamMetadata :: Default LamMetadata where
  default = LamMetadata { name: default, indentBody: false }

derive instance newTypeLamMetadata :: Newtype LamMetadata _

derive newtype instance showLamMetadata :: Show LamMetadata

newtype NeuMetadata
  = NeuMetadata {}

instance defaultNeuMetadata :: Default NeuMetadata where
  default = NeuMetadata {}

derive instance newTypeNeuMetadata :: Newtype NeuMetadata _

derive newtype instance showNeuMetadata :: Show NeuMetadata

newtype ArgItemMetadata
  = ArgItemMetadata {}

instance defaultArgItemMetadata :: Default ArgItemMetadata where
  default = ArgItemMetadata {}

derive instance newTypeArgItemMetadata :: Newtype ArgItemMetadata _

derive newtype instance showArgItemMetadata :: Show ArgItemMetadata

newtype ArgMetadata
  = ArgMetadata { indented :: Boolean }

instance defaultArgMetadata :: Default ArgMetadata where
  default = ArgMetadata { indented: false }

derive instance newTypeArgMetadata :: Newtype ArgMetadata _

derive newtype instance showArgMetadata :: Show ArgMetadata

newtype LetMetadata
  = LetMetadata { name :: Name, indentArg :: Boolean, indentBody :: Boolean }

instance defaultLetMetadata :: Default LetMetadata where
  default = LetMetadata { name: default, indentArg: false, indentBody: true }

derive instance newTypeLetMetadata :: Newtype LetMetadata _

derive newtype instance showLetMetadata :: Show LetMetadata

newtype BufMetadata
  = BufMetadata {}

instance defaultBufMetadata :: Default BufMetadata where
  default = BufMetadata {}

derive instance newTypeBufMetadata :: Newtype BufMetadata _

derive newtype instance showBufMetadata :: Show BufMetadata

newtype DataMetadata
  = DataMetadata { name :: Name, indentSum :: Boolean }

instance defaultDataMetadata :: Default DataMetadata where
  default = DataMetadata { name: default, indentSum: false }

derive instance newTypeDataMetadata :: Newtype DataMetadata _

derive newtype instance showDataMetadata :: Show DataMetadata

newtype MatchMetadata
  = MatchMetadata { indentCases :: Boolean }

instance defaultMatchMetadata :: Default MatchMetadata where
  default = MatchMetadata { indentCases: true }

derive instance newTypeMatchMetadata :: Newtype MatchMetadata _

derive newtype instance showMatchMetadata :: Show MatchMetadata

newtype HoleMetadata
  = HoleMetadata {}

instance defaultHoleMetadata :: Default HoleMetadata where
  default = HoleMetadata {}

derive instance newTypeHoleMetadata :: Newtype HoleMetadata _

derive newtype instance showHoleMetadata :: Show HoleMetadata

newtype TypeBindMetadata
  = TypeBindMetadata {}

instance defaultTypeBindMetadata :: Default TypeBindMetadata where
  default = TypeBindMetadata {}

derive instance newTypeTypeBindMetadata :: Newtype TypeBindMetadata _

derive newtype instance showTypeBindMetadata :: Show TypeBindMetadata

newtype TermBindMetadata
  = TermBindMetadata {}

instance defaultTermBindMetadata :: Default TermBindMetadata where
  default = TermBindMetadata {}

derive instance newTypeTermBindMetadata :: Newtype TermBindMetadata _

derive newtype instance showTermBindMetadata :: Show TermBindMetadata

newtype SumItemMetadata
  = SumItemMetadata {}

instance defaultSumItemMetadata :: Default SumItemMetadata where
  default = SumItemMetadata {}

derive instance newTypeSumItemMetadata :: Newtype SumItemMetadata _

derive newtype instance showSumItemMetadata :: Show SumItemMetadata

newtype CaseItemMetadata
  = CaseItemMetadata {}

instance defaultCaseItemMetadata :: Default CaseItemMetadata where
  default = CaseItemMetadata {}

derive instance newTypeCaseItemMetadata :: Newtype CaseItemMetadata _

derive newtype instance showCaseItemMetadata :: Show CaseItemMetadata

newtype ParamItemMetadata
  = ParamItemMetadata {}

instance defaultParamItemMetadata :: Default ParamItemMetadata where
  default = ParamItemMetadata {}

derive instance newTypeParamItemMetadata :: Newtype ParamItemMetadata _

derive newtype instance showParamItemMetadata :: Show ParamItemMetadata

newtype TermBindItemMetadata
  = TermBindItemMetadata {}

instance defaultTermBindItemMetadata :: Default TermBindItemMetadata where
  default = TermBindItemMetadata {}

derive instance newTypeTermBindItemMetadata :: Newtype TermBindItemMetadata _

derive newtype instance showTermBindItemMetadata :: Show TermBindItemMetadata

-- | Name 
newtype Name
  = Name (Maybe String)

derive instance newTypeName :: Newtype Name _

derive newtype instance eqName :: Eq Name

derive newtype instance ordName :: Ord Name

derive newtype instance showName :: Show Name

instance defaultName :: Default Name where
  default = Name Nothing

-- -- | TypeName 
-- newtype TypeName
--   = TypeName (Maybe String)
-- derive instance newTypeTypeName :: Newtype TypeName _
-- derive newtype instance eqTypeName :: Eq TypeName
-- derive newtype instance showTypeName :: Show TypeName
-- instance defaultTypeName :: Default TypeName where
--   default = TypeName Nothing
-- -- | TermName 
-- newtype TermName
--   = TermName (Maybe String)
-- derive instance newTypeTermName :: Newtype TermName _
-- derive newtype instance eqTermName :: Eq TermName
-- derive newtype instance showTermName :: Show TermName
-- instance defaultTermName :: Default TermName where
--   default = TermName Nothing
