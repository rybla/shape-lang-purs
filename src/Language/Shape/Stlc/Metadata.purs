module Language.Shape.Stlc.Metadata where

import Data.Tuple
import Data.Tuple.Nested
import Prelude
import Data.Default (class Default, default)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as Variant
import Prim.Row (class Cons)
import Record as Record
import Type.Proxy (Proxy(..))
import Undefined (undefined)

-- | Type Metadata
newtype ArrowTypeMetadata
  = ArrowTypeMetadata {}

instance defaultArrowTypeMetadata :: Default ArrowTypeMetadata where
  default = ArrowTypeMetadata {}

derive instance newTypeArrowTypeMetadata :: Newtype ArrowTypeMetadata _

instance showArrowTypeMetadata :: Show ArrowTypeMetadata where
  show x = "ArrowTypeMetadata " <> show (unwrap x)

newtype DataTypeMetadata
  = DataTypeMetadata {}

instance defaultDataTypeMetadata :: Default DataTypeMetadata where
  default = DataTypeMetadata {}

derive instance newTypeDataTypeMetadata :: Newtype DataTypeMetadata _

instance showDataTypeMetadata :: Show DataTypeMetadata where
  show x = "DataTypeMetadata " <> show (unwrap x)

newtype HoleTypeMetadata
  = HoleTypeMetadata {}

instance defaultHoleTypeMetadata :: Default HoleTypeMetadata where
  default = HoleTypeMetadata {}

derive instance newTypeHoleTypeMetadata :: Newtype HoleTypeMetadata _

instance showHoleTypeMetadata :: Show HoleTypeMetadata where
  show x = "HoleTypeMetadata " <> show (unwrap x)

-- | Term Metadata
newtype LamMetadata
  = LamMetadata { name :: Name, indentBody :: Boolean }

instance defaultLamMetadata :: Default LamMetadata where
  default = LamMetadata { name: default, indentBody: false }

derive instance newTypeLamMetadata :: Newtype LamMetadata _

instance showLamMetadata :: Show LamMetadata where
  show x = "LamMetadata " <> show (unwrap x)

newtype NeuMetadata
  = NeuMetadata {}

instance defaultNeuMetadata :: Default NeuMetadata where
  default = NeuMetadata {}

derive instance newTypeNeuMetadata :: Newtype NeuMetadata _

instance showNeuMetadata :: Show NeuMetadata where
  show x = "NeuMetadata " <> show (unwrap x)

newtype ArgItemMetadata
  = ArgItemMetadata { indented :: Boolean }

instance defaultArgItemMetadata :: Default ArgItemMetadata where
  default = ArgItemMetadata { indented: false }

derive instance newTypeArgItemMetadata :: Newtype ArgItemMetadata _

instance showArgItemMetadata :: Show ArgItemMetadata where
  show x = "ArgItemMetadata " <> show (unwrap x)

newtype ArgMetadata
  = ArgMetadata { indented :: Boolean }

instance defaultArgMetadata :: Default ArgMetadata where
  default = ArgMetadata { indented: false }

derive instance newTypeArgMetadata :: Newtype ArgMetadata _

instance showArgMetadata :: Show ArgMetadata where
  show x = "ArgMetadata " <> show (unwrap x)

newtype LetMetadata
  = LetMetadata { name :: Name, indentSign :: Boolean, indentImpl :: Boolean, indentBody :: Boolean }

instance defaultLetMetadata :: Default LetMetadata where
  default = LetMetadata { name: default, indentSign: false, indentImpl: false, indentBody: false }

derive instance newTypeLetMetadata :: Newtype LetMetadata _

instance showLetMetadata :: Show LetMetadata where
  show x = "LetMetadata " <> show (unwrap x)

newtype BufMetadata
  = BufMetadata { indentSign :: Boolean, indentImpl :: Boolean, indentBody :: Boolean }

instance defaultBufMetadata :: Default BufMetadata where
  default = BufMetadata { indentSign: false, indentImpl: false, indentBody: false }

derive instance newTypeBufMetadata :: Newtype BufMetadata _

instance showBufMetadata :: Show BufMetadata where
  show x = "BufMetadata " <> show (unwrap x)

newtype DataMetadata
  = DataMetadata { name :: Name, indentSum :: Boolean, indentBody :: Boolean }

instance defaultDataMetadata :: Default DataMetadata where
  default = DataMetadata { name: default, indentSum: false, indentBody: false }

derive instance newTypeDataMetadata :: Newtype DataMetadata _

instance showDataMetadata :: Show DataMetadata where
  show x = "DataMetadata " <> show (unwrap x)

newtype MatchMetadata
  = MatchMetadata { indentCases :: Boolean }

instance defaultMatchMetadata :: Default MatchMetadata where
  default = MatchMetadata { indentCases: false }

derive instance newTypeMatchMetadata :: Newtype MatchMetadata _

instance showMatchMetadata :: Show MatchMetadata where
  show x = "MatchMetadata " <> show (unwrap x)

newtype HoleMetadata
  = HoleMetadata {}

instance defaultHoleMetadata :: Default HoleMetadata where
  default = HoleMetadata {}

derive instance newTypeHoleMetadata :: Newtype HoleMetadata _

instance showHoleMetadata :: Show HoleMetadata where
  show x = "HoleMetadata " <> show (unwrap x)

newtype TermMetadata
  = TermMetadata (Variant ( lam :: LamMetadata, neu :: NeuMetadata, let_ :: LetMetadata, buf :: BufMetadata, data_ :: DataMetadata, match :: MatchMetadata, hole :: HoleMetadata ))

derive instance newTypeTermmetadata :: Newtype TermMetadata _

newtype TypeBindMetadata
  = TypeBindMetadata { name :: Name }

instance defaultTypeBindMetadata :: Default TypeBindMetadata where
  default = TypeBindMetadata { name: default }

derive instance newTypeTypeBindMetadata :: Newtype TypeBindMetadata _

instance showTypeBindMetadata :: Show TypeBindMetadata where
  show x = "TypeBindMetadata " <> show (unwrap x)

newtype TermBindMetadata
  = TermBindMetadata { name :: Name }

instance defaultTermBindMetadata :: Default TermBindMetadata where
  default = TermBindMetadata { name: default }

derive instance newTypeTermBindMetadata :: Newtype TermBindMetadata _

instance showTermBindMetadata :: Show TermBindMetadata where
  show x = "TermBindMetadata " <> show (unwrap x)

newtype SumItemMetadata
  = SumItemMetadata { indented :: Boolean }

instance defaultSumItemMetadata :: Default SumItemMetadata where
  default = SumItemMetadata { indented: false }

derive instance newTypeSumItemMetadata :: Newtype SumItemMetadata _

instance showSumItemMetadata :: Show SumItemMetadata where
  show x = "SumItemMetadata " <> show (unwrap x)

newtype CaseItemMetadata
  = CaseItemMetadata { indented :: Boolean }

instance defaultCaseItemMetadata :: Default CaseItemMetadata where
  default = CaseItemMetadata { indented: false }

derive instance newTypeCaseItemMetadata :: Newtype CaseItemMetadata _

instance showCaseItemMetadata :: Show CaseItemMetadata where
  show x = "CaseItemMetadata " <> show (unwrap x)

newtype ParamItemMetadata
  = ParamItemMetadata { indented :: Boolean }

instance defaultParamItemMetadata :: Default ParamItemMetadata where
  default = ParamItemMetadata { indented: false }

derive instance newTypeParamItemMetadata :: Newtype ParamItemMetadata _

instance showParamItemMetadata :: Show ParamItemMetadata where
  show x = "ParamItemMetadata " <> show (unwrap x)

newtype TermBindItemMetadata
  = TermBindItemMetadata { indented :: Boolean }

instance defaultTermBindItemMetadata :: Default TermBindItemMetadata where
  default = TermBindItemMetadata { indented: false }

derive instance newTypeTermBindItemMetadata :: Newtype TermBindItemMetadata _

instance showTermBindItemMetadata :: Show TermBindItemMetadata where
  show x = "TermBindItemMetadata " <> show (unwrap x)

-- | Name 
newtype Name
  = Name (Maybe String)

derive instance newTypeName :: Newtype Name _

derive newtype instance eqName :: Eq Name

derive newtype instance ordName :: Ord Name

instance showName :: Show Name where
  show x = "Name " <> show (unwrap x)

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
-- | Proxies
_lam = Proxy :: Proxy "lam"

_neu = Proxy :: Proxy "neu"

_let_ = Proxy :: Proxy "let_"

_buf = Proxy :: Proxy "buf"

_data_ = Proxy :: Proxy "data_"

_match = Proxy :: Proxy "match"

_hole = Proxy :: Proxy "hole"
