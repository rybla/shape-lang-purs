module Language.Shape.Stlc.Metadata where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String (codePointFromChar)
import Data.String as String
import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons)
import Record as Record
import Type.Proxy (Proxy(..))
import Undefined (undefined)

_indented = Proxy :: Proxy "indented"

_indented_term = Proxy :: Proxy "indented_term"

_indented_type = Proxy :: Proxy "indented_type"

_hidden = Proxy :: Proxy "hidden"

_annotated = Proxy :: Proxy "annotated"

type ModuleMetadata
  = { hidden :: Boolean }

defaultModuleMetadata = { hidden: true } :: ModuleMetadata

type BlockMetadata
  = { indented :: Boolean, hidden :: Boolean }

defaultBlockMetadata = { indented: false, hidden: false } :: BlockMetadata

type DefinitionItemMetadata
  = {}

defaultDefinitionItemMetadata = {} :: DefinitionItemMetadata

type TermDefinitionMetadata
  = { indented_type :: Boolean, indented_term :: Boolean, hidden :: Boolean }

defaultTermDefinitionMetadata = { indented_type: false, indented_term: false, hidden: false } :: TermDefinitionMetadata

type DataDefinitionMetadata
  = { hidden :: Boolean }

defaultDataDefinitionMetadata = { hidden: false } :: DataDefinitionMetadata

type ConstructorItemMetadata
  = { indented :: Boolean }

defaultConstructorItemMetadata = { indented: true } :: ConstructorItemMetadata

type ConstructorMetadata
  = {}

defaultConstructorMetadata = {} :: ConstructorMetadata

type ParameterItemMetadata
  = { indented :: Boolean }

defaultParameterItemMetadata = { indented: false } :: ParameterItemMetadata

type ArrowTypeMetadata
  = { indented :: Boolean }

defaultArrowTypeMetadata = { indented: false } :: ArrowTypeMetadata

type DataTypeMetadata
  = {}

defaultDataTypeMetadata = {} :: DataTypeMetadata

type HoleTypeMetadata
  = {}

defaultHoleTypeMetadata = {} :: HoleTypeMetadata

type LambdaTermMetadata
  = { annotated :: Boolean }

defaultLambdaTermMetadata = { annotated: true } :: LambdaTermMetadata

-- this shouldn't have "indented", but for some reason causes an error in Changes...
type ArgConsMetaData
  = { indented :: Boolean }

defaultArgConsMetaData = { indented: false } :: ArgConsMetaData

type NeutralTermMetadata
  = {}

defaultNeutralTermMetadata = {} :: NeutralTermMetadata

type ArgItemMetadata
  = { indented :: Boolean }

defaultArgItemMetadata = { indented: false } :: ArgItemMetadata

type VariableTermMetadata
  = {}

defaultVariableTermMetadata = {} :: VariableTermMetadata

type MatchTermMetadata
  = {}

defaultMatchTermMetadata = {} :: MatchTermMetadata

type CaseItemMetadata
  = { indented :: Boolean }

defaultCaseItemMetadata = { indented: true } :: CaseItemMetadata

type TermIdItemMetadata
  = { indented :: Boolean }

defaultTermIdItemMetadata = { indented: false } :: TermIdItemMetadata

type HoleTermMetadata
  = {}

defaultHoleTermMetadata = {} :: HoleTermMetadata

type CaseMetadata
  = {}

defaultCaseMetadata = {} :: CaseMetadata

type ParameterMetadata
  = { name :: TermName }

defaultParameterMetadata = { name: defaultTermName } :: ParameterMetadata

type TypeBindingMetadata
  = { name :: TypeName }

defaultTypeBindingMetadata = { name: defaultTypeName } :: TypeBindingMetadata

type TermBindingMetadata
  = { name :: TermName }

defaultTermBindingMetadata = { name: defaultTermName } :: TermBindingMetadata

newtype TypeName
  = TypeName String

newtype TermName
  = TermName String

defaultTypeName = TypeName "_" :: TypeName 
defaultTermName = TermName "_" :: TermName

derive instance newtypeTypeName :: Newtype TypeName _ 
derive instance newtypeTermName :: Newtype TermName _

-- instances for TypeName
derive instance genericTypeNam :: Generic TypeName _

derive instance eqTypeName :: Eq TypeName

derive instance ordTypeName :: Ord TypeName

-- instance showTypeName :: Show TypeName where
--   show (TypeName (Just label)) = label
--   show (TypeName Nothing) = "_"
instance showTypeName :: Show TypeName where show x = genericShow x 

-- readTermName :: String -> TermName
-- readTermName str =
--   let
--     str' = String.trim str
--   in
--     if str' == "_" then
--       TermName Nothing
--     else
--       TermName (Just str')

-- readTypeName :: String -> TypeName
-- readTypeName str =
--   let
--     str' = String.trim str
--   in
--     if str' == "_" then
--       TypeName "_"
--     else
--       TypeName (Just str')

-- instances for TermName
derive instance genericTermName :: Generic TermName _

derive instance eqTermName :: Eq TermName

derive instance ordTermName :: Ord TermName

-- instance showTermName :: Show TermName where
--   show (TermName (Just label)) = label
--   show (TermName Nothing) = "_"
instance showTermName :: Show TermName where show x = genericShow x 

toggle :: forall label row' row. IsSymbol label => Cons label Boolean row' row => Proxy label -> Record row -> Record row
toggle label = Record.modify label not
