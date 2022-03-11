module Language.Shape.Stlc.Rendering where

import Data.Tuple.Nested
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.RenderingAux
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import App.Action (setModule)
import App.Action as Action
import Data.Array as Array
import Data.List.Unsafe (List)
import Data.List.Unsafe as List
import Data.Map.Unsafe as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Set as Set
import Data.Symbol (class IsSymbol)
import Data.Tuple as Tuple
import Debug as Debug
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Language.Shape.Stlc.Changes (TypeChange(..))
import Language.Shape.Stlc.Recursion.MetaContext (MetaContext)
import Language.Shape.Stlc.Recursion.MetaContext as RecMeta
import Language.Shape.Stlc.Recursion.Wrap (Wrap, IndexWrap)
import Language.Shape.Stlc.Recursion.Wrap as RecWrap
import Language.Shape.Stlc.Typing (Context)
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe as Unsafe

type SyntaxComponent query state action m
  = H.Component query state action m

-- type SyntaxSlot = H.Slot query action Int
type SyntaxSlots
  = ( module_ :: H.Slot ModuleQuery ModuleState ModuleAction
    , definitions :: H.Slot DefinitionsQuery DefinitionsState DefinitionsAction
    , block :: H.Slot BlockQuery BlockState BlockAction
    , type :: H.Slot TypeQuery TypeState TypeAction
    , term :: H.Slot TermQuery TermState TermAction
    , typeId :: H.Slot TypeIdQuery TypeIdState TypeIdAction
    , termId :: H.Slot TermIdQuery TermIdState TermIdAction
    , typeName :: H.Slot TypeNameQuery TypeNameState TypeNameAction
    , termName :: H.Slot TermNameQuery TermNameState TermNameAction
    )

data ModuleQuery a

type ModuleState
  = {}

data ModuleAction

data DefinitionsQuery a

type DefinitionsState
  = {}

data DefinitionsAction

data BlockQuery a

type BlockState
  = {}

data BlockAction

data TypeQuery a

type TypeState
  = {}

data TypeAction

data TermQuery a

type TermState
  = {}

data TermAction

data TypeIdQuery a

type TypeIdState
  = {}

data TypeIdAction

data TermIdQuery a

type TermIdState
  = {}

data TermIdAction

data TypeNameQuery a

type TypeNameState
  = {}

data TypeNameAction

data TermNameQuery a

type TermNameState
  = {}

data TermNameAction
