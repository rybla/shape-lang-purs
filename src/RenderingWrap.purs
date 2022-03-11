module RenderingWrap where

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
import Prim.Row (class Cons)
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe as Unsafe

-- Universal syntax interface
type SyntaxComponent st m
  = H.Component (SyntaxQuery st) st Action.EditorAction m

data SyntaxAction st
  = EditorAction Action.EditorAction
  | ModifyStateSyntaxAction (st -> st)

data SyntaxQuery st a
  = ModifyStateSyntaxQuery (st -> st)

type SyntaxSlot st
  -- = H.Slot (SyntaxQuery st) Action.EditorAction Int
  = H.Slot (SyntaxQuery st) (SyntaxAction st) Int

type SyntaxSlots
  = ( module :: SyntaxSlot ModuleState
    , definitions :: SyntaxSlot DefinitionsState
    , block :: SyntaxSlot BlockState
    , constructor :: SyntaxSlot ConstructorState
    , type :: SyntaxSlot TypeState
    , term :: SyntaxSlot TermState
    , args :: SyntaxSlot ArgsState
    , case_ :: SyntaxSlot CaseState
    , parameter :: SyntaxSlot ParameterState
    , typeId :: SyntaxSlot TypeIdState
    , termId :: SyntaxSlot TermIdState
    , typeName :: SyntaxSlot TypeNameState
    , termName :: SyntaxSlot TermNameState
    )

_definitions = Proxy :: Proxy "definitions"

_definition = Proxy :: Proxy "definition"

_block = Proxy :: Proxy "block"

_constructor = Proxy :: Proxy "constructor"

_type = Proxy :: Proxy "type"

_term = Proxy :: Proxy "term"

_args = Proxy :: Proxy "args"

_case = Proxy :: Proxy "case_"

_parameter = Proxy :: Proxy "parameter"

_typeId = Proxy :: Proxy "typeId"

_termId = Proxy :: Proxy "termId"

_typeName = Proxy :: Proxy "typeName"

_termName = Proxy :: Proxy "termName"

-- Universal syntax utilities
classSyntax :: forall r1 r2 i. { cursor :: Boolean | r1 } -> String -> HP.IProp ( class ∷ String | r2 ) i
classSyntax st label = HP.class_ (HH.ClassName $ label <> if st.cursor then " selected" else "")

-- Render types
type ModuleState
  = { module_ :: Module, gamma :: Context, metaGamma :: MetaContext, wrap_module :: Wrap Module, view :: ModuleViewdata }

type ModuleViewdata
  = { cursor :: Boolean }

defaultModuleViewdata = { cursor: false } :: ModuleViewdata

type BlockState
  = { block :: Block, gamma :: Context, alpha :: Type, metaGamma :: MetaContext, wrap_block :: Wrap Block, view :: BlockViewdata }

type BlockViewdata
  = { cursor :: Boolean }

defaultBlockViewdata = { cursor: false } :: BlockViewdata

type DefinitionsState
  = { definitions :: List Definition, gamma :: Context, metaGamma :: MetaContext, wrap_definitions :: Wrap (List Definition), view :: DefinitionsViewdata, subviews :: Array DefinitionViewdata }

type DefinitionsViewdata
  = { cursor :: Boolean }

type DefinitionViewdata
  = { cursor :: Boolean }

defaultDefinitionsViewdata = { cursor: false } :: DefinitionsViewdata

defaultDefinitionViewdata = { cursor: false } :: DefinitionViewdata

type ConstructorState
  = { constructor :: Constructor, gamma :: Context, typeBinding :: TypeBinding, metaGamma :: MetaContext, wrap_constructor :: Wrap Constructor }

type TypeState
  = { type_ :: Type, gamma :: Context, metaGamma :: MetaContext, wrap_type :: Wrap Type, view :: TypeViewdata }

type TypeViewdata
  = { cursor :: Boolean }

defaultTypeViewdata = { cursor: false } :: TypeViewdata

type TermState
  = { term :: Term, gamma :: Context, metaGamma :: MetaContext, wrap_term :: Wrap Term, view :: TermViewdata }

type TermViewdata
  = { cursor :: Boolean }

defaultTermViewdata = { cursor: false } :: TermViewdata

type ArgsState
  = {}

type CaseState
  = {}

type ParameterState
  = {}

type TypeIdState
  = { typeId :: TypeId, gamma :: Context, metaGamma :: MetaContext, view :: TypeIdViewdata }

type TypeIdViewdata
  = { cursor :: Boolean }

defaultTypeIdViewdata = { cursor: false } :: TypeIdViewdata

type TermIdState
  = { termId :: TermId, gamma :: Context, metaGamma :: MetaContext, view :: TermIdViewdata }

type TermIdViewdata
  = { cursor :: Boolean }

defaultTermIdViewdata = { cursor: false } :: TermIdViewdata

type TypeNameState
  = { typeName :: TypeName, gamma :: Context, metaGamma :: MetaContext, view :: TypeNameViewdata }

type TypeNameViewdata
  = { cursor :: Boolean }

defaultTypeNameViewdata = { cursor: false } :: TypeNameViewdata

type TermNameState
  = { termName :: TermName, gamma :: Context, metaGamma :: MetaContext, view :: TermNameViewdata }

type TermNameViewdata
  = { cursor :: Boolean }

defaultTermNameViewdata = { cursor: false } :: TermNameViewdata

type SyntaxRenderer st m
  = st -> SyntaxComponent st m

-- Specific syntax renderers
moduleComponent :: forall m. SyntaxComponent ModuleState m
moduleComponent =
  H.mkComponent
    { initialState: identity
    , render: render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction }
    }
  where
  render st@{ module_, gamma, metaGamma, wrap_module, view } =
    RecWrap.recModule
      { module_:
          \definitions meta gamma metaGamma wrap_mod wrap_definitions ->
            HH.span
              [ classSyntax st.view "module_" ]
              [ HH.slot _definitions 0 definitionsComponent { definitions, gamma, metaGamma, wrap_definitions, view: defaultDefinitionsViewdata, subviews: Array.replicate (List.length definitions) defaultDefinitionViewdata } EditorAction ]
      }
      module_
      gamma
      metaGamma
      wrap_module

  handleAction = case _ of
    EditorAction editorAction -> H.raise editorAction
    ModifyStateSyntaxAction modifyState -> H.modify_ modifyState

definitionsComponent :: forall m. SyntaxComponent DefinitionsState m
definitionsComponent =
  H.mkComponent
    { initialState: identity
    , render: render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction }
    }
  where
  render st@{ definitions, gamma, metaGamma, wrap_definitions, view } =
    RecWrap.recDefinitions
      { definitions:
          \definitions gamma metaGamma wrap_definitions wrap_definition_at ->
            HH.span
              [ classSyntax st.view "definitions" ]
              ( Array.fromFoldable
                  $ List.mapWithIndex
                      ( \i -> case _ of
                          TermDefinition termBnd@(TermBinding termId _) alpha a meta ->
                            HH.span
                              [ undefined {-TODO: indentation-} ]
                              [ keyword.let_
                              , punctuation.space
                              , HH.slot _termName i termIdComponent { termId, gamma, metaGamma, view: defaultTermIdViewdata } EditorAction
                              , punctuation.space
                              , punctuation.colon
                              , punctuation.space
                              , HH.slot _typeName i typeComponent { type_: alpha, gamma, metaGamma, wrap_type: \type' tc -> wrap_definition_at i (TermDefinition termBnd type' a meta) tc, view: defaultTypeViewdata } EditorAction
                              , punctuation.space
                              , punctuation.termdef
                              , punctuation.space
                              , HH.slot _term i termComponent { term: a, gamma, metaGamma, wrap_term: \term' tc -> wrap_definition_at i (TermDefinition termBnd alpha term' meta) tc, view: defaultTermViewdata } EditorAction
                              ]
                          DataDefinition typeBnd@(TypeBinding typeId _) constrs meta ->
                            HH.span
                              [ undefined {-TODO: indentation-} ]
                              [ keyword.data_
                              , punctuation.space
                              , HH.slot _typeId i typeIdComponent { typeId, gamma, metaGamma, view: defaultTypeIdViewdata } EditorAction
                              , punctuation.space
                              , punctuation.typedef
                              , punctuation.space
                              , intercalateHTML (List.fromFoldable [ punctuation.space, punctuation.alt, punctuation.space ])
                                  ( List.mapWithIndex
                                      ( \i -> case _ of
                                          Constructor termBnd parameters meta -> undefined -- HH.span  --   [ classSyntax  ] undefined {-TODO-} 
                                      )
                                      constrs
                                  )
                              ]
                      )
                      definitions
              )
      }
      definitions
      gamma
      metaGamma
      wrap_definitions

  handleAction = case _ of
    EditorAction editorAction -> H.raise editorAction
    ModifyStateSyntaxAction modifyState -> H.modify_ modifyState

typeComponent :: forall m. SyntaxComponent TypeState m
typeComponent = undefined

termComponent :: forall m. SyntaxComponent TermState m
termComponent = undefined

typeIdComponent :: forall m. SyntaxComponent TypeIdState m
typeIdComponent = undefined

termIdComponent :: forall m. SyntaxComponent TermIdState m
termIdComponent = undefined
