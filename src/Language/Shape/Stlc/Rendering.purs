module Language.Shape.Stlc.Rendering where

import Data.Tuple.Nested
import Language.Shape.Stlc.Recursion.MetaContext
import Language.Shape.Stlc.RenderingAux
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import App as App
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple as Tuple
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Language.Shape.Stlc.Recursion.Wrap (Wrap, WrapI)
import Language.Shape.Stlc.Typing (Context)
import Prim.Row (class Cons)
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe as Unsafe

{-
Universal syntax interface
-}
type SyntaxComponent q m
  = H.Component q SyntaxState App.AppAction m

type SyntaxState
  = { cursor :: Boolean
    }

type SyntaxSlots q
  = ( definition :: H.Slot q App.AppAction Int
    , block :: H.Slot q App.AppAction Int
    , constructor :: H.Slot q App.AppAction Int
    , type :: H.Slot q App.AppAction Int
    , term :: H.Slot q App.AppAction Int
    , neutralTerm :: H.Slot q App.AppAction Int
    , typeBinding :: H.Slot q App.AppAction Int
    , termBinding :: H.Slot q App.AppAction Int
    )

_definition = Proxy :: Proxy "definition"

_block = Proxy :: Proxy "block"

_constructor = Proxy :: Proxy "constructor"

_type = Proxy :: Proxy "type"

_term = Proxy :: Proxy "term"

_neutralTerm = Proxy :: Proxy "neutralTerm"

_typeBinding = Proxy :: Proxy "typeBinding"

_termBinding = Proxy :: Proxy "termBinding"

data SyntaxAction
  = AppAction App.AppAction
  | ModifyState (SyntaxState -> SyntaxState)

initialSyntaxState :: SyntaxState
initialSyntaxState =
  { cursor: false
  }

-- HH.HTML (H.ComponentSlot slots m SyntaxAction)
mkSyntaxComponent ::
  forall syntax q m.
  (SyntaxState -> HH.ComponentHTML SyntaxAction (SyntaxSlots q) m) ->
  SyntaxComponent q m
mkSyntaxComponent render =
  H.mkComponent
    { initialState:
        const
          { cursor: false
          }
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction =
              case _ of
                AppAction appAction -> H.raise appAction
                ModifyState modifyState -> H.modify_ modifyState
            -- , receive = Just <<< ModifyState <<< const
            }
    , render: render
    }

slotSyntax ::
  forall label slot q m _1.
  IsSymbol label =>
  Ord slot =>
  Cons label (H.Slot q App.AppAction slot) _1 (SyntaxSlots q) =>
  Proxy label ->
  slot ->
  SyntaxComponent q m ->
  H.ComponentHTML SyntaxAction (SyntaxSlots q) m
slotSyntax label i html = HH.slot label i html initialSyntaxState AppAction

{-
Universal syntax utilities
-}
syntaxClass :: forall r i. SyntaxState -> String -> HP.IProp ( class ∷ String | r ) i
syntaxClass st label = HP.class_ (HH.ClassName $ label <> if st.cursor then " selected" else "")

{-
Specific syntax renderers
-}
renderModule :: forall q m. Module -> Context -> Wrap Module -> MetaContext -> SyntaxComponent q m
renderModule =
  recModule
    { module_:
        \defs meta gamma wrapDefI metaGamma ->
          mkSyntaxComponent \st ->
            HH.span
              [ syntaxClass st "module" ]
              [ intercalateHTML (List.singleton punctuation.newline)
                  $ List.mapWithIndex
                      ( \i def ->
                          slotSyntax
                            (Proxy :: Proxy "definition")
                            i
                            (renderDefinition def gamma (wrapDefI i) metaGamma)
                      )
                      defs
              ]
    }

renderBlock :: forall q m. Block -> Context -> Wrap Block -> MetaContext -> SyntaxComponent q m
renderBlock = undefined

renderDefinition :: forall q m. Definition -> Context -> Wrap Definition -> MetaContext -> SyntaxComponent q m
renderDefinition =
  recDefinition
    { term:
        \x alpha a meta gamma wrapType wrapTerm metaGamma ->
          mkSyntaxComponent \st ->
            HH.span
              [ syntaxClass st "term definition" ]
              [ keyword.let_
              , punctuation.space
              , slotSyntax _termBinding 0 $ renderTermBinding x gamma metaGamma
              , punctuation.space
              , punctuation.colon
              , punctuation.space
              , slotSyntax _type 0 $ renderType alpha gamma wrapType metaGamma
              , punctuation.space
              , punctuation.termdef
              , punctuation.space
              , slotSyntax _term 0 $ renderTerm a gamma alpha wrapTerm metaGamma
              ]
    , data:
        \x constrs meta gamma wrapConstructorI metaGamma ->
          mkSyntaxComponent \st ->
            HH.span
              [ syntaxClass st "data definition" ]
              [ keyword.data_
              , punctuation.space
              , slotSyntax _typeBinding 0 $ renderTypeBinding x gamma metaGamma
              , punctuation.space
              , punctuation.typedef
              , punctuation.space
              , intercalateHTML (List.fromFoldable [ punctuation.space, punctuation.alt, punctuation.space ])
                  ( List.mapWithIndex
                      ( \i constr ->
                          slotSyntax _constructor i $ renderConstructor constr gamma (wrapConstructorI i) metaGamma
                      )
                      constrs
                  )
              ]
    }

renderConstructor :: forall q m. Constructor -> Context -> Wrap Constructor -> MetaContext -> SyntaxComponent q m
renderConstructor = undefined

renderType :: forall q m. Type -> Context -> Wrap Type -> MetaContext -> SyntaxComponent q m
renderType = undefined

renderTerm :: forall q m. Term -> Context -> Type -> Wrap Term -> MetaContext -> SyntaxComponent q m
renderTerm = undefined

renderNeutralTerm :: forall q m. NeutralTerm -> Context -> Type -> Wrap NeutralTerm -> MetaContext -> SyntaxComponent q m
renderNeutralTerm = undefined

renderTypeBinding :: forall q m. TypeBinding -> Context -> MetaContext -> SyntaxComponent q m
renderTypeBinding = undefined

renderTermBinding :: forall q m. TermBinding -> Context -> MetaContext -> SyntaxComponent q m
renderTermBinding = undefined
