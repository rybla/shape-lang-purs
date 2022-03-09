module App where

import AppAction
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Rendering
import Language.Shape.Stlc.Syntax
import Prelude
import Data.Array as Array
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.UUID as UUID
import Debug as Debug
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Halogen as Effect
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Language.Shape.Stlc.Metadata as Metadata
import Language.Shape.Stlc.Recursion.MetaContext (emptyMetaContext)
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe as Unsafe

type AppState
  = { consoleState :: ConsoleState
    , editorState :: EditorState
    }

type ConsoleState
  = { logs :: Array String
    }

type EditorState
  = { module_ :: Module }

type AppSlots q
  = ( editor :: H.Slot q AppAction Int
    , console :: H.Slot q AppAction Int
    , module :: SyntaxSlot q
    , definition :: SyntaxSlot q
    , block :: SyntaxSlot q
    , constructor :: SyntaxSlot q
    , type :: SyntaxSlot q
    , term :: SyntaxSlot q
    , neutralTerm :: SyntaxSlot q
    , case_ :: SyntaxSlot q
    , parameter :: SyntaxSlot q
    , typeName :: SyntaxSlot q
    , termName :: SyntaxSlot q
    )

_editor = Proxy :: Proxy "editor"

_console = Proxy :: Proxy "console"

renderApp :: forall q i o. H.Component q i o Aff
renderApp =
  H.mkComponent
    { initialState:
        const
          { consoleState: { logs: [] }
          , editorState: { module_ }
          }
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction =
              case _ of
                LogConsole msg -> H.modify_ \st -> st { consoleState { logs = msg Array.: st.consoleState.logs } }
            }
    , render
    }
  where
  module_ = unsafePerformEffect initialModule

  render :: AppState -> H.ComponentHTML AppAction (AppSlots q) Aff
  render st =
    HH.div
      [ HP.class_ (HH.ClassName "app") ]
      [ HH.slot _editor 0 renderEditor st.editorState identity
      , HH.slot _console 0 renderConsole st.consoleState identity
      ]

data ChildAction st
  = AppAction AppAction
  | UpdateState (st -> st)

renderEditor :: forall q m. H.Component q EditorState AppAction m
renderEditor =
  H.mkComponent
    { initialState: identity
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction =
              case _ of
                AppAction appAction -> H.raise appAction
                UpdateState f -> H.modify_ f
            , receive = \st -> Just $ UpdateState (const st)
            }
    , render
    }
  where
  render :: EditorState -> H.ComponentHTML (ChildAction EditorState) (AppSlots q) m
  render st =
    HH.div
      [ HP.class_ (HH.ClassName "editor") ]
      [ HH.slot _module 0 (renderModule st.module_ Map.empty emptyMetaContext (\mod _ _ -> mod)) initialSyntaxState AppAction ]

renderConsole :: forall q m. H.Component q ConsoleState AppAction m
renderConsole =
  H.mkComponent
    { initialState: identity
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction =
              case _ of
                AppAction appAction -> H.raise appAction
                UpdateState f -> H.modify_ f
            , receive = \st -> Just $ UpdateState (const st)
            }
    , render:
        \st ->
          HH.div
            [ HP.class_ (HH.ClassName "console") ]
            [ HH.div_ (map (HH.div_ <<< Array.singleton <<< HH.text) $ Array.reverse st.logs)
            ]
    }

initialModule :: Effect Module
initialModule = do
  x_termID <- TermID <$> UUID.genUUID
  h1_holeID <- HoleID <$> UUID.genUUID
  h2_holeID <- HoleID <$> UUID.genUUID
  pure
    $ Module
        ( List.fromFoldable
            [ TermDefinition
                (TermBinding x_termID defaultTermBindingMetadata { name = TermName $ Just "identity" })
                ( ArrowType
                    (Parameter (HoleType h1_holeID Set.empty defaultHoleTypeMetadata) defaultParameterMetadata { name = TermName $ Just "x" })
                    (HoleType h2_holeID Set.empty defaultHoleTypeMetadata)
                    defaultArrowTypeMetadata
                )
                undefined -- (LambdaTerm ())
                defaultTermDefinitionMetadata
            ]
        )
        Metadata.defaultModuleMetadata
