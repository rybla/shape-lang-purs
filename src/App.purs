module App where

import App.Action
import App.Query
import App.State
import Control.Monad
import Data.Foldable
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Rendering
import Language.Shape.Stlc.Syntax
import Prelude
import Data.Array as Array
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Traversable (sequence)
import Data.UUID as UUID
import Debug as Debug
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Halogen as Effect
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Language.Shape.Stlc.Metadata as Metadata
import Language.Shape.Stlc.Recursion.MetaContext (emptyMetaContext)
import Record as R
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe as Unsafe

type AppSlots
  = ( editor :: H.Slot EditorQuery AppAction Int
    , console :: H.Slot ConsoleQuery AppAction Int
    , module :: SyntaxSlot
    , definition :: SyntaxSlot
    , block :: SyntaxSlot
    , constructor :: SyntaxSlot
    , type :: SyntaxSlot
    , term :: SyntaxSlot
    , neutralTerm :: SyntaxSlot
    , case_ :: SyntaxSlot
    , parameter :: SyntaxSlot
    , typeName :: SyntaxSlot
    , termName :: SyntaxSlot
    )

_editor = Proxy :: Proxy "editor"

_console = Proxy :: Proxy "console"

renderApp :: forall q i o. H.Component q i o Aff
renderApp =
  H.mkComponent
    { initialState: const {}
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    , render: render
    }
  where
  handleAction action =
    let
      _ = Debug.trace "renderApp.handleAction" identity

      _ = Debug.trace action identity
    in
      case action of
        LogAppAction msg -> void $ H.query _console 0 (LogConsoleQuery msg)

  render :: AppState -> H.ComponentHTML AppAction AppSlots Aff
  render st =
    HH.div
      [ HP.class_ (HH.ClassName "app") ]
      [ HH.slot _editor 0 renderEditor unit identity
      , HH.slot _console 0 renderConsole unit identity
      ]

renderEditor :: forall q i m. H.Component q i AppAction m
renderEditor =
  H.mkComponent
    { initialState: const { module_: unsafePerformEffect initialModule }
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    , render
    }
  where
  handleAction action =
    let
      _ = Debug.trace "renderEditor.handleAction" identity

      _ = Debug.trace action identity
    in
      case action of
        LiftEditorAction appAction -> H.raise appAction
        UpdateEditorAction f -> H.modify_ f
        SequenceEditorActions actions -> traverse_ handleAction actions

  render :: EditorState -> H.ComponentHTML EditorAction AppSlots m
  render st =
    HH.div
      [ HP.class_ (HH.ClassName "editor") ]
      [ HH.slot _module 0 (renderModule st.module_ Map.empty emptyMetaContext const) initialSyntaxState identity ]

renderConsole :: forall i m. H.Component ConsoleQuery i AppAction m
renderConsole =
  H.mkComponent
    { initialState: const { logs: [] }
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            , handleQuery = handleQuery
            }
    , render:
        \st ->
          Debug.trace st
            $ \_ ->
                HH.div
                  [ HP.class_ (HH.ClassName "console") ]
                  [ HH.div_ (map (HH.div_ <<< Array.singleton <<< HH.text) $ Array.reverse st.logs)
                  ]
    }
  where
  handleAction = H.raise

  handleQuery :: forall a. ConsoleQuery a -> H.HalogenM ConsoleState AppAction () AppAction m (Maybe a)
  handleQuery = case _ of
    LogConsoleQuery msg -> do
      H.modify_ $ R.modify _logs (msg Array.: _)
      pure Nothing

-- where
-- -- eval :: ConsoleQuery ~> H.Component
-- -- LogConsoleQuery msg -> ?a
-- eval :: H.HalogenQ ConsoleQuery AppAction ConsoleState ~> H.HalogenM ConsoleState AppAction () AppAction m
-- eval = case _ of 
--   H.Initialize 
initialModule :: Effect Module
initialModule = do
  id_termId <- TermId <$> UUID.genUUID
  log $ "id_termId: " <> show id_termId
  x_termId <- TermId <$> UUID.genUUID
  log $ "x_termId: " <> show x_termId
  h1_holeId <- HoleId <$> UUID.genUUID
  h2_holeId <- HoleId <$> UUID.genUUID
  pure
    $ Module
        ( List.fromFoldable
            [ TermDefinition
                (TermBinding id_termId defaultTermBindingMetadata { name = TermName $ Just "identity" })
                ( ArrowType
                    (Parameter (HoleType h1_holeId Set.empty defaultHoleTypeMetadata) defaultParameterMetadata { name = TermName $ Just "x" })
                    (HoleType h2_holeId Set.empty defaultHoleTypeMetadata)
                    defaultArrowTypeMetadata
                )
                ( LambdaTerm
                    x_termId
                    ( Block List.Nil
                        (NeutralTerm x_termId NoneArgs defaultNeutralTermMetadata)
                        defaultBlockMetadata
                    )
                    defaultLambdaTermMetadata
                )
                defaultTermDefinitionMetadata
            ]
        )
        Metadata.defaultModuleMetadata
