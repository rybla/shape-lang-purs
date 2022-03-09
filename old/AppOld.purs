module AppOld where

import Prelude
import Data.Array as Array
import Data.List as List
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Effect.Console as Console
import Halogen as Effect
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Language.Shape.Stlc.Metadata as Metadata
import Language.Shape.Stlc.Syntax as Syntax
import Type.Proxy (Proxy(..))
import Unsafe as Unsafe

-- AppState
type AppState
  = { consoleState :: ConsoleState
    , editorState :: EditorState
    }

type ConsoleState
  = { logs :: Array String
    }

type EditorState
  = { module_ :: Syntax.Module }

data AppAction
  = LogConsole String

type AppSlots
  = ( editor :: forall q. H.Slot q AppAction Int
    , console :: forall q. H.Slot q AppAction Int
    , link :: forall q. H.Slot q AppAction Int
    )

_editor = Proxy :: Proxy "editor"

_console = Proxy :: Proxy "console"

_link = Proxy :: Proxy "link"

renderApp :: forall q i o. H.Component q i o Aff
renderApp =
  H.mkComponent
    { initialState:
        const
          { consoleState: { logs: [] }
          , editorState: { module_: Syntax.Module List.Nil Metadata.defaultModuleMetadata }
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
  render :: AppState -> H.ComponentHTML AppAction AppSlots Aff
  render st =
    HH.div
      [ HP.class_ (HH.ClassName "app") ]
      [ HH.slot _editor 0 renderEditor st.editorState identity
      , HH.slot _console 0 renderConsole st.consoleState identity
      , HH.slot _link 0 renderLink 5 identity
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
  render :: EditorState -> H.ComponentHTML (ChildAction EditorState) AppSlots m
  render st =
    HH.button
      [ HP.class_ (HH.ClassName "editor")
      , HE.onClick \_ -> AppAction $ LogConsole "hello"
      ]
      [ HH.text "editor" ]

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
            [ HH.div_ [ HH.text "console" ]
            , HH.div_ (map (HH.div_ <<< Array.singleton <<< HH.text) $ Array.reverse st.logs)
            ]
    }

renderLink :: forall q m. H.Component q Int AppAction m
renderLink =
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
          HH.div_
            ( [ HH.button
                  [ HE.onClick \_ -> AppAction $ LogConsole $ "link " <> show st ]
                  [ HH.text $ "link " <> show st ]
              ]
                <> ( if st > 0 then
                      [ HH.slot _link 0 renderLink (st - 1) AppAction ]
                    else
                      []
                  )
            )
    }
