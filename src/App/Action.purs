module App.Action where

import Language.Shape.Stlc.Syntax
import Prelude
import App.State (Mode(..), State, logConsole)
import Data.Foldable (traverse_)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Web.Event.Event as E
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

data Action
  = Init
  | HandleKey H.SubscriptionId KeyboardEvent
  | SetModule Module
  | SetPalette (Array Action)
  | SetMode Mode
  | ModifyTermName (TermName -> TermName)
  | ModifyTypeName (TermName -> TermName)
  | LogConsole String
  | Sequence (Array Action)

handleAction :: forall cs output. Action -> H.HalogenM State Action cs output Aff Unit
handleAction = case _ of
  Init -> do
    document <- H.liftEffect $ Web.document =<< Web.window
    H.subscribe' \sid ->
      eventListener
        KET.keydown
        (HTMLDocument.toEventTarget document)
        (map (HandleKey sid) <<< KE.fromEvent)
  HandleKey sid event -> handleAction (LogConsole <<< show $ key event)
  SetModule module_ -> H.modify_ (_ { module_ = module_ })
  SetPalette _ -> H.modify_ \st -> st -- TODO
  SetMode mode -> H.modify_ (_ { mode = mode })
  ModifyTermName modifyTermName ->
    H.modify_ \st -> case st.mode of
      EditTermName wrapModifyTermName -> st { module_ = wrapModifyTermName modifyTermName }
      _ -> logConsole "[invalid action] tried ModifyTermName while mode isn't EditTermName" st
  ModifyTypeName modifyTypeName -> H.modify_ \st -> st
  LogConsole msg -> H.modify_ $ logConsole msg
  Sequence actions -> traverse_ handleAction actions
