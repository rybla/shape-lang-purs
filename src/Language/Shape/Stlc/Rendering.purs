module Language.Shape.Stlc.Rendering where

import Prelude
import App as App
import Data.List.Lazy.NonEmpty as List
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Language.Shape.Stlc.Syntax as Syntax
import Prim.Row (class Cons)
import Type.Proxy (Proxy)
import Undefined (undefined)

data SyntaxAction st
  = AppAction App.AppAction
  | ModifyState (st -> st)

type SyntaxComponent q st m
  = H.Component q st (SyntaxAction st) m

mkRenderSyntax ::
  forall syntax st q m.
  st ->
  (forall w. syntax -> HH.HTML w (SyntaxAction st)) ->
  syntax ->
  SyntaxComponent q st m -- H.Component q st App.AppAction m
mkRenderSyntax initialState render syntax =
  H.mkComponent
    { initialState: const initialState
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction =
              case _ of
                AppAction appAction -> H.raise appAction
                ModifyState modifyState -> H.modify_ modifyState
            -- , receive = Just <<< ModifyState <<< const
            }
    , render: const $ render syntax
    , receive: undefined
    }

slotSyntax ::
  forall st query input slots m label _1.
  Cons label (H.Slot query (SyntaxAction st) Int) _1 slots =>
  IsSymbol label =>
  Proxy label ->
  Int ->
  input ->
  H.Component query input (SyntaxAction st) m ->
  HH.HTML (H.ComponentSlot slots m (SyntaxAction st)) (SyntaxAction st)
slotSyntax label slot initialState syntax = HH.slot label slot syntax initialState identity

renderModule :: forall q m. Syntax.Module -> SyntaxComponent q Unit m
renderModule =
  mkRenderSyntax unit \(Syntax.Module defs meta) ->
    HH.div
      [ HP.class_ (HH.ClassName "module") ]
      (map (slotSyntax ?label ?slot ?input (renderDefinition ?def)) (List.toUnfoldable defs))

renderDefinition :: forall q m. Syntax.Definition -> SyntaxComponent q Unit m
renderDefinition = mkRenderSyntax unit \def -> undefined
