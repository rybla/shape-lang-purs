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

data SyntaxAction syntax
  = AppAction App.AppAction
  | ModifyState (SyntaxState syntax -> SyntaxState syntax)

type SyntaxState syntax
  = { syntax :: syntax
    , cursor :: Boolean
    }

type SyntaxComponent q syntax m
  = H.Component q (SyntaxState syntax) App.AppAction m

mkRenderSyntax ::
  forall syntax q m.
  (forall w. SyntaxState syntax -> HH.HTML w (SyntaxAction syntax)) ->
  SyntaxComponent q syntax m
mkRenderSyntax render =
  H.mkComponent
    { initialState: identity
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction =
              case _ of
                AppAction appAction -> H.raise appAction
                ModifyState modifyState -> H.modify_ modifyState
            , receive = Just <<< ModifyState <<< const
            }
    , render
    }
