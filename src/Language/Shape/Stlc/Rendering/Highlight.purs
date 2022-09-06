module Language.Shape.Stlc.Rendering.Highlight where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Language.Shape.Stlc.Rendering.Types (NodeProps)
import Language.Shape.Stlc.Types (This)
import Partial.Unsafe (unsafeCrashWith)
import React (getState, modifyState)
import React.DOM.Props as Props
import React.SyntheticEvent (stopPropagation)
import Web.HTML (HTMLElement)

foreign import setHighlight :: Boolean -> HTMLElement -> Effect Unit

foreign import getElementById :: String -> Effect HTMLElement

propsHighlight :: This -> NodeProps -> String -> Array Props.Props
propsHighlight this _props elemId =
  [ Props.onMouseOver \event -> do

      stopPropagation event
      elem <- getElementById elemId
      st <- getState this
      -- if parent on top of stack, then unhighlight parent
      case Array.uncons st.highlights of
        Nothing -> pure unit
        Just { head: parent } -> setHighlight false parent
      -- highlight self
      setHighlight true elem
      -- push self to stack of highlights
      modifyState
        this \_ -> st { highlights = elem Array.: st.highlights }
  , Props.onMouseOut \event -> do
      stopPropagation event
      elem <- getElementById elemId
      st <- getState this
      -- unhighlight self
      setHighlight false elem
      -- pop self from stack of highlights
      highlights <- case Array.uncons st.highlights of
        Nothing -> unsafeCrashWith "expected there to be a parent on highlights stack"
        Just { tail: highlights' } -> case Array.uncons highlights' of
          Nothing -> pure []
          Just { head: parent, tail: highlights'' } -> do
            -- if parent on top of stack, then highlight it
            setHighlight true parent
            pure highlights''
      modifyState this \_ -> st { highlights = highlights }
  ]
