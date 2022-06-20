module Language.Shape.Stlc.Rendering.Highlight where

import Data.Array
import Data.Tuple
import Data.Tuple.Nested
import Language.Shape.Stlc.ChAtIndex
import Language.Shape.Stlc.Changes
import Language.Shape.Stlc.Hole
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Metacontext
import Language.Shape.Stlc.Recursor.Action
import Language.Shape.Stlc.Recursor.Index
import Language.Shape.Stlc.Rendering.Token
import Language.Shape.Stlc.Rendering.Types
import Language.Shape.Stlc.Rendering.Utilities
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Control.Monad.State (State)
import Control.Monad.State as State
import Data.Array as Array
import Data.Default (default)
import Data.Foldable (foldM)
import Data.List.Unsafe (List(..), reverse)
import Data.List.Unsafe as List
import Data.Map.Unsafe as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.OrderedSet (OrderedSet)
import Data.OrderedSet as OrderedSet
import Data.Set as Set
import Data.String (joinWith)
import Data.Traversable (sequence)
import Debug as Debug
import Effect (Effect)
import Effect.Console as Console
import Language.Shape.Stlc.Context (Context(..))
import Language.Shape.Stlc.CopyPasteBackend (changesBetweenContexts, fitsInHole)
import Language.Shape.Stlc.Metadata (Name(..))
import Language.Shape.Stlc.Recursor.Action as Rec
import Language.Shape.Stlc.Recursor.Context as RecCtx
import Language.Shape.Stlc.Recursor.Index as RecIx
import Language.Shape.Stlc.Recursor.Metacontext as RecMeta
import Language.Shape.Stlc.Types (Action(..), This)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Union)
import React (ReactElement, getState, modifyState)
import React.DOM as DOM
import React.DOM.Props as Props
import React.SyntheticEvent (NativeEventTarget, shiftKey, stopPropagation, target)
import Record as Record
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe (fromJust)
import Web.Event.Event (Event)
import Web.HTML (HTMLElement)

foreign import setHighlight :: Boolean -> HTMLElement -> Effect Unit

foreign import getElementById :: String -> Effect HTMLElement

propsHighlight :: This -> NodeProps -> String -> Array Props.Props
propsHighlight this props elemId =
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
        this \st -> st { highlights = elem : st.highlights }
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
      modifyState this \st -> st { highlights = highlights }
  ]
