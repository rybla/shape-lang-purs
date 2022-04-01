module Language.Shape.Stlc.Rendering2 where

import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Syntax
import Prelude
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console as Console
import Language.Shape.Stlc.Initial as Initial
import Language.Shape.Stlc.Recursion.MetaContext (emptyMetaContext)
import Language.Shape.Stlc.Recursion.Prerender (Words)
import Language.Shape.Stlc.Recursion.Prerender as RecPrerender
import Language.Shape.Stlc.Typing (emptyContext)
import React (ReactClass, ReactElement, ReactThis, component, getState)
import React.DOM.Dynamic as DOM
import Undefined (undefined)

type ProgramProps
  = {}

type ProgramState
  = { module_ :: Module
    , ix_cursor :: DownwardIndex
    }

type ProgramGiven
  = { state :: ProgramState
    , render :: Effect ReactElement
    , componentDidMount :: Effect Unit
    }

programClass :: ReactClass ProgramProps
programClass = component "Program" programComponent

programComponent :: ReactThis ProgramProps ProgramState -> Effect ProgramGiven
programComponent this =
  pure
    { state
    , render: render <$> getState this
    , componentDidMount:
        do
          Console.log "componentDidMount"
    }
  where
  state :: ProgramState
  state =
    { module_: Initial.module_
    , ix_cursor: DownwardIndex Nil
    }

  render :: ProgramState -> ReactElement
  render st = undefined

  renderWords :: Words -> ReactElement
  renderWords ws = undefined

  renderModule :: ProgramState -> ReactElement
  renderModule st =
    renderWords
      $ RecPrerender.recModule
          undefined -- { module_: \defItems meta gamma metaGamma ixArgs transArgs -> ?a }
          st.module_
          emptyContext
          emptyMetaContext
          { ix: UpwardIndex Nil, csr: Just st.ix_cursor }
          identity
