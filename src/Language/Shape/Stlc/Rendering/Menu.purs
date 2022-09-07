module Language.Shape.Stlc.Rendering.Menu where

{-
The menu bar that appears at the top of the screen. Manages the following
functionalities:
- managing a color theme
- managing a syntax theme 
- loading pre-defined programs
- TODO: loading programs

-}
import Data.Tuple.Nested
import Language.Shape.Stlc.Transition
import Language.Shape.Stlc.Types
import Prelude
import Prim hiding (Type)
import Data.Array (singleton)
import Data.Traversable (sequence)
import Effect (Effect)
import Language.Shape.Stlc.Example.Basic as ExampleBasic
import Language.Shape.Stlc.Example.Blank as ExampleBlank
import Language.Shape.Stlc.Example.Datatypes as ExampleDatatypes
import Language.Shape.Stlc.Rendering.Token as Token
import React (ReactElement, getState, modifyState)
import React.DOM as DOM
import React.DOM.Props as Props
import React.SyntheticEvent (SyntheticMouseEvent)
import Web.HTML (window)
import Web.HTML.Window (alert)

renderMenubar :: This -> Effect (Array ReactElement)
renderMenubar this = do
  singleton
    <$> DOM.div [ Props.className "menubar" ]
    <$> sequence
        [ pure
            $ DOM.div
                [ Props.className "title" ]
                [ DOM.text "shape" ]
        , renderExternalMenu this
        -- , renderColorThemeMenu this -- TODO: implement color themes via a class that's given to things in the renderer
        , renderSyntaxThemeMenu this
        , renderExampleMenu this
        , renderResourcesMenu this
        ]

makeMenuButton ::
  String ->
  (SyntheticMouseEvent → Effect Unit) ->
  Effect ReactElement
makeMenuButton name onClick =
  pure
    $ DOM.div
        [ Props.className "menu menu-button"
        , Props.onClick onClick
        ]
        [ DOM.text name ]

makeMenu ::
  String ->
  Array (ReactElement /\ (SyntheticMouseEvent → Effect Unit)) ->
  Effect ReactElement
makeMenu name items =
  pure
    $ DOM.div
        [ Props.className "menu" ]
        [ DOM.div [ Props.className "menu-name" ]
            [ DOM.text name ]
        , DOM.div [ Props.className "menu-items" ]
            [ DOM.table' [ DOM.tbody' (makeItem <$> items) ] ]
        ]
  where
  makeItem (label /\ onClick) =
    DOM.tr'
      $ [ DOM.td
            [ Props.className "menu-item"
            , Props.onClick onClick
            ]
            [ label ]
        ]

renderColorThemeMenu :: This -> Effect ReactElement
renderColorThemeMenu this = makeMenu "color" []

renderSyntaxThemeMenu :: This -> Effect ReactElement
renderSyntaxThemeMenu this = do
  st <- getState this
  let
    makeItem name synthm =
      ( if synthm.meta.name == st.syntaxtheme.meta.name then
          DOM.b' [ DOM.text name ]
        else
          DOM.text name
      )
        /\ \_ -> modifyState this _ { syntaxtheme = synthm }
  makeMenu "syntax"
    [ makeItem "default" Token.defaultSyntaxTheme
    , makeItem "expanded" Token.expandedSyntaxTheme
    , makeItem "contracted" Token.contractedSyntaxTheme
    , makeItem "minimalist" Token.minimalistSyntaxTheme
    , makeItem "typescript" Token.typescriptSyntaxTheme
    -- TODO
    -- , makeItem "haskell" HaskellSynaxTheme
    -- , makeItem "typescript" TypescriptSyntaxTheme
    -- , makeItem "minimalist" MinimalistSyntaxTheme
    -- , makeItem "verbose" VerboseSyntaxTheme
    ]

renderExampleMenu :: This -> Effect ReactElement
renderExampleMenu this =
  makeMenu "example"
    [ makeItem "blank" ExampleBlank.program
    , makeItem "basic" ExampleBasic.program
    , makeItem "datatypes" ExampleDatatypes.program
    ]
  where
  makeItem name program =
    DOM.div [ Props.className "filename" ] [ DOM.text (name <> ".shape") ]
      /\ \event -> do
          doTransition { this, event: MouseTransitionEvent event }
            $ { label: "load example program"
              , effect: setProgram program
              }

-- modifyState this (updateStateProgram term type_)
renderExternalMenu :: This -> Effect ReactElement
renderExternalMenu this =
  makeMenu "external"
    [ DOM.text "import from file" /\ \_ -> alert "this feature is currently unimplemented" =<< window
    , DOM.text "export as file" /\ \_ -> alert "this feature is currently unimplemented" =<< window
    , DOM.text "export as blob" /\ \_ -> alert "this feature is currently unimplemented" =<< window
    , DOM.text "export selection as snippet" /\ \_ -> alert "this feature is currently unimplemented" =<< window
    ]

renderResourcesMenu :: This -> Effect ReactElement
renderResourcesMenu this =
  makeMenu "resources"
    [ makeLinkItem "repository" "https://github.com/Riib11/shape-lang-purs/blob/master/README.md"
    , makeLinkItem "documentation" "https://github.com/Riib11/shape-lang-purs/blob/master/"
    , makeLinkItem "similar projects" "https://github.com/Riib11/shape-lang-purs/blob/master/README.md#similar-projects"
    ]
  where
  makeLinkItem label href =
    DOM.div [ Props.className "link" ]
      [ DOM.a [ Props.href href ]
          [ DOM.text label ]
      ]
      /\ \_ -> pure unit
