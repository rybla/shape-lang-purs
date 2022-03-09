module Language.Shape.Stlc.Rendering where

import Data.Tuple.Nested
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.RenderingAux
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import App as App
import Data.List as List
import Data.Map.Unsafe as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple as Tuple
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Language.Shape.Stlc.Recursion.MetaContext (MetaContext)
import Language.Shape.Stlc.Recursion.MetaContext as RecMeta
import Language.Shape.Stlc.Recursion.Wrap (Wrap, WrapI)
import Language.Shape.Stlc.Recursion.Wrap as RecWrap
import Language.Shape.Stlc.Typing (Context)
import Prim.Row (class Cons)
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe as Unsafe

{-
Universal syntax interface
-}
type SyntaxComponent q m
  = H.Component q SyntaxState App.AppAction m

type SyntaxState
  = { cursor :: Boolean
    }

type SyntaxSlot q
  = H.Slot q App.AppAction Int

type SyntaxSlots q
  = ( definition :: SyntaxSlot q
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

_definition = Proxy :: Proxy "definition"

_block = Proxy :: Proxy "block"

_constructor = Proxy :: Proxy "constructor"

_type = Proxy :: Proxy "type"

_term = Proxy :: Proxy "term"

_neutralTerm = Proxy :: Proxy "neutralTerm"

_case = Proxy :: Proxy "case_"

_parameter = Proxy :: Proxy "parameter"

_typeName = Proxy :: Proxy "typeName"

_termName = Proxy :: Proxy "termName"

data SyntaxAction
  = AppAction App.AppAction
  | ModifyState (SyntaxState -> SyntaxState)

initialSyntaxState :: SyntaxState
initialSyntaxState =
  { cursor: false
  }

-- HH.HTML (H.ComponentSlot slots m SyntaxAction)
mkSyntaxComponent ::
  forall syntax q m.
  (SyntaxState -> HH.ComponentHTML SyntaxAction (SyntaxSlots q) m) ->
  SyntaxComponent q m
mkSyntaxComponent render =
  H.mkComponent
    { initialState:
        const
          { cursor: false
          }
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction =
              case _ of
                AppAction appAction -> H.raise appAction
                ModifyState modifyState -> H.modify_ modifyState
            -- , receive = Just <<< ModifyState <<< const
            }
    , render: render
    }

slotSyntax ::
  forall label slot q m _1.
  IsSymbol label =>
  Ord slot =>
  Cons label (H.Slot q App.AppAction slot) _1 (SyntaxSlots q) =>
  Proxy label ->
  slot ->
  SyntaxComponent q m ->
  H.ComponentHTML SyntaxAction (SyntaxSlots q) m
slotSyntax label i html = HH.slot label i html initialSyntaxState AppAction

{-
Universal syntax utilities
-}
classSyntax :: forall r i. SyntaxState -> String -> HP.IProp ( class ∷ String | r ) i
classSyntax st label = HP.class_ (HH.ClassName $ label <> if st.cursor then " selected" else "")

{-
Specific syntax renderers
-}
renderModule :: forall q m. Module -> Context -> MetaContext -> Wrap Module -> SyntaxComponent q m
renderModule =
  RecWrap.recModule
    { module_:
        \defs meta gamma metaGamma wrap_def_at ->
          mkSyntaxComponent \st ->
            HH.span
              [ classSyntax st "module" ]
              [ intercalateHTML (List.singleton punctuation.newline)
                  $ List.mapWithIndex
                      ( \i def ->
                          slotSyntax _definition i
                            $ (renderDefinition def gamma metaGamma (wrap_def_at i))
                      )
                      defs
              ]
    }

renderBlock :: forall q m. Block -> Context -> Type -> MetaContext -> Wrap Block -> SyntaxComponent q m
renderBlock =
  RecWrap.recBlock
    { block:
        \defs a meta gamma alpha metaGamma wrap_def_at ->
          mkSyntaxComponent \st ->
            HH.span
              [ classSyntax st "block" ]
              [ intercalateHTML (List.singleton punctuation.newline)
                  $ List.mapWithIndex
                      ( \i def ->
                          slotSyntax _definition i
                            $ renderDefinition def gamma metaGamma (wrap_def_at i)
                      )
                      defs
              ]
    }

renderDefinition :: forall q m. Definition -> Context -> MetaContext -> Wrap Definition -> SyntaxComponent q m
renderDefinition =
  RecWrap.recDefinition
    { term:
        \x alpha a meta gamma metaGamma wrap_alpha wrap_a ->
          mkSyntaxComponent \st ->
            HH.span
              [ classSyntax st "term definition" ]
              [ keyword.let_
              , punctuation.space
              , slotSyntax _termName 0 $ renderTermBinding x gamma metaGamma
              , punctuation.space
              , punctuation.colon
              , punctuation.space
              , slotSyntax _type 0 $ renderType alpha gamma metaGamma wrap_alpha
              , punctuation.space
              , punctuation.termdef
              , punctuation.space
              , slotSyntax _term 0 $ renderTerm a gamma alpha metaGamma wrap_a
              ]
    , data:
        \x constrs meta gamma metaGamma wrap_constr_at ->
          mkSyntaxComponent \st ->
            HH.span
              [ classSyntax st "data definition" ]
              [ keyword.data_
              , punctuation.space
              , slotSyntax _typeName 0 $ renderTypeBinding x gamma metaGamma
              , punctuation.space
              , punctuation.typedef
              , punctuation.space
              , intercalateHTML (List.fromFoldable [ punctuation.space, punctuation.alt, punctuation.space ])
                  ( List.mapWithIndex
                      ( \i constr ->
                          slotSyntax _constructor i $ renderConstructor constr gamma x metaGamma (wrap_constr_at i)
                      )
                      constrs
                  )
              ]
    }

renderConstructor :: forall q m. Constructor -> Context -> TypeBinding -> MetaContext -> Wrap Constructor -> SyntaxComponent q m
renderConstructor =
  RecWrap.recConstructor
    { constructor:
        \x prms meta gamma xType metaGamma wrap_prm_at ->
          mkSyntaxComponent \st ->
            HH.span [ classSyntax st "constructor" ]
              $ [ slotSyntax _termName 0 $ renderTermBinding x gamma metaGamma
                , punctuation.space
                , punctuation.colon
                , punctuation.space
                ]
              {-
              TODO:
              Should I just render the type of constructor by looking it up in
              context? The problem is that I need the types still to know their
              place in the constructor's parameters, so that typechanges are
              derived correctly.
              -}
              
              <> ( if List.length prms > 0 then
                    [ intercalateHTML (List.fromFoldable [ punctuation.space, punctuation.arrow, punctuation.space ])
                        ( List.mapWithIndex
                            ( \i prm ->
                                slotSyntax _parameter i
                                  $ renderParameter prm gamma metaGamma (wrap_prm_at i)
                            )
                            prms
                        )
                    , punctuation.space
                    , punctuation.arrow
                    ]
                  else
                    []
                )
              <> [ slotSyntax _termName 0 $ renderTypeBinding xType gamma metaGamma ]
    }

renderType :: forall q m. Type -> Context -> MetaContext -> Wrap Type -> SyntaxComponent q m
renderType =
  RecWrap.recType
    { arrow:
        \prm beta meta gamma metaGamma wrap_prm wrap_alpha ->
          mkSyntaxComponent \st ->
            HH.span
              [ classSyntax st "arrow type" ]
              [ slotSyntax _parameter 0 $ renderParameter prm gamma metaGamma wrap_prm
              , punctuation.space
              , punctuation.arrow
              , punctuation.space
              , slotSyntax _type 0 $ renderType beta gamma metaGamma wrap_alpha
              ]
    , data:
        \id meta gamma metaGamma ->
          mkSyntaxComponent \st ->
            HH.span
              [ classSyntax st "data type" ]
              [ slotSyntax _typeName 0 $ renderTypeID id gamma metaGamma
              ]
    , hole:
        \id wkn meta gamma metaGamma ->
          mkSyntaxComponent \st ->
            HH.span
              [ classSyntax st "hole type" ]
              [ HH.text "?" ]
    , proxyHole:
        \id gamma metaGamma ->
          mkSyntaxComponent \st ->
            HH.span
              [ classSyntax st "hole proxy type" ]
              [ HH.text "?" ]
    }

renderTerm :: forall q m. Term -> Context -> Type -> MetaContext -> Wrap Term -> SyntaxComponent q m
renderTerm =
  RecWrap.recTerm
    { lambda:
        \x b meta gamma beta metaGamma wrap_b ->
          mkSyntaxComponent \st ->
            HH.span
              [ classSyntax st "term lambda" ]
              [ slotSyntax _termName 0 $ renderTermBinding x gamma metaGamma
              , punctuation.lparen
              , punctuation.space
              , punctuation.mapsto
              , punctuation.space
              , slotSyntax _block 0 $ renderBlock b gamma beta metaGamma wrap_b
              , punctuation.rparen
              ]
    , neutral:
        \neu meta gamma alpha metaGamma wrap_neu ->
          mkSyntaxComponent \st ->
            HH.span
              [ classSyntax st "term neutral" ]
              [ slotSyntax _neutralTerm 0 $ renderNeutralTerm neu gamma alpha metaGamma wrap_neu ]
    , hole:
        \meta gamma alpha metaGamma ->
          mkSyntaxComponent \st ->
            HH.span
              [ classSyntax st "term hole" ]
              [ HH.text "?" ]
    , match:
        \typeID a cases meta gamma alpha metaGamma wrap_a wrap_case_at ->
          mkSyntaxComponent \st ->
            HH.span
              [ classSyntax st "term match" ]
              [ keyword.match
              , punctuation.space
              , slotSyntax _term 0 $ renderTerm a gamma (DataType typeID defaultDataTypeMetadata) metaGamma wrap_a
              , punctuation.space
              , keyword.with
              , punctuation.space
              , intercalateHTML (List.fromFoldable [ punctuation.space, punctuation.alt, punctuation.space ])
                  ( List.mapWithIndex
                      ( \i case_ ->
                          slotSyntax _case i
                            $ renderCase case_ gamma alpha metaGamma (wrap_case_at i)
                      )
                      cases
                  )
              ]
    }

renderNeutralTerm :: forall q m. NeutralTerm -> Context -> Type -> MetaContext -> Wrap NeutralTerm -> SyntaxComponent q m
renderNeutralTerm =
  RecWrap.recNeutralTerm
    { variable:
        \id meta gamma alpha metaGamma ->
          mkSyntaxComponent \st ->
            HH.span
              [ classSyntax st "neutral term variable" ]
              [ slotSyntax _termName 0 $ renderTermID id gamma metaGamma ]
    , application:
        \neu a meta gamma prm@(Parameter alpha _) beta metaGamma wrap_neu wrap_a ->
          let
            prefix /\ suffix = case a of
              LambdaTerm _ _ _ -> [ punctuation.lparen ] /\ [ punctuation.rparen ]
              NeutralTerm (ApplicationTerm _ _ _) _ -> [ punctuation.lparen ] /\ [ punctuation.rparen ]
              NeutralTerm (VariableTerm _ _) _ -> [] /\ []
              HoleTerm _ -> [] /\ []
              MatchTerm _ _ _ _ -> [ punctuation.lparen ] /\ [ punctuation.rparen ]
          in
            mkSyntaxComponent \st ->
              HH.span
                [ classSyntax st "neutral term application" ]
                $ [ slotSyntax _neutralTerm 0 $ renderNeutralTerm neu gamma (ArrowType prm beta defaultArrowTypeMetadata) metaGamma wrap_neu
                  , punctuation.space
                  ]
                <> prefix
                <> [ slotSyntax _term 0 $ renderTerm a gamma alpha metaGamma wrap_a ]
                <> suffix
    }

renderCase :: forall q m. Case -> Context -> Type -> MetaContext -> Wrap Case -> SyntaxComponent q m
renderCase =
  RecWrap.recCase
    { case_:
        \xs a meta gamma alpha typeID constrID metaGamma wrap_a ->
          mkSyntaxComponent \st ->
            HH.span
              [ classSyntax st "case" ]
              [ slotSyntax _termName 0 $ renderTermID constrID gamma metaGamma
              , intersperseLeftHTML (List.singleton punctuation.space)
                  ( List.mapWithIndex
                      ( \i x ->
                          slotSyntax _termName i
                            $ renderTermBinding x gamma metaGamma
                      )
                      xs
                  )
              , punctuation.space
              , punctuation.mapsto
              , slotSyntax _term 0 $ renderTerm a gamma alpha metaGamma wrap_a
              ]
    }

renderParameter :: forall q m. Parameter -> Context -> MetaContext -> Wrap Parameter -> SyntaxComponent q m
renderParameter =
  RecWrap.recParameter
    { parameter:
        \alpha meta gamma metaGamma wrap_alpha ->
          let
            shadowIndex = Map.lookup' meta.name metaGamma.termScope.shadows
          in
            mkSyntaxComponent \st ->
              HH.span
                [ classSyntax st "parameter" ]
                [ punctuation.lparen
                , slotSyntax _termName 0 $ renderTermName meta.name shadowIndex gamma metaGamma
                , punctuation.space
                , punctuation.colon
                , punctuation.space
                , slotSyntax _type 0 $ renderType alpha gamma metaGamma wrap_alpha
                , punctuation.rparen
                ]
    }

renderTypeBinding :: forall q m. TypeBinding -> Context -> MetaContext -> SyntaxComponent q m
renderTypeBinding (TypeBinding id meta) gamma metaGamma = renderTypeName meta.name shadowIndex gamma metaGamma
  where
  shadowIndex = Map.lookup' id metaGamma.typeScope.shadowIndices

renderTermBinding :: forall q m. TermBinding -> Context -> MetaContext -> SyntaxComponent q m
renderTermBinding (TermBinding id meta) gamma metaGamma = renderTermName meta.name shadowIndex gamma metaGamma
  where
  shadowIndex = Map.lookup' id metaGamma.termScope.shadowIndices

renderTypeID :: forall q m. TypeID -> Context -> MetaContext -> SyntaxComponent q m
renderTypeID id gamma metaGamma = renderTypeName name shadowIndex gamma metaGamma
  where
  name = Map.lookup' id metaGamma.typeScope.names

  shadowIndex = Map.lookup' id metaGamma.typeScope.shadowIndices

renderTermID :: forall q m. TermID -> Context -> MetaContext -> SyntaxComponent q m
renderTermID id gamma metaGamma = renderTermName name shadowIndex gamma metaGamma
  where
  name = Map.lookup' id metaGamma.termScope.names

  shadowIndex = Map.lookup' id metaGamma.termScope.shadowIndices

renderTypeName :: forall q m. TypeName -> Int -> Context -> MetaContext -> SyntaxComponent q m
renderTypeName (TypeName name) shadowIndex gamma metaGamma =
  let
    shadowSuffix = if shadowIndex > 0 then show shadowIndex else ""
  in
    mkSyntaxComponent \st ->
      HH.span
        [ classSyntax st "type name" ]
        [ case name of
            Just label -> HH.text $ label <> shadowSuffix
            Nothing -> HH.text "_"
        ]

renderTermName :: forall q m. TermName -> Int -> Context -> MetaContext -> SyntaxComponent q m
renderTermName (TermName name) shadowIndex gamma metaGamma =
  let
    shadowSuffix = if shadowIndex > 0 then show shadowIndex else ""
  in
    mkSyntaxComponent \st ->
      HH.span
        [ classSyntax st "term name" ]
        [ case name of
            Just label -> HH.text $ label <> shadowSuffix
            Nothing -> HH.text "_"
        ]
