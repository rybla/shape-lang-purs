module Language.Shape.Stlc.Rendering where

import Data.Tuple.Nested
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.RenderingAux
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import App.Action (setModule)
import App.Action as Action
import Data.Array as Array
import Data.List.Unsafe (List)
import Data.List.Unsafe as List
import Data.Map.Unsafe as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Symbol (class IsSymbol)
import Data.Tuple as Tuple
import Debug as Debug
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Language.Shape.Stlc.Changes (TypeChange(..))
import Language.Shape.Stlc.Recursion.MetaContext (MetaContext)
import Language.Shape.Stlc.Recursion.MetaContext as RecMeta
import Language.Shape.Stlc.Recursion.Wrap (Wrap, IndexWrap)
import Language.Shape.Stlc.Recursion.Wrap as RecWrap
import Language.Shape.Stlc.Typing (Context)
import Prim.Row (class Cons)
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe as Unsafe

{-
Universal syntax interface
-}
-- type SyntaxComponent syntax m
--   = H.Component (SyntaxQuery syntax) Unit Action.EditorAction m
type SyntaxComponent st m
  = H.Component (SyntaxQuery st) Unit Action.EditorAction m

data SyntaxAction syntax
  = EditorAction Action.EditorAction
  | ModifyStateSyntaxAction (SyntaxState syntax -> SyntaxState syntax)

data SyntaxQuery syntax a
  = ModifyStateSyntaxQuery (SyntaxState syntax -> SyntaxState syntax)

type SyntaxState syntax
  = { cursor :: Boolean
    , syntax :: syntax
    }

type SyntaxSlot syntax
  = H.Slot (SyntaxQuery syntax) Action.EditorAction Int

type SyntaxSlots
  = ( module :: SyntaxSlot Module
    , definitions :: SyntaxSlot (List Definition)
    , definition :: SyntaxSlot Definition
    , block :: SyntaxSlot Block
    , constructor :: SyntaxSlot Constructor
    , type :: SyntaxSlot Type
    , term :: SyntaxSlot Term
    , args :: SyntaxSlot Args
    , case_ :: SyntaxSlot Case
    , parameter :: SyntaxSlot Parameter
    , typeName :: SyntaxSlot TypeName
    , termName :: SyntaxSlot TermName
    )

_definitions = Proxy :: Proxy "definitions"

_definition = Proxy :: Proxy "definition"

_block = Proxy :: Proxy "block"

_constructor = Proxy :: Proxy "constructor"

_type = Proxy :: Proxy "type"

_term = Proxy :: Proxy "term"

_args = Proxy :: Proxy "args"

_case = Proxy :: Proxy "case_"

_parameter = Proxy :: Proxy "parameter"

_typeName = Proxy :: Proxy "typeName"

_termName = Proxy :: Proxy "termName"

initialSyntaxState :: forall syntax. syntax -> SyntaxState syntax
initialSyntaxState syntax =
  { cursor: false
  , syntax
  }

mkSyntaxComponent ::
  forall syntax m.
  syntax ->
  (SyntaxState syntax -> HH.ComponentHTML (SyntaxAction syntax) SyntaxSlots m) ->
  SyntaxComponent syntax m
mkSyntaxComponent syntax render =
  H.mkComponent
    { initialState: const $ initialSyntaxState syntax
    , eval:
        -- H.mkEval
        --   H.defaultEval
        --     { handleAction = handleAction
        --     , receive = Just <<< ModifyStateSyntaxAction <<< const
        --     }
        undefined
    , render: undefined -- render
    }
  where
  handleAction = case _ of
    ModifyStateSyntaxAction modifyState -> undefined --  H.modify_ modifyState
    EditorAction editorAction -> H.raise editorAction

slotSyntax ::
  forall syntax label slot m _1.
  IsSymbol label =>
  Ord slot =>
  Cons label (H.Slot (SyntaxQuery syntax) Action.EditorAction slot) _1 SyntaxSlots =>
  Proxy label ->
  slot ->
  SyntaxComponent syntax m ->
  H.ComponentHTML (SyntaxAction syntax) SyntaxSlots m
slotSyntax label i html = HH.slot label i html unit EditorAction

{-
Universal syntax utilities
-}
classSyntax :: forall r1 r2 i. { cursor :: Boolean | r1 } -> String -> HP.IProp ( class ∷ String | r2 ) i
classSyntax st label = HP.class_ (HH.ClassName $ label <> if st.cursor then " selected" else "")

{-
Render types
-}
type ModuleState
  = (Module /\ Context /\ MetaContext /\ Wrap Module)

type BlockState
  = (Block /\ Context /\ Type /\ MetaContext /\ Wrap Block)

type DefinitionsState
  = (List Definition /\ Context /\ MetaContext /\ Wrap (List Definition))

type ConstructorState
  = (Constructor /\ Context /\ TypeBinding /\ MetaContext /\ Wrap Constructor)

type SyntaxRenderer st m
  = st -> SyntaxComponent st m

{-
Specific syntax renderers
-}
renderModule :: forall q m. Module -> Context -> MetaContext -> Wrap Module -> SyntaxComponent q m
renderModule =
  RecWrap.recModule
    { module_:
        \defs meta gamma metaGamma wrap_mod wrap_defs ->
          mkSyntaxComponent \st ->
            HH.span
              [ classSyntax st "module" ]
              [ slotSyntax _definitions 0 $ renderDefinitions defs gamma metaGamma wrap_defs ]
    }

renderBlock :: forall q m. Block -> Context -> Type -> MetaContext -> Wrap Block -> SyntaxComponent q m
renderBlock =
  RecWrap.recBlock
    { block:
        \defs a meta gamma alpha metaGamma wrap_block wrap_defs wrap_a ->
          mkSyntaxComponent \st ->
            HH.span
              [ classSyntax st "block" ]
              $ ( if List.length defs > 0 then
                    [ indent meta metaGamma
                    , slotSyntax _definitions 0 $ renderDefinitions defs gamma metaGamma wrap_defs
                    , indent meta metaGamma
                    , keyword.in_
                    , slotSyntax _term 0 $ renderTerm a gamma alpha metaGamma wrap_a
                    ]
                  else
                    [ slotSyntax _term 0 $ renderTerm a gamma alpha metaGamma wrap_a ]
                )
    }

renderDefinitions :: forall q m. List Definition -> Context -> MetaContext -> Wrap (List Definition) -> SyntaxComponent q m
renderDefinitions =
  RecWrap.recDefinitions
    { definitions:
        \defs gamma metaGamma wrap_defs wrap_def_at ->
          let
            renderDefinition def wrap_def = case def of
              TermDefinition termBnd@(TermBinding termId _) alpha a meta ->
                mkSyntaxComponent \st ->
                  HH.span
                    [ classSyntax st "term definition" ]
                    [ keyword.let_
                    , punctuation.space
                    , slotSyntax _termName 0 $ renderTermId termId gamma metaGamma
                    , punctuation.space
                    , punctuation.colon
                    , punctuation.space
                    , slotSyntax _type 0 $ renderType alpha gamma metaGamma (wrap_def <<< \alpha' -> TermDefinition termBnd alpha' a meta)
                    , punctuation.space
                    , punctuation.termdef
                    , punctuation.space
                    , slotSyntax _term 0 $ renderTerm a gamma alpha metaGamma (wrap_def <<< \a' -> TermDefinition termBnd alpha a meta)
                    ]
              DataDefinition typeBnd@(TypeBinding typeId _) constrs meta ->
                mkSyntaxComponent \st ->
                  HH.span
                    [ classSyntax st "data definition" ]
                    [ keyword.data_
                    , punctuation.space
                    , slotSyntax _typeName 0 $ renderTypeId typeId gamma metaGamma
                    , punctuation.space
                    , punctuation.typedef
                    , punctuation.space
                    , intercalateHTML (List.fromFoldable [ punctuation.space, punctuation.alt, punctuation.space ])
                        ( List.mapWithIndex
                            ( \i constr ->
                                slotSyntax _constructor i $ renderConstructor constr gamma typeBnd metaGamma (wrap_def <<< \constr' -> DataDefinition typeBnd (List.updateAt' i constr' constrs) meta)
                            )
                            constrs
                        )
                    ]
          in
            mkSyntaxComponent \st ->
              HH.span
                [ classSyntax st "definitions" ]
                [ HH.button
                    [ HE.onClick \event ->
                        let
                          def = TermDefinition termBnd alpha a defaultTermDefinitionMetadata

                          termBnd = TermBinding (freshTermId unit) defaultTermBindingMetadata { name = TermName $ Just "z" }

                          alpha = HoleType (freshHoleID unit) Set.empty defaultHoleTypeMetadata

                          a = HoleTerm defaultHoleTermMetadata

                          _ = Debug.trace (wrap_defs (def List.: defs) NoChange) identity
                        in
                          EditorAction <<< setModule $ wrap_defs (def List.: defs) NoChange
                    ]
                    [ HH.text "add def" ]
                , punctuation.newline
                , intercalateHTML (List.singleton punctuation.newline)
                    $ List.mapWithIndex
                        ( \i def ->
                            slotSyntax _definition i
                              $ (renderDefinition def (wrap_def_at i))
                        )
                        defs
                ]
    }

renderConstructor :: forall q m. Constructor -> Context -> TypeBinding -> MetaContext -> Wrap Constructor -> SyntaxComponent q m
renderConstructor =
  RecWrap.recConstructor
    { constructor:
        \termBnd@(TermBinding termId _) prms meta gamma xType metaGamma wrap_constr wrap_prm_at ->
          mkSyntaxComponent \st ->
            HH.span [ classSyntax st "constructor" ]
              $ [ slotSyntax _termName 0 $ renderTermId termId gamma metaGamma
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
              <> [ slotSyntax _termName 0 $ renderTermId termId gamma metaGamma ]
    }

renderType :: forall q m. Type -> Context -> MetaContext -> Wrap Type -> SyntaxComponent q m
renderType =
  RecWrap.recType
    { arrow:
        \prm beta meta gamma metaGamma wrap_type wrap_prm wrap_alpha ->
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
        \typeId meta gamma metaGamma wrap_type ->
          mkSyntaxComponent \st ->
            HH.span
              [ classSyntax st "data type" ]
              [ slotSyntax _typeName 0 $ renderTypeId typeId gamma metaGamma
              ]
    , hole:
        \id wkn meta gamma metaGamma wrap_type ->
          mkSyntaxComponent \st ->
            HH.span
              [ classSyntax st "hole type" ]
              [ HH.text "?" ]
    , proxyHole:
        \id gamma metaGamma wrap_type ->
          mkSyntaxComponent \st ->
            HH.span
              [ classSyntax st "hole proxy type" ]
              [ HH.text "?" ]
    }

renderTerm :: forall q m. Term -> Context -> Type -> MetaContext -> Wrap Term -> SyntaxComponent q m
renderTerm =
  RecWrap.recTerm
    { lambda:
        \termId block meta gamma prm beta metaGamma wrap_term wrap_block ->
          mkSyntaxComponent \st ->
            HH.span
              [ classSyntax st "term lambda"
              -- toggle indent
              , HE.onClick
                  ( \event ->
                      let
                        _ = Debug.trace "toggle indent on lambda term:" identity

                        _ = Debug.trace (LambdaTerm termId block meta) identity

                        _ = Debug.trace "resulting module:" identity

                        -- TODO: for some reason this wrap is not replacing the term..
                        _ = Debug.trace (wrap_term (LambdaTerm termId block meta { indented = not meta.indented }) NoChange) identity
                      in
                        -- EditorAction $ Action.UpdateEditorAction (_ { module_ = wrap_term (HoleTerm defaultHoleTermMetadata) NoChange })
                        EditorAction
                          $ Action.SequenceEditorActions
                              [ Action.LiftEditorAction $ Action.LogAppAction "toggle indentation of lambda"
                              , Action.setModule $ wrap_term (LambdaTerm termId block meta { indented = not meta.indented }) NoChange
                              ]
                  )
              ]
              [ punctuation.lparen
              , slotSyntax _termName 0 $ renderTermId termId gamma metaGamma
              , punctuation.space
              , punctuation.mapsto
              , indentOrSpace meta metaGamma
              , slotSyntax _block 0 $ renderBlock block gamma beta metaGamma wrap_block
              , punctuation.rparen
              ]
    , neutral:
        \termId args meta gamma alpha metaGamma wrap_term wrap_args ->
          mkSyntaxComponent \st ->
            HH.span
              [ classSyntax st "term neutral" ]
              $ case args of
                  NoneArgs -> [ slotSyntax _termName 0 $ renderTermId termId gamma metaGamma ]
                  _ ->
                    [ slotSyntax _termName 0 $ renderTermId termId gamma metaGamma
                    , punctuation.space
                    , slotSyntax _args 0 $ renderArgs args gamma alpha metaGamma wrap_args
                    ]
    , hole:
        \meta gamma alpha metaGamma wrap_term ->
          mkSyntaxComponent \st ->
            HH.span
              [ classSyntax st "term hole" ]
              [ HH.text "?" ]
    , match:
        \typeId a cases meta gamma alpha metaGamma constrIDs wrap_term wrap_a wrap_case_at ->
          mkSyntaxComponent \st ->
            HH.span
              [ classSyntax st "term match" ]
              [ keyword.match
              , punctuation.space
              , slotSyntax _term 0 $ renderTerm a gamma (DataType typeId defaultDataTypeMetadata) metaGamma wrap_a
              , punctuation.space
              , keyword.with
              , punctuation.space
              , intercalateHTML
                  ( List.fromFoldable
                      [ indentOrSpace meta metaGamma
                      , punctuation.alt
                      , punctuation.space
                      ]
                  )
                  ( List.mapWithIndex
                      ( \i (case_ /\ constrID) ->
                          slotSyntax _case i
                            $ renderCase case_ gamma alpha typeId constrID metaGamma (wrap_case_at i)
                      )
                      (List.zip cases constrIDs)
                  )
              ]
    }

renderArgs :: forall q m. Args -> Context -> Type -> MetaContext -> Wrap Args -> SyntaxComponent q m
renderArgs =
  RecWrap.recArgs
    { none: mkSyntaxComponent \st -> HH.span_ []
    , cons:
        \a args meta gamma (Parameter alpha _) beta metaGamma wrap_args wrap_a wrap_args' ->
          mkSyntaxComponent \st ->
            HH.span
              [ classSyntax st "arg" ]
              [ slotSyntax _term 0 $ renderTerm a gamma alpha metaGamma wrap_a
              , indentOrSpace meta metaGamma
              , slotSyntax _args 0 $ renderArgs args gamma beta metaGamma wrap_args'
              ]
    }

renderCase :: forall q m. Case -> Context -> Type -> TypeId -> TermId -> MetaContext -> Wrap Case -> SyntaxComponent q m
renderCase =
  RecWrap.recCase
    { case_:
        \termIds a meta gamma alpha typeId constrID metaGamma wrap_case wrap_a ->
          mkSyntaxComponent \st ->
            HH.span
              [ classSyntax st "case" ]
              [ slotSyntax _termName 0 $ renderTermId constrID gamma metaGamma
              , intersperseLeftHTML (List.singleton punctuation.space)
                  ( List.mapWithIndex
                      ( \i termId ->
                          slotSyntax _termName i
                            $ renderTermId termId gamma metaGamma
                      )
                      termIds
                  )
              , punctuation.space
              , punctuation.mapsto
              , slotSyntax _term 0 $ renderTerm a gamma alpha metaGamma wrap_a
              ]
    }

renderParameter :: forall q m. Parameter -> Context -> MetaContext -> Wrap Parameter -> SyntaxComponent q m
renderParameter =  -- RecWrap.recParameter --   { parameter: --       \alpha meta gamma metaGamma wrap_prm wrap_alpha ->
  --         mkSyntaxComponent \st ->
  --           HH.span
  --             [ classSyntax st "parameter" ]
  --             [ punctuation.lparen
  --             , slotSyntax _termName 0 $ renderTermName meta.name gamma metaGamma
  --             , punctuation.space
  --             , punctuation.colon
  --             , punctuation.space
  --             , slotSyntax _type 0 $ renderType alpha gamma metaGamma wrap_alpha
  --             , punctuation.rparen
  --             ]
  --   }
  undefined

renderTypeId :: forall q m. TypeId -> Context -> MetaContext -> SyntaxComponent q m
renderTypeId typeId gamma metaGamma =  -- mkSyntaxComponent \st -> --   HH.span --     [ classSyntax st "typeId" ] --     $ [ case typeName of --           TypeName Nothing -> HH.text "_"
  --           TypeName (Just label) -> HH.text label
  --       ]
  --     <> if shadowIndex > 0 then [ HH.sub_ [ HH.text $ show shadowIndex ] ] else []
  undefined
  where
  -- _ = Debug.trace ("renderTypeId: " <> show typeId) identity
  typeName = Map.lookup' typeId metaGamma.typeScope.names

  shadowIndex = Map.lookup' typeId metaGamma.typeScope.shadowIndices

renderTermId :: forall syntax m. TermId -> Context -> MetaContext -> SyntaxComponent syntax m
renderTermId termId gamma metaGamma =  -- mkSyntaxComponent \st -> --   HH.span --     [ classSyntax st "termId" ] --     $ [ case termName of --           TermName Nothing -> HH.text "_" --           TermName (Just label) -> HH.text label --       ]
  --     <> if shadowIndex > 0 then [ HH.sub_ [ HH.text $ show shadowIndex ] ] else []
  undefined
  where
  -- _ = Debug.trace ("renderTermId: " <> show termId) identity
  termName = Map.lookup' termId metaGamma.termScope.names

  shadowIndex = Map.lookup' termId metaGamma.termScope.shadowIndices

renderTermName :: forall q m. TermName -> Context -> MetaContext -> SyntaxComponent q m
renderTermName termName gamma metaGamma =  -- mkSyntaxComponent \st -> --   HH.span --     [ classSyntax st "termName" ] --     $ [ case termName of --           TermName Nothing -> HH.text "_" --           TermName (Just label) -> HH.text label
  --       ]
  --     <> if shadowIndex > 0 then [ HH.sub_ [ HH.text $ show shadowIndex ] ] else []
  undefined
  where
  -- _ = Debug.trace ("renderTermName: " <> show termName) identity
  shadowIndex = Map.lookup' termName metaGamma.termScope.shadows
