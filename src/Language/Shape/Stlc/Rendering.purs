module Language.Shape.Stlc.Renderer where

import Prelude
import Prim hiding (Type)
import App.Action (Action(..))
import App.State (Mode(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Language.Shape.Stlc.Context (Context)
import Language.Shape.Stlc.Context as Context
import Language.Shape.Stlc.Syntax (BaseType(..), Block(..), Buffer, Case(..), Constructor(..), Definition(..), HoleId, Module(..), NeutralTerm(..), Parameter(..), Term(..), TermBinding(..), TermId, TermName(..), TermReference(..), TermUniqueBinding(..), Type(..), TypeId, TypeName(..), TypeUniqueBinding(..), freshTermId, makeBaseType, makeHoleTerm, makeHoleType, makeNeutralTerm, makeTermDefinition, makeTermUniqueBinding)
import Language.Shape.Stlc.Typing as Typing
import Undefined (undefined)
import Unsafe as Unsafe

type Wrap a
  = a -> Module

renderModule :: forall cs m. Module -> H.ComponentHTML Action cs m
renderModule (Module defs meta) =
  HH.span
    [ HP.class_ (HH.ClassName "module") ]
    [ intercalateNewlines <<< flip mapWithIndex defs
        $ \i def ->
            HH.span_
              [ HH.button
                  [ HP.class_ (HH.ClassName "insertDefinition")
                  , HE.onClick \_ -> SetModule (insertDefinition i)
                  ]
                  [ HH.text "+" ]
              , HH.button
                  [ HP.class_ (HH.ClassName "deleteDefinition")
                  , HE.onClick \_ -> SetModule (deleteDefinition i)
                  ]
                  [ HH.text "-" ]
              , HH.br_
              , renderDefinition def gamma (wrapDefinition i)
              ]
    , HH.br_
    , HH.button
        [ HP.class_ (HH.ClassName "insertDefinition")
        , HE.onClick \_ -> SetModule (insertDefinition (List.length defs))
        ]
        [ HH.text "+" ]
    ]
  where
  wrapDefinition i def' = Module (Unsafe.fromJust $ List.updateAt i def' defs) meta

  gamma = Context.addDefinitionBindings defs Context.emptyContext

  deleteDefinition :: Int -> Module
  deleteDefinition i = Module (Unsafe.fromJust (List.deleteAt i defs)) meta

  insertDefinition :: Int -> Module
  insertDefinition i =
    Module
      ( Unsafe.fromJust
          $ List.insertAt i
              ( makeTermDefinition
                  (makeTermUniqueBinding (freshTermId unit))
                  (makeBaseType $ makeHoleType unit)
                  (makeNeutralTerm makeHoleTerm)
              )
              defs
      )
      meta

renderBlock :: forall w. Block -> BaseType -> Context -> Wrap Block -> HH.HTML w Action
renderBlock (Block defs bufs neu metaBlock) alpha gamma wrapBlock =
  HH.span
    [ HP.class_ (HH.ClassName "block") ]
    [ HH.span_ defsHTML
    , HH.span_ bufsHTML
    , renderNeutralTerm neu alpha gamma' wrapNeutralTerm
    ]
  where
  gamma' = Context.addDefinitionBindings defs gamma

  defsHTML =
    if List.length defs == 0 then
      []
    else
      [ intercalateNewlines (mapWithIndex (\i def -> renderDefinition def gamma' (wrapDefinition i)) defs)
      , renderPunctuation "newline"
      ]

  bufsHTML =
    if List.length bufs == 0 then
      []
    else
      [ intercalateNewlines (mapWithIndex (\i buf -> renderBuffer buf gamma' (wrapBuffer i)) bufs)
      , renderPunctuation "newline"
      ]

  wrapDefinition i def = wrapBlock $ Block (Unsafe.fromJust $ List.updateAt i def defs) bufs neu metaBlock

  wrapBuffer i buf = wrapBlock $ Block defs (Unsafe.fromJust $ List.updateAt i buf bufs) neu metaBlock

  wrapNeutralTerm neu = wrapBlock $ Block defs bufs neu metaBlock

renderBuffer :: forall w. NeutralTerm -> Context -> Wrap Buffer -> HH.HTML w Action
renderBuffer neu gamma = renderNeutralTerm neu (Typing.typeOfNeutralTerm neu gamma) gamma

renderDefinition :: forall w. Definition -> Context -> Wrap Definition -> HH.HTML w Action
renderDefinition (TermDefinition x alpha@(ArrowType prms out metaArrow) a@(LambdaTerm xs block metaLambda) metaDef) gamma wrapDefinition =
  HH.span
    [ HP.class_ (HH.ClassName "term-definition definition") ]
    [ renderKeyword "let"
    , renderPunctuation "space"
    , renderTermUniqueBinding x gamma wrapTermUniqueBinding
    , renderPunctuation "lparen"
    , HH.span_ <<< List.toUnfoldable $ mapWithIndex (\i prm -> renderParameter prm gamma (wrapParameter i)) prms
    , renderPunctuation "rparen"
    , renderPunctuation "colon"
    , renderPunctuation "space"
    , renderBaseType out (Context.addTermUniqueBinding x alpha gamma) wrapBaseType
    , renderPunctuation "space"
    , renderPunctuation "assign"
    , renderPunctuation "space"
    , renderBlock block out gamma' wrapBlock
    ]
  where
  gamma' =
    Context.addParameterBindings (List.zip xs prms)
      <<< Context.addTermUniqueBinding x alpha
      $ gamma

  wrapTermUniqueBinding x = wrapDefinition $ TermDefinition x alpha a metaDef

  wrapParameter i prm = wrapDefinition $ TermDefinition x (ArrowType (Unsafe.fromJust $ List.updateAt i prm prms) out metaArrow) a metaDef

  wrapBaseType out = wrapDefinition $ TermDefinition x (ArrowType prms out metaArrow) a metaDef

  wrapBlock block = wrapDefinition $ TermDefinition x alpha (LambdaTerm xs block metaLambda) metaDef

renderDefinition (TermDefinition x alpha a meta) gamma wrapDefinition =
  HH.span
    [ HP.class_ (HH.ClassName "term-definition definition") ]
    [ renderKeyword "let"
    , renderPunctuation "space"
    , renderTermUniqueBinding x gamma wrapTermUniqueBinding
    , renderPunctuation "colon"
    , renderType alpha gamma wrapType
    , renderPunctuation "space"
    , renderPunctuation "assign"
    , renderPunctuation "space"
    , renderTerm a alpha gamma wrapTerm
    ]
  where
  wrapTermUniqueBinding x = wrapDefinition $ TermDefinition x alpha a meta

  wrapType alpha = wrapDefinition $ TermDefinition x alpha a meta

  wrapTerm a = wrapDefinition $ TermDefinition x alpha a meta

renderDefinition (DataDefinition x constrs meta) gamma wrapDefinition =
  HH.span
    [ HP.class_ (HH.ClassName "data-definition definition") ]
    [ renderKeyword "data"
    , renderPunctuation "space"
    , renderTypeUniqueBinding x gamma wrapTypeUniqueBinding
    , renderPunctuation "space"
    , renderPunctuation "assign"
    , renderPunctuation "space"
    , intersperseLeftNewlines
        $ mapWithIndex
            (\i constr -> renderConstructor constr gamma (wrapConstructor i))
            constrs
    ]
  where
  wrapTypeUniqueBinding x = wrapDefinition $ DataDefinition x constrs meta

  wrapConstructor i constr = wrapDefinition $ DataDefinition x (Unsafe.fromJust $ List.updateAt i constr constrs) meta

renderConstructor :: forall w. Constructor -> Context -> Wrap Constructor -> HH.HTML w Action
renderConstructor constr@(Constructor x prms meta) gamma wrapConstructor =
  if List.length prms == 0 then
    HH.span
      [ HP.class_ (HH.ClassName "constructor") ]
      [ renderPunctuation "alt"
      , renderPunctuation "space"
      , renderTermUniqueBinding x gamma wrapTermUniqueBinding
      ]
  else
    HH.span
      [ HP.class_ (HH.ClassName "constructor") ]
      [ renderPunctuation "alt"
      , renderPunctuation "space"
      , renderTermUniqueBinding x gamma wrapTermUniqueBinding
      , renderPunctuation "lparen"
      , intercalateCommas (mapWithIndex (\i prm -> renderParameter prm gamma (wrapParameter i)) prms)
      , renderPunctuation "rparen"
      ]
  where
  wrapParameter i prm = wrapConstructor $ Constructor x (Unsafe.fromJust $ List.updateAt i prm prms) meta

  wrapTermUniqueBinding x = wrapConstructor $ Constructor x prms meta

renderType :: forall w. Type -> Context -> Wrap Type -> HH.HTML w Action
renderType (ArrowType prms out meta) gamma wrapType =
  HH.span
    [ HP.class_ (HH.ClassName "arrow type") ]
    [ renderPunctuation "lparen"
    , intercalateCommas (mapWithIndex (\i prm -> renderParameter prm gamma (wrapParameter i)) prms)
    , renderPunctuation "rparen"
    , renderPunctuation "space"
    , renderPunctuation "arrow"
    , renderPunctuation "space"
    , renderBaseType out gamma wrapBaseType
    ]
  where
  wrapParameter i prm = wrapType $ ArrowType (Unsafe.fromJust $ List.updateAt i prm prms) out meta

  wrapBaseType out = wrapType $ ArrowType prms out meta

renderType (BaseType alpha) gamma wrapBaseType = renderBaseType alpha gamma (wrapBaseType <<< BaseType)

renderType' :: forall w. Type -> Context -> HH.HTML w Action
renderType' (ArrowType prms out meta) gamma =
  HH.span
    [ HP.class_ (HH.ClassName "arrow type") ]
    [ renderPunctuation "lparen"
    , intercalateCommas (mapWithIndex (\i prm -> renderParameter' prm gamma) prms)
    , renderPunctuation "rparen"
    , renderPunctuation "space"
    , renderPunctuation "arrow"
    , renderPunctuation "space"
    , renderBaseType' out gamma
    ]

renderType' (BaseType alpha) gamma = renderBaseType' alpha gamma

renderBaseType :: forall w. BaseType -> Context -> Wrap BaseType -> HH.HTML w Action
renderBaseType (DataType x meta) gamma wrapBaseType =
  HH.span
    [ HP.class_ (HH.ClassName "data type") ]
    [ renderTypeId x gamma wrapTypeId ]
  where
  wrapTypeId x = wrapBaseType $ DataType x meta

renderBaseType (HoleType id wkn meta) gamma wrapBaseType =
  HH.span
    [ HP.class_ (HH.ClassName "hole type") ]
    [ renderHoleId id gamma wrapHoleId ]
  where
  wrapHoleId id = wrapBaseType $ HoleType id wkn meta

renderBaseType' :: forall w. BaseType -> Context -> HH.HTML w Action
renderBaseType' (DataType x meta) gamma =
  HH.span
    [ HP.class_ (HH.ClassName "data type") ]
    [ renderTypeId' x gamma ]

renderBaseType' (HoleType id wkn meta) gamma =
  HH.span
    [ HP.class_ (HH.ClassName "hole type") ]
    [ renderHoleId' id gamma ]

renderTerm :: forall w. Term -> Type -> Context -> Wrap Term -> HH.HTML w Action
renderTerm (LambdaTerm xs block metaLambda) (ArrowType prms out metaArrow) gamma wrapTerm =
  HH.span
    [ HP.class_ (HH.ClassName "lambda term") ]
    [ renderPunctuation "lparen"
    , intercalateCommas $ map (\prm -> renderParameter' prm gamma) prms
    , renderPunctuation "rparen"
    , renderPunctuation "space"
    , renderPunctuation "arrow"
    , renderPunctuation "space"
    , renderBlock block out gamma' wrapBlock
    ]
  where
  gamma' = Context.addParameterBindings (List.zip xs prms) gamma

  wrapBlock block = wrapTerm $ LambdaTerm xs block metaLambda

renderTerm (LambdaTerm _ _ _) _ _ _ = Unsafe.error "impossible: LambdaTerm cannot have type other than ArrowType"

renderTerm (NeutralTerm neu) (BaseType alpha) gamma wrapTerm = renderNeutralTerm neu alpha gamma (wrapTerm <<< NeutralTerm)

renderTerm (NeutralTerm _) _ _ _ = Unsafe.error "impossible: NeutralTerm cannot have type other than BaseType"

renderNeutralTerm :: forall w. NeutralTerm -> BaseType -> Context -> Wrap NeutralTerm -> HH.HTML w Action
renderNeutralTerm (ApplicationTerm x@(TermReference id _) args meta) alpha gamma wrapNeutralTerm =
  HH.span
    [ HP.class_ (HH.ClassName "neutral term") ]
    [ renderTermReference x gamma wrapTermReference
    , HH.span_
        if List.length args == 0 then
          []
        else case Context.getType id gamma of
          ArrowType prms _ _ ->
            [ renderPunctuation "lparen"
            , intercalateCommas
                $ mapWithIndex (\i (Tuple a (Parameter _ alpha _)) -> renderTerm a alpha gamma (wrapArgument i))
                $ List.zip args prms
            -- $ mapWithIndex (\i (Tuple a (Parameter _ alpha _)) -> renderTerm a alpha gamma)
            -- $ List.zip args prms
            , renderPunctuation "rparen"
            ]
          _ -> Unsafe.error "impossible: Applicant must have type ArrowType"
    ]
  where
  wrapTermReference x = wrapNeutralTerm $ ApplicationTerm x args meta

  wrapArgument i a = wrapNeutralTerm $ ApplicationTerm x (Unsafe.fromJust $ List.updateAt i a args) meta

renderNeutralTerm (MatchTerm alpha neu cases meta) beta gamma wrapNeutralTerm1 =
  HH.div
    [ HP.class_ (HH.ClassName "match") ]
    [ renderKeyword "match"
    , renderPunctuation "space"
    , renderNeutralTerm neu alpha gamma wrapNeutralTerm2
    , renderPunctuation "colon"
    , renderPunctuation "space"
    , renderBaseType alpha gamma wrapBaseType
    , renderPunctuation "space"
    , renderKeyword "with"
    , case alpha of
        DataType typeId _ ->
          HH.span
            [ HP.class_ (HH.ClassName "cases") ]
            [ intersperseLeftNewlines
                ( mapWithIndex
                    (\i (Tuple idConstr case_) -> renderCase idConstr case_ beta gamma (wrapCase i))
                    (List.zip (Context.getConstructorIds typeId gamma) cases)
                )
            ]
        HoleType _ _ _ -> HH.span_ [] -- TODO: how to display empty cases?
    ]
  where
  wrapNeutralTerm2 neu = wrapNeutralTerm1 $ MatchTerm alpha neu cases meta

  wrapBaseType alpha = wrapNeutralTerm1 $ MatchTerm alpha neu cases meta

  wrapCase i case_ = wrapNeutralTerm1 $ MatchTerm alpha neu (Unsafe.fromJust $ List.updateAt i case_ cases) meta

renderNeutralTerm (HoleTerm meta) alpha gamma wrapNeutralTerm =
  HH.span
    [ HP.class_ (HH.ClassName "term hole") ]
    [ HH.text "?" ]

renderCase :: forall w. TermId -> Case -> BaseType -> Context -> Wrap Case -> HH.HTML w Action
renderCase idConstr (Case ids block metaCase) alpha gamma wrapCase =
  HH.span
    [ HP.class_ (HH.ClassName "case") ]
    [ renderPunctuation "alt"
    , renderPunctuation "space"
    , renderTermId' idConstr gamma
    , HH.span_ case Context.getType idConstr gamma of
        ArrowType prms _ _ ->
          [ renderParameters' prms gamma
          , renderPunctuation "space"
          , renderPunctuation "mapsto"
          , renderPunctuation "space"
          , renderBlock block alpha gamma' wrapBlock
          ]
          where
          gamma' = Context.addParameterBindings (List.zip ids prms) gamma
        _ ->
          [ renderPunctuation "space"
          , renderPunctuation "mapsto"
          , renderPunctuation "space"
          , renderBlock block alpha gamma wrapBlock
          ]
    ]
  where
  wrapBlock block = wrapCase $ Case ids block metaCase

renderParameter :: forall w. Parameter -> Context -> Wrap Parameter -> HH.HTML w Action
renderParameter (Parameter name alpha meta) gamma wrapParameter =
  HH.span
    [ HP.class_ (HH.ClassName "parameter") ]
    [ renderTermName name i gamma wrapTermName
    , renderPunctuation "colon"
    , renderPunctuation "space"
    , renderType alpha gamma wrapType
    ]
  where
  wrapTermName name = wrapParameter $ Parameter name alpha meta

  wrapType alpha = wrapParameter $ Parameter name alpha meta

  i = List.length $ Context.getTermNameClash name gamma -- assumes this is where the parameter is introduced

-- phantom
renderParameter' :: forall w. Parameter -> Context -> HH.HTML w Action
renderParameter' (Parameter name alpha meta) gamma =
  HH.span
    [ HP.class_ (HH.ClassName "parameter") ]
    [ renderTermName' name i gamma
    , renderPunctuation "colon"
    , renderPunctuation "space"
    , renderType' alpha gamma
    ]
  where
  i = List.length $ Context.getTermNameClash name gamma -- assumes this is where the parameter is introduced

renderParameters :: forall w. List.List Parameter -> Context -> (Int -> Wrap Parameter) -> HH.HTML w Action
renderParameters prms gamma wrapParameter =
  HH.span
    [ HP.class_ (HH.ClassName "parameters") ]
    [ renderPunctuation "lparen"
    , HH.span_ <<< List.toUnfoldable $ mapWithIndex (\i prm -> renderParameter prm gamma (wrapParameter i)) prms
    , renderPunctuation "rparen"
    ]

-- phantom
renderParameters' :: forall w. List.List Parameter -> Context -> HH.HTML w Action
renderParameters' prms gamma =
  HH.span
    [ HP.class_ (HH.ClassName "parameters") ]
    [ renderPunctuation "lparen"
    , HH.span_ <<< List.toUnfoldable $ map (\prm -> renderParameter' prm gamma) prms
    , renderPunctuation "rparen"
    ]

renderTermUniqueBinding :: forall w. TermUniqueBinding -> Context -> Wrap TermUniqueBinding -> HH.HTML w Action
renderTermUniqueBinding (TermUniqueBinding id meta) gamma wrapTermUniqueBinding =
  HH.span
    [ HP.class_ (HH.ClassName "termUniqueBinding")
    , HE.onClick
        ( \_ ->
            Sequence
              [ LogConsole $ "SetMode EditTermName for " <> show meta.name <> "#" <> show id
              , SetMode (EditTermName \modifyTermName -> wrapTermUniqueBinding (TermUniqueBinding id meta { name = modifyTermName meta.name }))
              ]
        )
    ]
    [ renderTermId id gamma (wrapTermUniqueBinding <<< (\id -> TermUniqueBinding id meta))
    ]

renderTermBinding :: forall w. TermBinding -> Context -> Wrap TermBinding -> HH.HTML w Action
renderTermBinding (TermBinding id meta) gamma wrapTermBinding =
  HH.span
    [ HP.class_ (HH.ClassName "termBinding") ]
    [ renderTermId id gamma (wrapTermBinding <<< (\id -> TermBinding id meta)) ]

renderTermReference :: forall w. TermReference -> Context -> Wrap TermReference -> HH.HTML w Action
renderTermReference (TermReference id meta) gamma wrapTermReference =
  HH.span
    [ HP.class_ (HH.ClassName "termReference") ]
    [ renderTermId id gamma (wrapTermReference <<< (\id -> TermReference id meta)) ]

renderTermId :: forall w. TermId -> Context -> Wrap TermId -> HH.HTML w Action
renderTermId id gamma wrapTermId =
  HH.span
    [ HP.class_ (HH.ClassName "termId") ]
    [ renderTermName' (Context.getTermName id gamma) i gamma ]
  where
  i = Context.getTermNameClashIndex id gamma

-- phantom
renderTermId' :: forall w. TermId -> Context -> HH.HTML w Action
renderTermId' id gamma =
  HH.span
    [ HP.class_ (HH.ClassName "termId") ]
    [ renderTermName' (Context.getTermName id gamma) i gamma ]
  where
  i = Context.getTermNameClashIndex id gamma

renderTermName :: forall w. TermName -> Int -> Context -> Wrap TermName -> HH.HTML w Action
renderTermName (TermName str) i gamma wrapTermName =
  HH.span
    [ HP.class_ (HH.ClassName "termName") ]
    [ HH.text str
    , HH.sub_ [ HH.text (show i) ]
    ]

renderTermName IgnoreTermName i gamma wrapTermName =
  HH.span
    [ HP.class_ (HH.ClassName "termName") ]
    [ HH.text "_" ]

-- phantom
renderTermName' :: forall w. TermName -> Int -> Context -> HH.HTML w Action
renderTermName' (TermName str) i gamma =
  HH.span
    [ HP.class_ (HH.ClassName "termName") ]
    [ HH.text str
    , HH.sub_ [ HH.text (show i) ]
    ]

renderTermName' IgnoreTermName i gamma =
  HH.span
    [ HP.class_ (HH.ClassName "termName") ]
    [ HH.text "_" ]

renderTypeUniqueBinding :: forall w. TypeUniqueBinding -> Context -> Wrap TypeUniqueBinding -> HH.HTML w Action
renderTypeUniqueBinding (TypeUniqueBinding id meta) gamma wrapTypeUniqueBinding =
  HH.span
    [ HP.class_ (HH.ClassName "typeUniqueBinding") ]
    [ renderTypeId id gamma (wrapTypeUniqueBinding <<< (\id -> TypeUniqueBinding id meta)) ]

renderTypeId :: forall w. TypeId -> Context -> Wrap TypeId -> HH.HTML w Action
renderTypeId id gamma wrapTypeId =
  HH.span
    [ HP.class_ (HH.ClassName "typeId") ]
    [ renderTypeName' name i gamma ]
  where
  name = Context.getTypeName id gamma

  i = Context.getTypeNameClashIndex id gamma

-- phantom
renderTypeId' :: forall w. TypeId -> Context -> HH.HTML w Action
renderTypeId' id gamma =
  HH.span
    [ HP.class_ (HH.ClassName "typeId") ]
    [ renderTypeName' name i gamma ]
  where
  name = Context.getTypeName id gamma

  i = Context.getTypeNameClashIndex id gamma

renderTypeName :: forall w. TypeName -> Int -> Context -> Wrap TypeName -> HH.HTML w Action
renderTypeName (TypeName str) i gamma wrapTypeName =
  HH.span
    [ HP.class_ (HH.ClassName "typeName") ]
    [ HH.text str
    , HH.sub_ [ HH.text (show i) ]
    ]

renderTypeName IgnoreTypeName i gamma wrapTypeName =
  HH.span
    [ HP.class_ (HH.ClassName "typeName") ]
    [ HH.text "_" ]

renderTypeName' :: forall w. TypeName -> Int -> Context -> HH.HTML w Action
renderTypeName' (TypeName str) i gamma =
  HH.span
    [ HP.class_ (HH.ClassName "typeName") ]
    [ HH.text str
    , HH.sub_ [ HH.text (show i) ]
    ]

renderTypeName' IgnoreTypeName i gamma =
  HH.span
    [ HP.class_ (HH.ClassName "typeName") ]
    [ HH.text "_" ]

renderHoleId :: forall w. HoleId -> Context -> Wrap HoleId -> HH.HTML w Action
renderHoleId id gamma wrapHoleId =
  HH.span
    [ HP.class_ (HH.ClassName "holeId") ]
    [ HH.text "?" ]

renderHoleId' :: forall w. HoleId -> Context -> HH.HTML w Action
renderHoleId' id gamma =
  HH.span
    [ HP.class_ (HH.ClassName "holeId") ]
    [ HH.text "?" ]

keywords :: forall w. Map.Map String (HH.HTML w Action)
keywords =
  Map.fromFoldable <<< map makeKeyword
    $ [ "data"
      , "match"
      , "with"
      , "let"
      ]
  where
  makeKeyword title = Tuple title (HH.span [ HP.class_ (HH.ClassName (List.intercalate " " [ title, " keyword" ])) ] [ HH.text title ])

renderKeyword :: forall w. String -> HH.HTML w Action
renderKeyword title = Unsafe.lookup title keywords

punctuations :: forall w. Map.Map String (HH.HTML w Action)
punctuations =
  Map.fromFoldable
    $ ( map (Tuple.uncurry makePunctuation)
          $ [ Tuple "period" "."
            , Tuple "comma" ","
            , Tuple "colon" ":"
            , Tuple "lparen" "("
            , Tuple "rparen" ")"
            , Tuple "alt" "|"
            , Tuple "arrow" "->"
            , Tuple "assign" ":="
            , Tuple "mapsto" "=>"
            , Tuple "space" " "
            , Tuple "indent" "  "
            ]
      )
    <> [ Tuple "newline" HH.br_ ]
  where
  makePunctuation title punc = Tuple title (HH.span [ HP.class_ (HH.ClassName (List.intercalate " " [ title, "punctuation" ])) ] [ HH.text punc ])

renderPunctuation :: forall w. String -> HH.HTML w Action
renderPunctuation title = Unsafe.lookup title punctuations

-- intercalate
intercalate inter = HH.span_ <<< List.toUnfoldable <<< List.intercalate inter <<< map List.singleton

intercalateAlts = intercalate $ List.fromFoldable [ renderPunctuation "space", renderPunctuation "alt", renderPunctuation "space" ]

intercalateCommas = intercalate $ List.fromFoldable [ renderPunctuation "comma", renderPunctuation "space" ]

intercalateNewlines = intercalate $ List.fromFoldable [ renderPunctuation "newline" ]

intercalateDoubleNewlines = intercalate $ List.fromFoldable [ renderPunctuation "newline", renderPunctuation "newline" ]

intercalateSpaces = intercalate $ List.fromFoldable [ renderPunctuation "space" ]

-- intersperse
intersperseLeft inter = HH.span_ <<< List.toUnfoldable <<< List.foldMap (\x -> inter <> (List.singleton x))

intersperseLeftSpaces = intersperseLeft $ List.fromFoldable [ renderPunctuation "space" ]

intersperseLeftNewlines = intersperseLeft $ List.fromFoldable [ renderPunctuation "newline" ]
