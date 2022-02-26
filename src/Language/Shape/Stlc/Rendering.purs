module Language.Shape.Stlc.Renderer where

import Data.FunctorWithIndex
import Data.Maybe
import Data.Tuple
import Language.Shape.Stlc.Context
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)
import Data.Array as Array
import Data.List as List
import Data.Map as Map
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Language.Shape.Stlc.Typing (typeOfNeutralTerm)
import Unsafe (lookup, error)

renderModule :: forall w i. Module -> HH.HTML w i
renderModule (Module defs) =
  HH.span
    [ HP.class_ (HH.ClassName "module") ]
    [ intercalateDoubleNewlines $ List.mapWithIndex (\i def -> renderDefinition def gamma) defs ]
  where
  gamma = addDefinitions defs emptyContext

renderBlock :: forall w i. Block -> BaseType -> Context -> HH.HTML w i
renderBlock (Block defs bufs neu) alpha gamma =
  HH.span
    [ HP.class_ (HH.ClassName "block") ]
    [ HH.span_ defsHTML
    , HH.span_ bufsHTML
    , renderNeutralTerm neu alpha gamma'
    ]
  where
  gamma' = addDefinitions defs gamma

  defsHTML =
    if List.length defs == 0 then
      []
    else
      [ intercalateNewlines (mapWithIndex (\i def -> renderDefinition def gamma') defs)
      , renderPunctuation "newline"
      ]

  bufsHTML =
    if List.length bufs == 0 then
      []
    else
      [ intercalateNewlines (mapWithIndex (\i buf -> renderBuffer buf gamma') bufs)
      , renderPunctuation "newline"
      ]

renderBuffer :: forall w i. NeutralTerm -> Context -> HH.HTML w i
renderBuffer neu gamma = renderTerm (NeutralTerm neu) (typeOfNeutralTerm neu gamma) gamma

renderDefinition :: forall w i. Definition -> Context -> HH.HTML w i
renderDefinition (TermDefinition name id (ArrowType prms out) (LambdaTerm ids block)) gamma =
  HH.span
    [ HP.class_ (HH.ClassName "term-definition definition") ]
    [ renderKeyword "let"
    , renderPunctuation "space"
    , renderTermBinding id gamma
    , renderPunctuation "lparen"
    , HH.span_ <<< List.toUnfoldable
        $ mapWithIndex (\i (Tuple x alpha) -> renderParameter x alpha gamma) prms
    , renderPunctuation "rparen"
    , renderPunctuation "colon"
    , renderPunctuation "space"
    , renderType
        (BaseType out)
        (addTermBinding name id (ArrowType prms out) gamma)
    , renderPunctuation "space"
    , renderPunctuation "assign"
    , renderPunctuation "space"
    , renderBlock block out gamma'
    ]
  where
  gamma' = addTermBindings (map fst prms) ids (map snd prms) gamma

renderDefinition (TermDefinition name id alpha a) gamma =
  HH.span
    [ HP.class_ (HH.ClassName "term-definition definition") ]
    [ renderKeyword "let"
    , renderPunctuation "space"
    , renderTermBinding id gamma
    , renderPunctuation "colon"
    , renderType alpha gamma
    , renderPunctuation "space"
    , renderPunctuation "assign"
    , renderPunctuation "space"
    , renderTerm a alpha gamma
    ]

renderDefinition (DataDefinition name id constrs) gamma =
  HH.span
    [ HP.class_ (HH.ClassName "data-definition definition") ]
    [ renderKeyword "data"
    , renderPunctuation "space"
    , renderTypeBinding name id gamma
    , renderPunctuation "space"
    , renderPunctuation "assign"
    , renderPunctuation "space"
    , intersperseLeftNewlines
        $ mapWithIndex
            (\i constr -> renderConstructor constr gamma)
            constrs
    ]

renderConstructor :: forall w i. Constructor -> Context -> HH.HTML w i
renderConstructor (Constructor x id prms) gamma =
  if List.length prms == 0 then
    HH.span
      [ HP.class_ (HH.ClassName "constructor") ]
      [ renderPunctuation "alt"
      , renderPunctuation "space"
      , renderTermBinding id gamma
      ]
  else
    HH.span
      [ HP.class_ (HH.ClassName "constructor") ]
      [ renderPunctuation "alt"
      , renderPunctuation "space"
      , renderTermBinding id gamma
      , renderPunctuation "lparen"
      , intercalateAlts (mapWithIndex (\i (Tuple x alpha) -> renderParameter x alpha gamma) prms)
      , renderPunctuation "rparen"
      ]

renderType :: forall w i. Type -> Context -> HH.HTML w i
renderType (ArrowType prms out) gamma =
  HH.span
    [ HP.class_ (HH.ClassName "arrow type") ]
    [ renderPunctuation "lparen"
    , intercalateCommas (mapWithIndex (\i (Tuple x alpha) -> renderParameter x alpha gamma) prms)
    , renderPunctuation "rparen"
    , renderPunctuation "space"
    , renderPunctuation "arrow"
    , renderPunctuation "space"
    , renderType (BaseType out) gamma
    ]

renderType (BaseType (DataType x)) gamma =
  HH.span
    [ HP.class_ (HH.ClassName "data type") ]
    [ renderTypeReference x gamma ]

renderType (BaseType (HoleType h w)) gamma =
  HH.span
    [ HP.class_ (HH.ClassName "hole type") ]
    [ renderHoleId ]

renderTerm :: forall w i. Term -> Type -> Context -> HH.HTML w i
renderTerm (LambdaTerm ids block) (ArrowType prms out) gamma =
  HH.span
    [ HP.class_ (HH.ClassName "lambda term") ]
    [ renderParameters prms gamma
    , renderPunctuation "space"
    , renderPunctuation "arrow"
    , renderPunctuation "space"
    , renderBlock block out gamma'
    ]
  where
  gamma' = addTermBindings (map fst prms) ids (map snd prms) gamma

renderTerm (NeutralTerm neu) (BaseType base) gamma = renderNeutralTerm neu base gamma

renderTerm _ _ _ = error "renderTerm: impossible"

renderNeutralTerm :: forall w i. NeutralTerm -> BaseType -> Context -> HH.HTML w i
renderNeutralTerm (ApplicationTerm id args) alpha gamma =
  HH.span
    [ HP.class_ (HH.ClassName "neutral term") ]
    [ renderTermId id gamma
    , HH.span_
        if List.length args == 0 then
          []
        else
          let
            Tuple prms out = case getTermIdType id gamma of
              ArrowType prms out -> Tuple prms out
              _ -> error "renderNeutralTerm: impossible"
          in
            [ renderPunctuation "lparen"
            , intercalateCommas
                $ mapWithIndex (\i (Tuple a alpha) -> renderTerm a alpha gamma)
                $ List.zip args (map snd prms)
            , renderPunctuation "rparen"
            ]
    ]

renderNeutralTerm (MatchTerm alpha a cases) beta gamma =
  HH.div
    [ HP.class_ (HH.ClassName "match") ]
    [ renderKeyword "match"
    , renderPunctuation "space"
    , renderNeutralTerm a alpha gamma
    , renderPunctuation "colon"
    , renderPunctuation "space"
    , renderType (BaseType alpha) gamma
    , renderPunctuation "space"
    , renderKeyword "with"
    , case alpha of
        DataType typeId ->
          HH.span
            [ HP.class_ (HH.ClassName "cases") ]
            [ intersperseLeftNewlines
                ( map
                    (\(Tuple idConstr case_) -> renderCase idConstr case_ beta gamma)
                    (List.zip (getTypeIdConstructorIds typeId gamma) cases)
                )
            ]
        HoleType _ _ -> HH.span_ [] -- TODO: how to display empty cases?
    ]

renderNeutralTerm HoleTerm alpha gamma =
  HH.span
    [ HP.class_ (HH.ClassName "term hole") ]
    [ HH.text "?" ]

renderCase :: forall w i. TermId -> Case -> BaseType -> Context -> HH.HTML w i
renderCase idConstr (Case ids block) alpha gamma =
  HH.span
    [ HP.class_ (HH.ClassName "case") ]
    [ renderPunctuation "alt"
    , renderPunctuation "space"
    , renderTermId idConstr gamma
    , HH.span_ case getTermIdType idConstr gamma of
        ArrowType prms out ->
          [ renderParameters prms gamma
          , renderPunctuation "space"
          , renderPunctuation "mapsto"
          , renderPunctuation "space"
          , renderBlock block alpha gamma'
          ]
          where
          gamma' = addTermBindings (map fst prms) ids (map snd prms) gamma
        _ ->
          [ renderPunctuation "space"
          , renderPunctuation "mapsto"
          , renderPunctuation "space"
          , renderBlock block alpha gamma
          ]
    ]

renderParameter :: forall w i. TermName -> Type -> Context -> HH.HTML w i
renderParameter x alpha gamma =
  HH.span
    [ HP.class_ (HH.ClassName "parameter") ]
    [ renderTermName x
    , renderPunctuation "colon"
    , renderPunctuation "space"
    , renderType alpha gamma
    ]

renderParameters :: forall w i. List.List (Tuple TermName Type) -> Context -> HH.HTML w i
renderParameters prms gamma =
  HH.span
    [ HP.class_ (HH.ClassName "parameters") ]
    [ renderPunctuation "lparen"
    , intercalateCommas $ map (\(Tuple x alpha) -> renderParameter x alpha gamma) prms
    , renderPunctuation "rparen"
    ]

renderTermBinding :: forall w i. TermId -> Context -> HH.HTML w i
renderTermBinding id gamma =
  HH.span
    [ HP.class_ (HH.ClassName "termBinding") ]
    [ renderTermId id gamma ]

renderTermName :: forall w i. TermName -> HH.HTML w i
renderTermName (TermName str) =
  HH.span
    [ HP.class_ (HH.ClassName "termName") ]
    [ HH.text str ]

renderTermId :: forall w i. TermId -> Context -> HH.HTML w i
renderTermId id gamma = renderTermName (getTermIdName id gamma)

renderTypeBinding :: forall w i. TypeName -> TypeId -> Context -> HH.HTML w i
renderTypeBinding name id gamma =
  HH.span
    [ HP.class_ (HH.ClassName "typeBinding") ]
    [ renderTypeName name ]

renderTypeReference :: forall w i. TypeId -> Context -> HH.HTML w i
renderTypeReference id gamma =
  HH.span
    [ HP.class_ (HH.ClassName "typeReference") ]
    [ renderTypeName (getTypeIdName id gamma) ]

renderTypeName :: forall w i. TypeName -> HH.HTML w i
renderTypeName (TypeName str) =
  HH.span
    [ HP.class_ (HH.ClassName "typeName") ]
    [ HH.text str ]

renderHoleId :: forall w i. HH.HTML w i
renderHoleId = HH.text "?"

keywords :: forall w i. Map.Map String (HH.HTML w i)
keywords =
  Map.fromFoldable <<< map makeKeyword
    $ [ "data"
      , "match"
      , "with"
      , "let"
      ]
  where
  makeKeyword title = Tuple title (HH.span [ HP.class_ (HH.ClassName (List.intercalate " " [ title, " keyword" ])) ] [ HH.text title ])

renderKeyword :: forall w i. String -> HH.HTML w i
renderKeyword title = lookup title keywords

punctuations :: forall w i. Map.Map String (HH.HTML w i)
punctuations =
  Map.fromFoldable
    $ ( map (uncurry makePunctuation)
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

renderPunctuation :: forall w i. String -> HH.HTML w i
renderPunctuation title = lookup title punctuations

-- intercalate
intercalate :: forall w i. List.List (HH.HTML w i) -> List.List (HH.HTML w i) -> HH.HTML w i
intercalate inter = HH.span_ <<< List.toUnfoldable <<< List.intercalate inter <<< map List.singleton

intercalateAlts = intercalate $ List.fromFoldable [ renderPunctuation "space", renderPunctuation "alt", renderPunctuation "space" ]

intercalateCommas = intercalate $ List.fromFoldable [ renderPunctuation "comma", renderPunctuation "space" ]

intercalateNewlines = intercalate $ List.fromFoldable [ renderPunctuation "newline" ]

intercalateDoubleNewlines = intercalate $ List.fromFoldable [ renderPunctuation "newline", renderPunctuation "newline" ]

intercalateSpaces = intercalate $ List.fromFoldable [ renderPunctuation "space" ]

-- intersperse
intersperseLeft :: forall w i. List.List (HH.HTML w i) -> List.List (HH.HTML w i) -> HH.HTML w i
intersperseLeft inter = HH.span_ <<< List.toUnfoldable <<< List.foldMap (\x -> inter <> (List.singleton x))

intersperseLeftSpaces = intersperseLeft $ List.fromFoldable [ renderPunctuation "space" ]

intersperseLeftNewlines = intersperseLeft $ List.fromFoldable [ renderPunctuation "newline" ]
