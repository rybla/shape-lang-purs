module Language.Shape.Stlc.Renderer where

import Data.FunctorWithIndex
import Data.Maybe
import Data.Tuple
import Language.Shape.Stlc.Context
import Language.Shape.Stlc.Index
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
import Language.Typing (typeOfNeutralTerm)
import Partial (crashWith)
import Undefined (undefined)

fromJust' :: forall a. Partial => Maybe a -> String -> a
fromJust' (Just a) _ = a

fromJust' Nothing msg = crashWith msg

renderModule :: forall w i. Partial => Module -> HH.HTML w i
renderModule (Module defs) =
  HH.div
    [ HP.class_ (HH.ClassName "module") ]
    (List.toUnfoldable $ List.mapWithIndex (\i def -> renderDefinition def gamma (List.singleton (IndexStep_Definition i))) defs)
  where
  gamma = addDefinitions defs emptyContext

renderBlock :: forall w i. Partial => Block -> Type -> Context -> Index -> HH.HTML w i
renderBlock (Block defs bufs a) alpha gamma ix =
  HH.div
    [ HP.class_ (HH.ClassName "block") ]
    [ HH.map_ defsHTML
    , HH.map_ bufsHTML
    , renderTerm (NeutralTerm a) alpha gamma' (pushIndex ix IndexStep_Term)
    ]
  where
  gamma' = addDefinitions defs gamma

  defsHTML =
    if List.length defs == 0 then
      [ intercalateNewlines (mapWithIndex (\i def -> renderDefinition def gamma' (pushIndex ix (IndexStep_Definition i))) defs)
      , renderPunctuation "newline"
      ]
    else
      []

  bufsHTML =
    if List.length bufs == 0 then
      [ intercalateNewlines (mapWithIndex (\i buf -> renderBuffer buf gamma' (pushIndex ix (IndexStep_Buffer i))) bufs)
      , renderPunctuation "newline"
      ]
    else
      []

renderBuffer :: forall w i. Partial => NeutralTerm -> Context -> Index -> HH.HTML w i
renderBuffer neu gamma ix = renderTerm (NeutralTerm neu) (typeOfNeutralTerm neu gamma) gamma ix

renderDefinition :: forall w i. Partial => Definition -> Context -> Index -> HH.HTML w i
renderDefinition (TermDefinition name id (ArrowType prms out) a) gamma ix =
  HH.div
    [ HP.class_ (HH.ClassName "term-definition definition") ]
    [ renderKeyword "let"
    , renderPunctuation "space"
    , renderUniqueTermBinding name id gamma (pushIndex ix IndexStep_UniqueTermBinding)
    , renderPunctuation "lparen"
    , HH.map_ <<< List.toUnfoldable
        $ mapWithIndex (\i (Tuple x alpha) -> renderParameter x alpha gamma (appendIndex ix (IndexStep_Type List.: IndexStep_Parameter i List.: List.Nil))) prms
    , renderPunctuation "rparen"
    , renderPunctuation "colon"
    , renderType
        (BaseType out)
        (addUniqueTermBinding name id (ArrowType prms out) gamma)
        (pushIndex ix IndexStep_Output)
    , renderPunctuation "space"
    , renderPunctuation "assign"
    , renderPunctuation "space"
    , renderTerm a (ArrowType prms out) gamma (pushIndex ix IndexStep_Term)
    ]

renderDefinition (TermDefinition name id alpha a) gamma ix =
  HH.div
    [ HP.class_ (HH.ClassName "term-definition definition") ]
    [ renderKeyword "let"
    , renderPunctuation "space"
    , renderUniqueTermBinding name id gamma (pushIndex ix IndexStep_UniqueTermBinding)
    , renderPunctuation "colon"
    , renderType alpha gamma (pushIndex ix IndexStep_Type)
    , renderPunctuation "space"
    , renderPunctuation "assign"
    , renderPunctuation "space"
    , renderTerm a alpha gamma (pushIndex ix IndexStep_Term)
    ]

renderDefinition (DataDefinition name id constrs) gamma ix =
  HH.div
    [ HP.class_ (HH.ClassName "data-definition definition") ]
    [ renderKeyword "data"
    , renderPunctuation "space"
    , renderUniqueTypeBinding name id gamma (pushIndex ix IndexStep_UniqueTypeBinding)
    , renderPunctuation "assign"
    , intercalateAlts
        $ mapWithIndex
            (\i constr -> renderConstructor constr gamma (pushIndex ix (IndexStep_Constructor i)))
            constrs
    ]

renderConstructor :: forall w i. Partial => Constructor -> Context -> Index -> HH.HTML w i
renderConstructor (Constructor x id prms) gamma ix =
  if List.length prms == 0 then
    HH.div
      [ HP.class_ (HH.ClassName "constructor") ]
      [ renderUniqueTermBinding x id gamma (pushIndex ix IndexStep_UniqueTermBinding) ]
  else
    HH.div
      [ HP.class_ (HH.ClassName "constructor") ]
      [ renderUniqueTermBinding x id gamma (pushIndex ix IndexStep_UniqueTermBinding)
      , renderPunctuation "lparen"
      , intercalateAlts (mapWithIndex (\i (Tuple x alpha) -> renderParameter x alpha gamma (pushIndex ix (IndexStep_Parameter i))) prms)
      , renderPunctuation "rparen"
      ]

renderType :: forall w i. Partial => Type -> Context -> Index -> HH.HTML w i
renderType (ArrowType prms out) gamma ix =
  HH.div
    [ HP.class_ (HH.ClassName "arrow type") ]
    [ renderPunctuation "lparen"
    , intercalateCommas (mapWithIndex (\i (Tuple x alpha) -> renderParameter x alpha gamma (pushIndex ix (IndexStep_Parameter i))) prms)
    , renderPunctuation "rparen"
    , renderPunctuation "space"
    , renderPunctuation "arrow"
    , renderPunctuation "space"
    , renderType (BaseType out) gamma (pushIndex ix IndexStep_Output)
    ]

renderType (BaseType (DataType x)) gamma ix =
  HH.div
    [ HP.class_ (HH.ClassName "data type") ]
    [ renderTypeReference x gamma (pushIndex ix IndexStep_TypeReference) ]

renderType (BaseType (HoleType h w)) gamma ix =
  HH.div
    [ HP.class_ (HH.ClassName "hole type") ]
    [ renderHoleId ]

renderTerm :: forall w i. Partial => Term -> Type -> Context -> Index -> HH.HTML w i
renderTerm (LambdaTerm xs block) alpha gamma ix =
  HH.div
    [ HP.class_ (HH.ClassName "lambda term") ]
    [ renderPunctuation "lparen"
    , intercalateCommas (mapWithIndex (\i x -> renderTermBinding x gamma' (pushIndex ix (IndexStep_TermBinding i))) xs)
    , renderPunctuation "rparen"
    , renderPunctuation "space"
    , renderPunctuation "arrow"
    , renderBlock block alpha gamma' (pushIndex ix IndexStep_Block)
    ]
  where
  gamma' = undefined

renderTerm (NeutralTerm (ApplicationTerm id args)) alpha gamma ix =
  HH.div
    [ HP.class_ (HH.ClassName "neutral term") ]
    [ renderTermReference id gamma (pushIndex ix IndexStep_TermReference)
    , let
        (ArrowType params out) = fromJust' (Map.lookup id gamma.termIdType) "renderTerm:NeutralTerm"
      in
        HH.map_
          if List.length args == 0 then
            []
          else
            [ renderPunctuation "lparen"
            , intercalateCommas $ mapWithIndex (\i arg -> renderTerm arg (case fromJust' (params List.!! i) "renderTerm" of Tuple x alpha -> alpha) gamma (pushIndex ix (IndexStep_Parameter i))) args
            , renderPunctuation "rparen"
            ]
    ]

renderTerm (NeutralTerm HoleTerm) alpha gamma ix =
  HH.div
    [ HP.class_ (HH.ClassName "hole term") ]
    [ renderHoleId ]

renderParameter :: forall w i. Partial => TermName -> Type -> Context -> Index -> HH.HTML w i
renderParameter x alpha gamma ix =
  HH.div
    [ HP.class_ (HH.ClassName "parameter") ]
    [ renderTermLabel x gamma (pushIndex ix IndexStep_TermLabel)
    , renderPunctuation "colon"
    , renderPunctuation "space"
    , renderType alpha gamma (pushIndex ix IndexStep_Type)
    ]

renderUniqueTermBinding :: forall w i. TermName -> TermId -> Context -> Index -> HH.HTML w i
renderUniqueTermBinding x id gamma ix =
  HH.div
    [ HP.class_ (HH.ClassName "uniqueTermBinding") ]
    [ renderTermName x ]

renderTermBinding :: forall w i. Partial => TermId -> Context -> Index -> HH.HTML w i
renderTermBinding id gamma ix =
  HH.div
    [ HP.class_ (HH.ClassName "termBinding") ]
    [ renderTermId id gamma ]

renderTermReference :: forall w i. Partial => TermId -> Context -> Index -> HH.HTML w i
renderTermReference id gamma ix =
  HH.div
    [ HP.class_ (HH.ClassName "termReference") ]
    [ renderTermId id gamma ]

renderTermLabel :: forall w i. TermName -> Context -> Index -> HH.HTML w i
renderTermLabel name gamma ix =
  HH.div
    [ HP.class_ (HH.ClassName "termLabel") ]
    [ renderTermName name ]

renderTermName :: forall w i. TermName -> HH.HTML w i
renderTermName (TermName str) = HH.text str

renderTermId :: forall w i. Partial => TermId -> Context -> HH.HTML w i
renderTermId id gamma = renderTermName (fromJust' (Map.lookup id gamma.termIdName) ("renderTermId: " <> show id))

renderUniqueTypeBinding :: forall w i. TypeName -> TypeId -> Context -> Index -> HH.HTML w i
renderUniqueTypeBinding name id gamma ix =
  HH.div
    [ HP.class_ (HH.ClassName "uniqueTypeBinding") ]
    [ renderTypeName name ]

renderTypeReference :: forall w i. Partial => TypeId -> Context -> Index -> HH.HTML w i
renderTypeReference id gamma ix =
  HH.div
    [ HP.class_ (HH.ClassName "typeReference") ]
    [ renderTypeName (fromJust' (Map.lookup id gamma.typeIdName) "renderTypeReference") ]

renderTypeName :: forall w i. TypeName -> HH.HTML w i
renderTypeName (TypeName str) = HH.text str

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
  makeKeyword title = Tuple title (HH.div [ HP.class_ (HH.ClassName (List.intercalate " " [ title, " keyword" ])) ] [ HH.text title ])

renderKeyword :: forall w i. Partial => String -> HH.HTML w i
renderKeyword title = fromJust' (Map.lookup title keywords) ("renderKeyword: " <> title)

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
  makePunctuation title punc = Tuple title (HH.div [ HP.class_ (HH.ClassName (List.intercalate " " [ title, "punctuation" ])) ] [ HH.text punc ])

renderPunctuation :: forall w i. Partial => String -> HH.HTML w i
renderPunctuation title = fromJust' (Map.lookup title punctuations) "renderPunctuation"

intercalateAlts :: forall w i. Partial => List.List (HH.HTML w i) -> HH.HTML w i
intercalateAlts = makeIntercalater $ List.fromFoldable [ renderPunctuation "space", renderPunctuation "alt", renderPunctuation "space" ]

intercalateCommas :: forall w i. Partial => List.List (HH.HTML w i) -> HH.HTML w i
intercalateCommas = makeIntercalater $ List.fromFoldable [ renderPunctuation "comma", renderPunctuation "space" ]

intercalateNewlines :: forall w i. Partial => List.List (HH.HTML w i) -> HH.HTML w i
intercalateNewlines = makeIntercalater $ List.fromFoldable [ renderPunctuation "newline" ]

makeIntercalater :: forall w i. Partial => List.List (HH.HTML w i) -> List.List (HH.HTML w i) -> HH.HTML w i
makeIntercalater inter = HH.map_ <<< List.toUnfoldable <<< List.intercalate inter <<< map List.singleton
