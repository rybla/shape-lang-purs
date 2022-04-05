module Language.Shape.Stlc.Recursion.MetaContext where

import Data.Foldable
import Data.Maybe
import Data.Tuple.Nested
import Effect.Ref as Ref
import Effect.Ref (Ref)
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Typing
import Prelude
import Prim hiding (Type)
import Data.List (List(..), take)
import Data.List as List
import Data.Map.Unsafe (Map)
import Data.Map.Unsafe as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (fst)
import Debug as Debug
import Effect.Unsafe (unsafePerformEffect)
import Language.Shape.Stlc.Recursion.Context as RecContext
import Record as Record
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe as Unsafe

-- Context for metadata info such as names, constructor names, shadowing, etc.
type MetaContext
  = { typeScope :: Scope TypeId TypeName
    , termScope :: Scope TermId TermName
    , typeHoleIds :: Ref (List HoleId)
    , indentation :: Int
    }

emptyMetaContext :: MetaContext
emptyMetaContext =
  { typeScope: emptyScope
  , termScope: emptyScope
  , typeHoleIds: unsafePerformEffect $ Ref.new Nil
  , indentation: 0
  }

_typeScope = Proxy :: Proxy "typeScope"

_termScope = Proxy :: Proxy "termScope"

_constructorTermIds = Proxy :: Proxy "constructorTermIds"

_typeHoleIds = Proxy :: Proxy "typeHoleIds"

_indentation = Proxy :: Proxy "indentation"

-- Recursion principles for handling the metacontext
type RecModule a
  = RecContext.RecModule (MetaContext -> a)

type RecModule_Module a
  = RecContext.RecModule_Module (MetaContext -> a)

recModule ::
  forall a.
  { module_ :: RecModule_Module a } ->
  RecModule a
-- recModule rec mod gamma = RecContext.recModule rec mod gamma <<< incrementIndentation
recModule = RecContext.recModule

type RecBlock a
  = RecContext.RecBlock (MetaContext -> a)

type RecBlock_Block a
  = RecContext.RecBlock_Block (MetaContext -> a)

recBlock ::
  forall a.
  { block :: RecBlock_Block a } ->
  RecBlock a
recBlock rec block@(Block defItems _ _) gamma alpha =
  RecContext.recBlock rec block gamma alpha
    <<< (if not (List.null defItems) then incrementIndentation else identity)

type RecDefinitionItems a
  = RecContext.RecDefinitionItems (MetaContext -> a)

type RecDefinitionItems_DefinitionItems a
  = RecContext.RecDefinitionItems_DefinitionItems (MetaContext -> a)

recDefinitionItems ::
  forall a.
  { definitionItems :: RecDefinitionItems_DefinitionItems a } ->
  RecDefinitionItems a
recDefinitionItems rec =
  RecContext.recDefinitionItems
    { definitionItems:
        \defItems gamma ->
          rec.definitionItems defItems gamma
            <<< foldl (>>>) identity
                [ registerDefinitions (fromItem <$> defItems)
                ]
    }

type RecDefinition a
  = RecContext.RecDefinition (MetaContext -> a)

type RecDefinition_TermDefinition a
  = RecContext.RecDefinition_TermDefinition ({ metaGamma_self :: MetaContext, metaGamma_children :: MetaContext } -> a)

type RecDefinition_DataDefinition a
  = RecContext.RecDefinition_DataDefinition ({ metaGamma_self :: MetaContext, metaGamma_children :: MetaContext } -> a)

-- registration already handled by recDefinitionItems
recDefinition ::
  forall a.
  { term :: RecDefinition_TermDefinition a
  , data :: RecDefinition_DataDefinition a
  } ->
  RecDefinition a
recDefinition rec =
  RecContext.recDefinition
    { term:
        \termBinding type_ term meta gamma metaGamma ->
          rec.term termBinding type_ term meta gamma
            { metaGamma_self: metaGamma
            , metaGamma_children: incrementIndentation metaGamma
            }
    , data:
        \typeBinding cases meta gamma metaGamma ->
          rec.data typeBinding cases meta gamma
            { metaGamma_self: metaGamma
            , metaGamma_children: incrementIndentation metaGamma
            }
    }

type RecConstructor a
  = RecContext.RecConstructor (MetaContext -> a)

type RecConstructor_Constructor a
  = RecContext.RecConstructor_Constructor (MetaContext -> (Int -> MetaContext) -> a)

-- registration already handled by recDefinitionItems
recConstructor ::
  forall a.
  { constructor :: RecConstructor_Constructor a } ->
  RecConstructor a
recConstructor rec =
  RecContext.recConstructor
    { constructor:
        \termBinding params meta typeId gamma alpha metaGamma ->
          rec.constructor termBinding params meta typeId gamma alpha metaGamma
            (\i -> foldl (\metaGamma' (Parameter _ { name }) -> registerParameterName name metaGamma') metaGamma (fst <$> take (i + 1) params))
    }

type RecDefinitionBindings a
  = RecContext.RecDefinitionBindings (MetaContext -> a)

type RecDefinitionBindings_ArrowLambda a
  = RecContext.RecDefinitionBindings_ArrowLambda (MetaContext -> a)

type RecDefinitionBindings_Wildcard a
  = RecContext.RecDefinitionBindings_Wildcard (MetaContext -> a)

{-
recDefinitionBindings ::
  forall a.
  { arrow_lambda :: RecDefinitionBindings_ArrowLambda a
  , wildcard :: RecDefinitionBindings_Wildcard a
  } ->
  RecDefinitionBindings a
recDefinitionBindings rec =
  RecContext.recDefinitionBindings
    { arrow_lambda:
        \param@(Parameter _ { name }) beta termId block meta gamma ->
          rec.arrow_lambda param beta termId block meta gamma
            <<< foldl (>>>) identity
                [ registerTermId termId name
                , incrementIndentation
                ]
    , wildcard: rec.wildcard
    }
-}
type RecType a
  = RecContext.RecType (MetaContext -> a)

type RecType_Arrow a
  = RecContext.RecType_Arrow (MetaContext -> a)

type RecType_Data a
  = RecContext.RecType_Data (MetaContext -> a)

type RecType_Hole a
  = RecContext.RecType_Hole (MetaContext -> a)

type RecType_ProxyHole a
  = RecContext.RecType_ProxyHole (MetaContext -> a)

recType ::
  forall a.
  { arrow :: RecType_Arrow a
  , data :: RecType_Data a
  , hole :: RecType_Hole a
  , proxyHole :: RecType_ProxyHole a
  } ->
  RecType a
recType rec =
  RecContext.recType
    { arrow:
        \param@(Parameter _ { name }) beta meta gamma ->
          rec.arrow param beta meta gamma
            <<< foldl (>>>) identity
                [ registerParameterName name
                , incrementIndentation
                ]
    , data: rec.data
    , hole:
        \holeId wkn meta gamma ->
          rec.hole holeId wkn meta gamma
            <<< registerHoleId holeId
    , proxyHole:
        \holeId gamma ->
          rec.proxyHole holeId gamma
            <<< registerHoleId holeId
    }

type RecTerm a
  = RecContext.RecTerm (MetaContext -> a)

type RecTerm_Lambda a
  = RecContext.RecTerm_Lambda (MetaContext -> a)

type RecTerm_Neutral a
  = RecContext.RecTerm_Neutral (MetaContext -> a)

type RecTerm_Match a
  = RecContext.RecTerm_Match (MetaContext -> List TermId -> a)

type RecTerm_Hole a
  = RecContext.RecTerm_Hole (MetaContext -> a)

recTerm ::
  forall a.
  { lambda :: RecTerm_Lambda a
  , neutral :: RecTerm_Neutral a
  , match :: RecTerm_Match a
  , hole :: RecTerm_Hole a
  } ->
  RecTerm a
recTerm rec =
  RecContext.recTerm
    { lambda:
        \termId block meta gamma param@(Parameter _ { name }) beta ->
          rec.lambda termId block meta gamma param beta
            <<< foldl (>>>) identity
                [ registerTermId termId name
                , incrementIndentation
                ]
    , neutral: \termId argItems meta gamma alpha -> rec.neutral termId argItems meta gamma alpha <<< incrementIndentation
    , hole: \meta gamma alpha -> rec.hole meta gamma alpha <<< incrementIndentation
    , match:
        \typeId a cases meta gamma alpha metaGamma ->
          rec.match typeId a cases meta gamma alpha
            (incrementIndentation metaGamma)
            (lookupConstructorIds typeId gamma)
    }

type RecArgItems a
  = RecContext.RecArgItems (MetaContext -> a)

type RecArgItems_Nil (a :: Prim.Type)
  = RecContext.RecArgItems_Nil (MetaContext -> a)

type RecArgItems_Cons a
  = RecContext.RecArgItems_Cons (MetaContext -> a)

recArgItems ::
  forall a.
  { nil :: RecArgItems_Nil a
  , cons :: RecArgItems_Cons a
  } ->
  RecArgItems a
recArgItems rec =
  RecContext.recArgItems
    { nil: rec.nil
    , cons: \argItem argItems gamma param alpha -> rec.cons argItem argItems gamma param alpha <<< incrementIndentation
    }

type RecCase a
  = RecContext.RecCase (MetaContext -> a)

type RecCase_Case a
  = RecContext.RecCase_Case (MetaContext -> a)

recCase ::
  forall a.
  { case_ :: RecCase_Case a } ->
  RecCase a
recCase rec =
  RecContext.recCase
    { case_:
        \termIdItems block meta typeId constrId gamma alpha ->
          let
            params /\ _ = flattenArrowType $ lookupTyping constrId gamma
          in
            rec.case_ termIdItems block meta typeId constrId gamma alpha
              <<< foldl (>>>) identity
                  [ registerTermIds (List.zip (fromItem <$> termIdItems) (map (\(Parameter _ { name }) -> name) params))
                  , incrementIndentation
                  ]
    }

type RecParameter a
  = RecContext.RecParameter (MetaContext -> a)

type RecParameter_Parameter a
  = RecContext.RecParameter_Parameter ({ metaGamma_self :: MetaContext, metaGamma_children :: MetaContext } -> a)

-- `recParameter` doesn't need to `registerParameterName` because recType
-- already does. This is because the updated `metaGamma` needs to be passed to
-- `beta` of `ArrowType`.
recParameter ::
  forall a.
  { parameter :: RecParameter_Parameter a } ->
  RecParameter a
recParameter rec =
  RecContext.recParameter
    { parameter:
        \alpha meta gamma metaGamma ->
          rec.parameter alpha meta gamma
            { metaGamma_self: metaGamma
            , metaGamma_children: foldl (>>>) identity [ incrementIndentation, registerParameterName meta.name ] metaGamma
            }
    }

-- Scope
type Scope id name
  = { names :: Map id name
    , shadows :: Map name Int
    , shadowIndices :: Map id Int
    }

_names = Proxy :: Proxy "names"

_shadows = Proxy :: Proxy "shadows"

_shadowIndices = Proxy :: Proxy "shadowIndices"

emptyScope :: forall id name. Scope id name
emptyScope =
  { names: Map.empty
  , shadows: Map.empty
  , shadowIndices: Map.empty
  }

incrementShadow :: forall id name. Ord name => name -> Scope id name -> Scope id name
incrementShadow name = Record.modify _shadows $ Map.insertWith (\i _ -> i + 1) name 0

-- set id's name 
-- increment name's shadow
-- set id's shadow index
registerId :: forall id name. Show name => Ord id => Ord name => id -> name -> Scope id name -> Scope id name
registerId id name =
  foldl (>>>) identity
    [ Record.modify _names (Map.insert id name)
    , incrementShadow name
    , \scope -> Record.modify _shadowIndices (Map.insert id $ Map.lookup' name scope.shadows) scope
    ]

incrementIndentation :: MetaContext -> MetaContext
incrementIndentation = Record.modify _indentation (_ + 1)

registerTypeBinding :: TypeBinding -> MetaContext -> MetaContext
registerTypeBinding (TypeBinding id { name }) = Record.modify _typeScope $ registerId id name

registerTermBinding :: TermBinding -> MetaContext -> MetaContext
registerTermBinding (TermBinding id { name }) = Record.modify _termScope $ registerId id name

registerTermBindings :: List TermBinding -> MetaContext -> MetaContext
registerTermBindings = flip $ foldl (flip registerTermBinding)

registerParameterName :: TermName -> MetaContext -> MetaContext
registerParameterName name = Record.modify _termScope $ incrementShadow name

registerTermId :: TermId -> TermName -> MetaContext -> MetaContext
registerTermId id name = Record.modify _termScope $ registerId id name

registerTermIds :: List (TermId /\ TermName) -> MetaContext -> MetaContext
registerTermIds = flip $ foldl (flip $ \(id /\ name) -> registerTermId id name)

registerDatatype :: TypeBinding -> List TermBinding -> MetaContext -> MetaContext
registerDatatype x@(TypeBinding typeId _) constrBnds metaGamma =
  ( foldl (>>>) identity
      [ registerTypeBinding x
      , registerTermBindings constrBnds
      -- , Record.modify _constructorTermIds $ Map.insert typeId (map (\(TermBinding constrID _) -> constrID) constrBnds)
      ]
      metaGamma
  )

registerDefinition :: Definition -> MetaContext -> MetaContext
registerDefinition = case _ of
  TermDefinition x alpha a meta -> registerTermBinding x
  DataDefinition x constrItems meta -> registerDatatype x (map (\(Constructor x _ _) -> x) (fromItem <$> constrItems))

registerDefinitions :: List Definition -> MetaContext -> MetaContext
registerDefinitions def = flip (foldl (flip registerDefinition)) def

registerHoleId :: HoleId -> MetaContext -> MetaContext
registerHoleId holeId metaGamma =
  unsafePerformEffect
    $ do
        Ref.modify_
          ( \typeHoleIds ->
              if elem holeId typeHoleIds then
                typeHoleIds
              else
                Cons holeId typeHoleIds
          )
          metaGamma.typeHoleIds
        pure metaGamma

-- typeHoleIds <- Ref.read metaGamma.typeHoleIds
-- undefined
-- let typeHoleIds = Record.ge
-- Record.modify _typeHoleIds
--   (\typeHoleIds -> if elem holeId typeHoleIds then typeHoleIds else Cons holeId typeHoleIds)
