module Language.Shape.Stlc.Recursion.MetaContext where

import Data.Foldable
import Data.Maybe
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Typing
import Prelude
import Prim hiding (Type)
import Data.List (List)
import Data.List as List
import Data.Map.Unsafe (Map)
import Data.Map.Unsafe as Map
import Language.Shape.Stlc.Recursion.Context as Rec
import Record as R
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe as Unsafe

-- Context for metadata info such as names, constructor names, shadowing, etc.
type MetaContext
  = { typeScope :: Scope TypeID TypeName
    , termScope :: Scope TermID TermName
    , constructorTermIDs :: Map TypeID (List TermID)
    , indentation :: Int
    }

_typeScope = Proxy :: Proxy "typeScope"

_termScope = Proxy :: Proxy "termScope"

_constructorTermIDs = Proxy :: Proxy "constructorTermIDs"

_indentation = Proxy :: Proxy "indentation"

-- Recursion principles for handling the metacontext
recModule ::
  forall a.
  { module_ :: List Definition -> ModuleMetadata -> Context -> MetaContext -> a
  } ->
  Module -> Context -> MetaContext -> a
recModule rec =
  Rec.recModule
    { module_:
        \defs meta gamma ->
          rec.module_ defs meta gamma
            <<< foldl (>>>) identity
                [ registerDefinitions defs
                , incrementIndentation
                ]
    }

recBlock ::
  forall a.
  { block :: List Definition -> Term -> BlockMetadata -> Context -> Type -> MetaContext -> a
  } ->
  Block -> Context -> Type -> MetaContext -> a
recBlock rec =
  Rec.recBlock
    { block:
        \defs a meta gamma alpha ->
          rec.block defs a meta gamma alpha
            <<< foldl (>>>) identity
                [ registerDefinitions defs
                , incrementIndentation
                ]
    }

recDefinition ::
  forall a.
  { term :: TermBinding -> Type -> Term -> TermDefinitionMetadata -> Context -> MetaContext -> a
  , data :: TypeBinding -> (List Constructor) -> DataDefinitionMetadata -> Context -> MetaContext -> a
  } ->
  Definition -> Context -> MetaContext -> a
recDefinition rec def gamma = Rec.recDefinition rec def gamma <<< incrementIndentation

recConstructor ::
  forall a.
  { constructor :: TermBinding -> List Parameter -> ConstructorMetadata -> Context -> TypeBinding -> MetaContext -> a
  } ->
  Constructor -> Context -> TypeBinding -> MetaContext -> a
recConstructor rec constr gamma x = Rec.recConstructor rec constr gamma x <<< incrementIndentation

recType ::
  forall a.
  { arrow :: Parameter -> Type -> ArrowTypeMetadata -> Context -> MetaContext -> a
  , data :: TypeID -> DataTypeMetadata -> Context -> MetaContext -> a
  , hole :: HoleID -> TypeWeakening -> HoleTypeMetadata -> Context -> MetaContext -> a
  , proxyHole :: HoleID -> Context -> MetaContext -> a
  } ->
  Type -> Context -> MetaContext -> a
recType rec alpha gamma = Rec.recType rec alpha gamma <<< incrementIndentation

recTerm ::
  forall a.
  { lambda :: TermBinding -> Block -> LambdaTermMetadata -> Context -> Type -> MetaContext -> a
  , neutral :: NeutralTerm -> NeutralTermMetadata -> Context -> Type -> MetaContext -> a
  , hole :: HoleTermMetadata -> Context -> Type -> MetaContext -> a
  , match :: TypeID -> Term -> List Case -> MatchTermMetadata -> Context -> Type -> MetaContext -> a
  } ->
  Term -> Context -> Type -> MetaContext -> a
recTerm rec =
  Rec.recTerm
    { lambda:
        \x b meta gamma alpha ->
          rec.lambda x b meta gamma alpha
            <<< foldl (>>>) identity
                [ registerTermBinding x
                , incrementIndentation
                ]
    , neutral: \neu meta gamma alpha -> rec.neutral neu meta gamma alpha <<< incrementIndentation
    , hole: \meta gamma alpha -> rec.hole meta gamma alpha <<< incrementIndentation
    , match: \id a cases meta gamma alpha -> rec.match id a cases meta gamma alpha <<< incrementIndentation
    }

recNeutralTerm ::
  forall a.
  { variable :: TermID -> VariableTermMetadata -> Context -> Type -> MetaContext -> a
  , application :: NeutralTerm -> Term -> ApplicationTermMetadata -> Context -> Parameter -> Type -> MetaContext -> a
  } ->
  NeutralTerm -> Context -> Type -> MetaContext -> a
recNeutralTerm rec neu gamma alpha = Rec.recNeutralTerm rec neu gamma alpha <<< incrementIndentation

recCase ::
  forall a.
  { case_ :: List TermBinding -> Term -> CaseMetadata -> Context -> Type -> TypeID -> TermID -> MetaContext -> a } ->
  Case -> Context -> Type -> MetaContext -> a
recCase rec =
  Rec.recCase
    { case_:
        \xs a meta gamma alpha typeID constrID ->
          rec.case_ xs a meta gamma alpha typeID constrID
            <<< foldl (>>>) identity
                [ registerTermBindings xs
                , incrementIndentation
                ]
    }

recParameter ::
  forall a.
  { parameter :: Type -> ParameterMetadata -> Context -> MetaContext -> a } ->
  Parameter -> Context -> MetaContext -> a
recParameter rec prm gamma = Rec.recParameter rec prm gamma <<< incrementIndentation

-- Scope
type Scope id name
  = { names :: Map id name
    , shadows :: Map name Int
    , shadowIndices :: Map id Int
    }

_names = Proxy :: Proxy "names"

_shadows = Proxy :: Proxy "shadows"

_shadowIndices = Proxy :: Proxy "shadowIndices"

-- 1. increment name's shadow
-- 2. set id's shadow index
registerName :: forall id name. Ord id => Ord name => id -> name -> Scope id name -> Scope id name
registerName id name =
  foldl (>>>) identity
    [ \scope -> R.modify _shadowIndices (Map.insert id $ Map.lookup' name scope.shadows) scope
    , R.modify _names $ Map.insert id name
    ]

incrementIndentation :: MetaContext -> MetaContext
incrementIndentation = R.modify _indentation (_ + 1)

registerTypeBinding :: TypeBinding -> MetaContext -> MetaContext
registerTypeBinding (TypeBinding id { name }) = R.modify _typeScope $ registerName id name

registerTermBinding :: TermBinding -> MetaContext -> MetaContext
registerTermBinding (TermBinding id { name }) = R.modify _termScope $ registerName id name

registerTermBindings :: List TermBinding -> MetaContext -> MetaContext
registerTermBindings = flip $ List.foldl (flip registerTermBinding)

registerDatatype :: TypeBinding -> List TermBinding -> MetaContext -> MetaContext
registerDatatype x@(TypeBinding typeID _) constrBnds metaGamma =
  ( foldl (>>>) identity
      [ registerTypeBinding x
      , registerTermBindings constrBnds
      , R.modify _constructorTermIDs $ Map.insert typeID (map (\(TermBinding constrID _) -> constrID) constrBnds)
      ]
      metaGamma
  )

registerDefinitions :: List Definition -> MetaContext -> MetaContext
registerDefinitions defs =
  foldl (<<<) identity
    [ flip
        ( List.foldl
            ( flip
                $ case _ of
                    TermDefinition x alpha a meta ->
                      foldl (<<<) identity
                        [ registerTermBinding x
                        ]
                    DataDefintion x constrs meta ->
                      let
                        constrBnds = map (\(Constructor x _ _) -> x) constrs
                      in
                        foldl (<<<) identity
                          [ registerDatatype x constrBnds
                          ]
            )
        )
        defs
    ]
