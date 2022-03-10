module Language.Shape.Stlc.Recursion.MetaContext where

import Data.Foldable
import Data.Maybe
import Data.Tuple.Nested
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Typing
import Prelude
import Prim hiding (Type)
import Data.List (List)
import Data.List as List
import Data.Map.Unsafe (Map)
import Data.Map.Unsafe as Map
import Debug as Debug
import Language.Shape.Stlc.Recursion.Context as Rec
import Record as R
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe as Unsafe

-- Context for metadata info such as names, constructor names, shadowing, etc.
type MetaContext
  = { typeScope :: Scope TypeId TypeName
    , termScope :: Scope TermId TermName
    , constructorTermIds :: Map TypeId (List TermId)
    , indentation :: Int
    }

emptyMetaContext :: MetaContext
emptyMetaContext =
  { typeScope: emptyScope
  , termScope: emptyScope
  , constructorTermIds: Map.empty
  , indentation: 0
  }

_typeScope = Proxy :: Proxy "typeScope"

_termScope = Proxy :: Proxy "termScope"

_constructorTermIds = Proxy :: Proxy "constructorTermIds"

_indentation = Proxy :: Proxy "indentation"

-- Recursion principles for handling the metacontext
recModule ::
  forall a.
  { module_ :: List Definition -> ModuleMetadata -> Context -> MetaContext -> a
  } ->
  Module -> Context -> MetaContext -> a
recModule rec mod gamma = Rec.recModule rec mod gamma <<< incrementIndentation

recBlock ::
  forall a.
  { block :: List Definition -> Term -> BlockMetadata -> Context -> Type -> MetaContext -> a
  } ->
  Block -> Context -> Type -> MetaContext -> a
recBlock rec block gamma alpha = Rec.recBlock rec block gamma alpha <<< incrementIndentation

recDefinitions ::
  forall a.
  { definitions :: List Definition -> Context -> MetaContext -> a } ->
  List Definition -> Context -> MetaContext -> a
recDefinitions rec =
  Rec.recDefinitions
    { definitions:
        \defs gamma ->
          rec.definitions defs gamma
            <<< foldl (>>>) identity
                [ registerDefinitions defs
                , incrementIndentation
                ]
    }

recConstructor ::
  forall a.
  { constructor :: TermBinding -> List Parameter -> ConstructorMetadata -> Context -> TypeBinding -> MetaContext -> a
  } ->
  Constructor -> Context -> TypeBinding -> MetaContext -> a
recConstructor rec constr gamma x = Rec.recConstructor rec constr gamma x <<< incrementIndentation

recType ::
  forall a.
  { arrow :: Parameter -> Type -> ArrowTypeMetadata -> Context -> MetaContext -> a
  , data :: TypeId -> DataTypeMetadata -> Context -> MetaContext -> a
  , hole :: HoleID -> TypeWeakening -> HoleTypeMetadata -> Context -> MetaContext -> a
  , proxyHole :: HoleID -> Context -> MetaContext -> a
  } ->
  Type -> Context -> MetaContext -> a
recType rec alpha gamma = Rec.recType rec alpha gamma <<< incrementIndentation

recTerm ::
  forall a.
  { lambda :: TermId -> Block -> LambdaTermMetadata -> Context -> Parameter -> Type -> MetaContext -> a
  , neutral :: TermId -> Args -> NeutralTermMetadata -> Context -> Type -> MetaContext -> a
  , hole :: HoleTermMetadata -> Context -> Type -> MetaContext -> a
  , match :: TypeId -> Term -> List Case -> MatchTermMetadata -> Context -> Type -> MetaContext -> List TermId -> a
  } ->
  Term -> Context -> Type -> MetaContext -> a
recTerm rec =
  Rec.recTerm
    { lambda:
        \termId block meta gamma prm@(Parameter _ { name }) beta ->
          rec.lambda termId block meta gamma prm beta
            <<< foldl (>>>) identity
                [ registerTermId termId name
                , incrementIndentation
                ]
    , neutral: \termId args meta gamma alpha -> rec.neutral termId args meta gamma alpha <<< incrementIndentation
    , hole: \meta gamma alpha -> rec.hole meta gamma alpha <<< incrementIndentation
    , match:
        \typeId a cases meta gamma alpha metaGamma ->
          rec.match typeId a cases meta gamma alpha
            (incrementIndentation metaGamma)
            (Map.lookup' typeId metaGamma.constructorTermIds)
    }

recArgs ::
  forall a.
  { none :: a
  , cons :: Term -> Args -> ArgConsMetaData -> Context -> Parameter -> Type -> MetaContext -> a
  } ->
  Args -> Context -> Type -> MetaContext -> a
recArgs rec =
  Rec.recArgs
    { none: \_ -> rec.none
    , cons: rec.cons
    }

recCase ::
  forall a.
  { case_ :: List TermId -> Term -> CaseMetadata -> Context -> Type -> TypeId -> TermId -> MetaContext -> a } ->
  Case -> Context -> Type -> TypeId -> TermId -> MetaContext -> a
recCase rec =
  Rec.recCase
    { case_:
        \termIds a meta gamma alpha typeId termId ->
          rec.case_ termIds a meta gamma alpha typeId termId
            <<< foldl (>>>) identity
                [ undefined -- TODO: registerTermIds termIds
                , incrementIndentation
                ]
    }

recParameter ::
  forall a.
  { parameter :: Type -> ParameterMetadata -> Context -> MetaContext -> a } ->
  Parameter -> Context -> MetaContext -> a
recParameter rec =
  Rec.recParameter
    { parameter:
        \alpha meta gamma ->
          let
            _ = Debug.trace ("incrementing shadow for " <> show meta.name) identity
          in
            rec.parameter alpha meta gamma
              <<< R.modify _termScope (incrementShadow meta.name)
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
incrementShadow name = R.modify _shadows $ Map.insertWith (\i _ -> i) name 0

-- 1. set id's name
-- 2. increment name's shadow
-- 2. set id's shadow index
registerName :: forall id name. Ord id => Ord name => id -> name -> Scope id name -> Scope id name
registerName id name =
  foldl (>>>) identity
    [ R.modify _names $ Map.insert id name
    , incrementShadow name
    , \scope -> R.modify _shadowIndices (Map.insert id $ Map.lookup' name scope.shadows) scope
    ]
  where
  _ = Debug.trace "registerName"

  _ = Debug.trace id

  _ = Debug.trace name

registerId :: forall id name. Ord id => Ord name => id -> name -> Scope id name -> Scope id name
registerId id name =
  foldl (>>>) identity
    [ R.modify _shadows (Map.insertWith (\i _ -> i + 1) name 0)
    , \scope -> R.modify _shadowIndices (Map.insert id $ Map.lookup' name scope.shadows) scope
    , R.modify _names (Map.insert id name)
    ]
  where
  _ = Debug.trace "registerId"

  _ = Debug.trace id

  _ = Debug.trace name

incrementIndentation :: MetaContext -> MetaContext
incrementIndentation = R.modify _indentation (_ + 1)

registerTypeBinding :: TypeBinding -> MetaContext -> MetaContext
registerTypeBinding (TypeBinding id { name }) = R.modify _typeScope $ registerName id name

registerTermBinding :: TermBinding -> MetaContext -> MetaContext
registerTermBinding (TermBinding id { name }) = R.modify _termScope $ registerName id name

registerTermBindings :: List TermBinding -> MetaContext -> MetaContext
registerTermBindings = flip $ foldl (flip registerTermBinding)

registerTermId :: TermId -> TermName -> MetaContext -> MetaContext
registerTermId id name = R.modify _termScope $ registerId id name

registerTermIds :: List (TermId /\ TermName) -> MetaContext -> MetaContext
registerTermIds = flip $ foldl (flip $ \(id /\ name) -> registerTermId id name)

registerDatatype :: TypeBinding -> List TermBinding -> MetaContext -> MetaContext
registerDatatype x@(TypeBinding typeId _) constrBnds metaGamma =
  ( foldl (>>>) identity
      [ registerTypeBinding x
      , registerTermBindings constrBnds
      , R.modify _constructorTermIds $ Map.insert typeId (map (\(TermBinding constrID _) -> constrID) constrBnds)
      ]
      metaGamma
  )

registerDefinition :: Definition -> MetaContext -> MetaContext
registerDefinition = case _ of
  TermDefinition x alpha a meta -> registerTermBinding x
  DataDefinition x constrs meta -> registerDatatype x (map (\(Constructor x _ _) -> x) constrs)

registerDefinitions :: List Definition -> MetaContext -> MetaContext
registerDefinitions defs = flip (foldl (flip registerDefinition)) defs
