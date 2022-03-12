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
import Language.Shape.Stlc.Recursion.Context as RecContext
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
type RecModule a
  = RecContext.RecModule (MetaContext -> a)

type RecModule_Module a
  = RecContext.RecModule_Module (MetaContext -> a)

recModule ::
  forall a.
  { module_ :: RecModule_Module a } ->
  RecModule a
recModule rec mod gamma = RecContext.recModule rec mod gamma <<< incrementIndentation

type RecBlock a
  = RecContext.RecBlock (MetaContext -> a)

type RecBlock_Block a
  = RecContext.RecBlock_Block (MetaContext -> a)

recBlock ::
  forall a.
  { block :: RecBlock_Block a } ->
  RecBlock a
recBlock rec block gamma alpha = RecContext.recBlock rec block gamma alpha <<< incrementIndentation

type RecDefinitions a
  = RecContext.RecDefinitions (MetaContext -> a)

type RecDefinitions_Definitions a
  = RecContext.RecDefinitions_Definitions (MetaContext -> a)

recDefinitions ::
  forall a.
  { definitions :: RecDefinitions_Definitions a } ->
  RecDefinitions a
recDefinitions rec =
  RecContext.recDefinitions
    { definitions:
        \defs gamma ->
          rec.definitions defs gamma
            <<< foldl (>>>) identity
                [ registerDefinitions defs
                , incrementIndentation
                ]
    }

recDefinition = undefined

recConstructor = undefined

{-
recConstructor ::
  forall a.
  { constructor :: TermBinding -> List Parameter -> ConstructorMetadata -> Context -> MetaContext -> TypeBinding -> a
  } ->
  Constructor -> Context -> TypeBinding -> MetaContext -> a
recConstructor rec constr gamma x = RecContext.recConstructor rec constr gamma x <<< incrementIndentation
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
recType rec alpha gamma = RecContext.recType rec alpha gamma <<< incrementIndentation

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

type RecArgs a
  = RecContext.RecArgs (MetaContext -> a)

type RecArgs_None (a :: Prim.Type)
  = RecContext.RecArgs_None a

type RecArgs_Cons a
  = RecContext.RecArgs_Cons (MetaContext -> a)

recArgs ::
  forall a.
  { none :: RecArgs_None a
  , cons :: RecArgs_Cons a
  } ->
  RecArgs a
recArgs rec =
  RecContext.recArgs
    { none: \_ -> rec.none
    , cons: \a args meta gamma prm alpha -> rec.cons a args meta gamma prm alpha <<< incrementIndentation
    }

{-
recCase ::
  forall a.
  { case_ :: List TermId -> Term -> CaseMetadata -> Context -> Type -> TypeId -> TermId -> MetaContext -> a } ->
  Case -> Context -> Type -> TypeId -> TermId -> MetaContext -> a
recCase rec =
  RecContext.recCase
    { case_:
        \termIds a meta gamma alpha typeId termId ->
          rec.case_ termIds a meta gamma alpha typeId termId
            <<< foldl (>>>) identity
                [ undefined -- TODO: registerTermIds termIds
                , incrementIndentation
                ]
    }
-}
{-
recParameter ::
  forall a.
  { parameter :: Type -> ParameterMetadata -> Context -> MetaContext -> a } ->
  Parameter -> Context -> MetaContext -> a
recParameter rec =
  RecContext.recParameter
    { parameter:
        \alpha meta gamma ->
          -- let
          --   _ = Debug.trace ("incrementing shadow for " <> show meta.name) identity
          -- in
          rec.parameter alpha meta gamma
            <<< R.modify _termScope (incrementShadow meta.name)
    }
-}
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
  let
    _ = Debug.trace "registerName"

    _ = Debug.trace id

    _ = Debug.trace name
  in
    foldl (>>>) identity
      [ R.modify _names $ Map.insert id name
      , incrementShadow name
      , \scope -> R.modify _shadowIndices (Map.insert id $ Map.lookup' name scope.shadows) scope
      ]

registerId :: forall id name. Ord id => Ord name => id -> name -> Scope id name -> Scope id name
registerId id name =
  let
    _ = Debug.trace "registerId"

    _ = Debug.trace id

    _ = Debug.trace name
  in
    foldl (>>>) identity
      [ R.modify _shadows (Map.insertWith (\i _ -> i + 1) name 0)
      , \scope -> R.modify _shadowIndices (Map.insert id $ Map.lookup' name scope.shadows) scope
      , R.modify _names (Map.insert id name)
      ]

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
