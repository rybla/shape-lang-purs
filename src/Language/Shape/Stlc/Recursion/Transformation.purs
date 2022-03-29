module Language.Shape.Stlc.Recursion.Transformation where

import Data.Either
import Data.Maybe
import Data.Tuple.Nested
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Typing
import Prelude
import Prim hiding (Type)
import Record
import Control.Monad.State (State, runState)
import Data.Foldable (foldl)
import Data.List.Unsafe as List
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Debug as Debug
import Language.Shape.Stlc.ChangeAtIndex (Change(..), chAtModule)
import Language.Shape.Stlc.Changes (TypeChange(..))
import Language.Shape.Stlc.Changes as Ch
import Language.Shape.Stlc.Holes (HoleSub, subModule)
import Language.Shape.Stlc.Recursion.Index as Rec
import Language.Shape.Stlc.Recursion.MetaContext (MetaContext)
import Language.Shape.Stlc.RenderingTypes (Environment)
import Language.Shape.Stlc.RenderingTypes as RenderingTypes
import Prim.Row (class Union)
import Record.Unsafe.Union (unsafeUnion)
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe as Unsafe

_gamma = Proxy :: Proxy "gamma"

_ix = Proxy :: Proxy "ix"

_type_ = Proxy :: Proxy "type_"

_syntax = Proxy :: Proxy "syntax"

_change = Proxy :: Proxy "change"

_typeChange = Proxy :: Proxy "typeChange"

-- transformations only operate on the prestate
type Transformation
  = forall r. RenderingTypes.Prestate r -> Maybe (RenderingTypes.Prestate r)

type ModuleTransformation
  = { gamma :: Context
    , ix :: DownwardIndex
    , syntax :: Syntax
    , change :: Change
    }

type TypeTransformation
  = { gamma :: Context
    , ix :: DownwardIndex
    , type_ :: Type
    , typeChange :: TypeChange
    }

makeModuleTransformation :: ModuleTransformation -> Transformation
makeModuleTransformation args st = do
  (module_ /\ ix' /\ holeSub) <- chAtModule st.module_ args.gamma args.syntax args.change args.ix
  let
    module' = subModule holeSub module_
  pure st { module_ = module', ix_cursor = ix' }

makeTypeTransformation :: TypeTransformation -> Transformation
makeTypeTransformation args =
  makeModuleTransformation
    $ union
        { syntax: SyntaxType args.type_
        , change: ChangeTypeChange args.typeChange
        }
        (delete _type_ $ delete _typeChange args)

type RecModule a
  = Rec.RecModule (a)

type RecModule_Module a
  = Rec.RecModule_Module ({} -> a)

recModule ::
  forall a.
  { module_ :: RecModule_Module a } ->
  RecModule a
recModule rec =
  Rec.recModule
    { module_: undefined
    }

type RecBlock a
  = Rec.RecBlock (a)

type RecBlock_Block a
  = Rec.RecBlock_Block ({} -> a)

recBlock ::
  forall a.
  { block :: RecBlock_Block a } ->
  RecBlock a
recBlock rec =
  Rec.recBlock
    { block:
        undefined
    }

type RecDefinitionItems a
  = Rec.RecDefinitionItems (a)

type RecDefinitionItems_DefinitionItems a
  = Rec.RecDefinitionItems_DefinitionItems ({} -> a)

recDefinitionItems ::
  forall a.
  { definitionItems :: RecDefinitionItems_DefinitionItems a } ->
  RecDefinitionItems a
recDefinitionItems rec =
  Rec.recDefinitionItems
    { definitionItems:
        undefined
    }

type RecDefinitionSeparator a
  = Rec.RecDefinitionSeparator a

type RecDefinitionSeparator_Separator a
  = Rec.RecDefinitionSeparator_Separator ({} -> a)

recDefinitionSeparator ::
  forall a.
  { separator :: RecDefinitionSeparator_Separator a } ->
  RecDefinitionSeparator a
recDefinitionSeparator rec =
  Rec.recDefinitionSeparator
    { separator:
        undefined
    }

type RecDefinition a
  = Rec.RecDefinition (a)

type RecDefinition_TermDefinition a
  = Rec.RecDefinition_TermDefinition ({} -> a)

type RecDefinition_DataDefinition a
  = Rec.RecDefinition_DataDefinition ({} -> a)

recDefinition ::
  forall a.
  { term :: RecDefinition_TermDefinition a
  , data :: RecDefinition_DataDefinition a
  } ->
  RecDefinition a
recDefinition rec =
  Rec.recDefinition
    { term:
        undefined
    , data:
        undefined
    }

type RecConstructorSeparator a
  = Rec.RecConstructorSeparator a

type RecConstructorSeparator_Separator a
  = Rec.RecConstructorSeparator_Separator ({} -> a)

recConstructorSeparator ::
  forall a.
  { separator :: RecConstructorSeparator_Separator a } ->
  RecConstructorSeparator a
recConstructorSeparator rec =
  Rec.recConstructorSeparator
    { separator: undefined }

type RecConstructor a
  = Rec.RecConstructor (a)

type RecConstructor_Constructor a
  = Rec.RecConstructor_Constructor ({} -> a)

-- registration already handled by recDefinitionItems
recConstructor ::
  forall a.
  { constructor :: RecConstructor_Constructor a } ->
  RecConstructor a
recConstructor rec =
  Rec.recConstructor
    { constructor:
        undefined
    }

type RecParameterSeparator a
  = Rec.RecParameterSeparator a

type RecParameterSeparator_Separator a
  = Rec.RecParameterSeparator_Separator ({} -> a)

recParameterSeparator ::
  forall a.
  { separator :: RecParameterSeparator_Separator a } ->
  RecParameterSeparator a
recParameterSeparator rec =
  Rec.recParameterSeparator
    { separator:
        undefined
    }

type CommonTypeTransformationsArgs
  = { gamma ∷ Context
    , ix ∷ DownwardIndex
    , type_ ∷ Type
    }

type CommonTypeTransformations r
  = { enArrow :: Transformation
    , dig :: Transformation
    | r
    }

-- WARNING: don't overlap labels in `CommonTypeTransformations` and `r`.
makeCommonTypeTransformations :: forall r. CommonTypeTransformationsArgs -> Record r -> CommonTypeTransformations r
makeCommonTypeTransformations args r =
  unsafeUnion r
    { enArrow:
        let
          hole = mkHoleType (freshHoleId unit) Set.empty
        in
          makeTypeTransformation
            $ union
                { type_: mkArrow (mkParam (TermName Nothing) hole) args.type_
                , typeChange: InsertArg hole
                }
                (delete _type_ args)
    , dig:
        let
          holeId = freshHoleId unit

          hole = mkHoleType holeId Set.empty
        in
          makeTypeTransformation
            $ union
                { type_: hole
                , typeChange: Dig holeId
                }
                (delete _type_ args)
    }

type RecType a
  = Rec.RecType (a)

type RecType_Arrow a
  = Rec.RecType_Arrow (CommonTypeTransformations ( delete :: Transformation ) -> a)

type RecType_Data a
  = Rec.RecType_Data (CommonTypeTransformations () -> a)

type RecType_Hole a
  = Rec.RecType_Hole (CommonTypeTransformations () -> a)

type RecType_ProxyHole a
  = Rec.RecType_ProxyHole a

recType ::
  forall a.
  { arrow :: RecType_Arrow a
  , data :: RecType_Data a
  , hole :: RecType_Hole a
  , proxyHole :: RecType_ProxyHole a
  } ->
  RecType a
recType rec =
  Rec.recType
    { arrow:
        \prm beta meta gamma metaGamma ixUp isSelected ix_prm csr_prm ix_beta csr_beta ->
          let
            ix = toDownwardIndex ixUp
          in
            rec.arrow prm beta meta gamma metaGamma ixUp isSelected ix_prm csr_prm ix_beta csr_beta
              $ makeCommonTypeTransformations { gamma, ix, type_: ArrowType prm beta meta }
                  { delete: makeTypeTransformation { gamma, ix, type_: beta, typeChange: RemoveArg } }
    , data:
        \typeId meta gamma metaGamma ix isSelected ->
          rec.data typeId meta gamma metaGamma ix isSelected
            $ makeCommonTypeTransformations { gamma, ix: toDownwardIndex ix, type_: DataType typeId meta } {}
    , hole:
        \holeId wkn meta gamma metaGamma ix isSelected ->
          rec.hole holeId wkn meta gamma metaGamma ix isSelected
            $ makeCommonTypeTransformations { gamma, ix: toDownwardIndex ix, type_: HoleType holeId wkn meta } {}
    , proxyHole: rec.proxyHole
    }

type RecTerm a
  = Rec.RecTerm (a)

type RecTerm_Lambda a
  = Rec.RecTerm_Lambda ({} -> a)

type RecTerm_Neutral a
  = Rec.RecTerm_Neutral ({} -> a)

type RecTerm_Match a
  = Rec.RecTerm_Match ({} -> a)

type RecTerm_Hole a
  = Rec.RecTerm_Hole ({} -> a)

recTerm ::
  forall a.
  { lambda :: RecTerm_Lambda a
  , neutral :: RecTerm_Neutral a
  , match :: RecTerm_Match a
  , hole :: RecTerm_Hole a
  } ->
  RecTerm a
recTerm rec =
  Rec.recTerm
    { lambda:
        undefined
    , neutral:
        undefined
    , match:
        undefined
    , hole:
        undefined
    }

type RecArgItems a
  = Rec.RecArgItems (a)

type RecArgItems_Nil a
  = Rec.RecArgItems_Nil ({} -> a)

type RecArgItems_Cons a
  = Rec.RecArgItems_Cons ({} -> a)

recArgItems ::
  forall a.
  { nil :: RecArgItems_Nil a
  , cons :: RecArgItems_Cons a
  } ->
  RecArgItems a
recArgItems rec =
  Rec.recArgItems
    { nil: undefined
    , cons: undefined
    }

type RecCase a
  = Rec.RecCase (a)

type RecCase_Case a
  = Rec.RecCase_Case ({} -> a)

recCase ::
  forall a.
  { case_ :: RecCase_Case a } ->
  RecCase a
recCase rec =
  Rec.recCase
    { case_:
        undefined
    }

type RecParameter a
  = Rec.RecParameter (a)

type RecParameter_Parameter a
  = Rec.RecParameter_Parameter ({} -> a)

recParameter ::
  forall a.
  { parameter :: RecParameter_Parameter a } ->
  RecParameter a
recParameter rec =
  Rec.recParameter
    { parameter:
        undefined
    }

type RecTypeBinding a
  = Rec.RecTypeBinding a

type RecTypeBinding_TypeBinding a
  = Rec.RecTypeBinding_TypeBinding ({} -> a)

recTypeBinding ::
  forall a.
  { typeBinding :: RecTypeBinding_TypeBinding a
  } ->
  RecTypeBinding a
recTypeBinding rec =
  Rec.recTypeBinding
    { typeBinding: undefined }

type RecTermBinding a
  = Rec.RecTermBinding a

type RecTermBinding_TermBinding a
  = Rec.RecTermBinding_TermBinding ({} -> a)

recTermBinding ::
  forall a.
  { termBinding :: RecTermBinding_TermBinding a
  } ->
  RecTermBinding a
recTermBinding rec =
  Rec.recTermBinding
    { termBinding: undefined }

type RecTermId a
  = Rec.RecTermId a

type RecTermId_TermId a
  = Rec.RecTermId_TermId ({} -> a)

recTermId :: forall a. { termId :: RecTermId_TermId a } -> RecTermId a
recTermId rec = Rec.recTermId { termId: undefined }
