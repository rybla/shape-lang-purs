module Language.Shape.Stlc.Recursion.Transformation where

import Data.Either
import Data.Maybe
import Data.Tuple.Nested
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.IndexSyntax
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Typing
import Prelude
import Prim hiding (Type)
import Record

import Control.Monad.State (State, runState)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.List.Unsafe (List(..))
import Data.List.Unsafe as List
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Data.Show.Generic (class GenericShow, genericShow)
import Data.String as String
import Data.Tuple (Tuple)
import Debug as Debug
import Language.Shape.Stlc.ChangeAtIndex (Change(..), chAtModule)
import Language.Shape.Stlc.Changes (TypeChange(..))
import Language.Shape.Stlc.Changes as Ch
import Language.Shape.Stlc.Holes (HoleSub, subModule)
import Language.Shape.Stlc.Recursion.Index as Rec
import Language.Shape.Stlc.Recursion.MetaContext (MetaContext)
import Language.Shape.Stlc.RenderingTypes as RenderingTypes
import Partial.Unsafe (unsafeCrashWith)
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

unimplementedTransformation :: String -> Transformation
unimplementedTransformation label = \_ _ -> unsafeCrashWith $ "unimplemented transformation: " <> label

-- transformations only operate on the prestate
type Transformation
  = TransformationInputs ->
    RenderingTypes.State -> Maybe RenderingTypes.State

type TransformationInputs
  = { syntax :: Maybe Syntax
    , i :: Maybe Int
    }

defaultTransformationInputs :: TransformationInputs
defaultTransformationInputs = { syntax: Nothing, i: Nothing }

type ModuleTransformation
  = TransformationInputs ->
    { gamma :: Context
    , ix :: DownwardIndex
    , syntax :: Syntax
    , change :: Change
    }

type TypeTransformation
  = TransformationInputs ->
    { gamma :: Context
    , ix :: DownwardIndex
    , type_ :: Type
    , typeChange :: TypeChange
    }

-- genericShowListTuple :: forall a1 rep1 a2 rep2. Generic a1 rep1 => Generic a2 rep2 => GenericShow rep1 => GenericShow rep2 => List (Tuple a1 a2) -> String
-- genericShowListTuple ls = show $ ((\(a1 /\ a2) -> genericShow a1 /\ genericShow a2) <$> ls)

makeModuleTransformation :: ModuleTransformation -> Transformation
makeModuleTransformation makeTransArgs transInputs st = do
  let
    transArgs = makeTransArgs transInputs
  -- Debug.traceM
  --   $ String.joinWith "\n"
  --       [ "transformation"
  --       , "module: " <> show st.module_
  --       , "gamma: " <> show transArgs.gamma
  --       , "syntax: " <> show transArgs.syntax
  --       , "change: " <> show transArgs.change
  --       , "ix: " <> show transArgs.ix
  --       ]
  
  let changeHistory' = (transArgs.syntax /\ transArgs.change /\ transArgs.ix) List.: st.changeHistory
  Debug.traceM $ "===[ changeHistory ] ================================================"
  Debug.traceM $ show changeHistory'

  (module' /\ ix' /\ holeSub) <- chAtModule st.module_ transArgs.gamma transArgs.syntax transArgs.change transArgs.ix
  -- TODO: let module' = subModule holeSub module_
  -- pure { module_ = module', ix_cursor = DownwardIndex Nil, changeHistory = changeHistory' }
  pure st { module_ = module', ix_cursor = ix', changeHistory = changeHistory' }

makeTypeTransformation :: TypeTransformation -> Transformation
makeTypeTransformation makeTransArgs =
  makeModuleTransformation \transInputs ->
    let
      transArgs = makeTransArgs transInputs
    in
      union
        { syntax: SyntaxType transArgs.type_
        , change: ChangeTypeChange transArgs.typeChange
        }
        (delete _type_ $ delete _typeChange transArgs)

type RecModule a
  = Rec.RecModule a

type RecModule_Module a
  = Rec.RecModule_Module ({} -> a)

recModule ::
  forall a.
  { module_ :: RecModule_Module a } ->
  RecModule a
recModule rec =
  Rec.recModule
    { module_:
        \defItems meta gamma metaGamma ixArgs ->
          rec.module_ defItems meta gamma metaGamma ixArgs
            {}
    }

type RecBlock a
  = Rec.RecBlock a

type RecBlock_Block a
  = Rec.RecBlock_Block ({} -> a)

recBlock ::
  forall a.
  { block :: RecBlock_Block a } ->
  RecBlock a
recBlock rec =
  Rec.recBlock
    { block:
        \defItems block meta gamma alpha metaGamma ixArgs ->
          rec.block defItems block meta gamma alpha metaGamma ixArgs
            {}
    }

type RecDefinitionItems a
  = Rec.RecDefinitionItems a

type RecDefinitionItems_DefinitionItems a
  = Rec.RecDefinitionItems_DefinitionItems
      ( { moveDefinition :: Transformation
        } ->
        a
      )

recDefinitionItems ::
  forall a.
  { definitionItems :: RecDefinitionItems_DefinitionItems a } ->
  RecDefinitionItems a
recDefinitionItems rec =
  Rec.recDefinitionItems
    { definitionItems:
        \defItems gamma metaGamma ixArgs ->
          rec.definitionItems defItems gamma metaGamma ixArgs
            { moveDefinition: unimplementedTransformation "DefinitionItems.moveDefinition"
            }
    }

type RecDefinitionSeparator a
  = Rec.RecDefinitionSeparator a

type RecDefinitionSeparator_Separator a
  = Rec.RecDefinitionSeparator_Separator
      ( { insertTermDefinition :: Transformation
        , insertDataDefinition :: Transformation
        , paste :: Transformation
        } ->
        a
      )

recDefinitionSeparator ::
  forall a.
  { separator :: RecDefinitionSeparator_Separator a } ->
  RecDefinitionSeparator a
recDefinitionSeparator rec =
  Rec.recDefinitionSeparator
    { separator:
        \ixArgs ->
          rec.separator ixArgs
            { insertTermDefinition: unimplementedTransformation "DefinitionSeparator.insertTermDefinition"
            , insertDataDefinition: unimplementedTransformation "DefinitionSeparator.insertDataDefinition"
            , paste: unimplementedTransformation "DefinitionSeparator.paste"
            }
    }

type CommonDefinitionTransformations r
  = { rename :: Transformation
    , delete :: Transformation
    | r
    }

makeCommonDefinitionTransformations :: forall r. Record r -> CommonDefinitionTransformations r
makeCommonDefinitionTransformations r =
  unsafeUnion r
    { rename: unimplementedTransformation "transformation: Definition.rename"
    , delete: unimplementedTransformation "transformation: Definition.delete"
    }

type RecDefinition a
  = Rec.RecDefinition a

type RecDefinition_TermDefinition a
  = Rec.RecDefinition_TermDefinition
      ( CommonDefinitionTransformations () ->
        a
      )

type RecDefinition_DataDefinition a
  = Rec.RecDefinition_DataDefinition
      ( CommonDefinitionTransformations () ->
        a
      )

recDefinition ::
  forall a.
  { term :: RecDefinition_TermDefinition a
  , data :: RecDefinition_DataDefinition a
  } ->
  RecDefinition a
recDefinition rec =
  Rec.recDefinition
    { term:
        \termBinding type_ term meta gamma metaGamma ixArgs ->
          rec.term termBinding type_ term meta gamma metaGamma ixArgs
            $ makeCommonDefinitionTransformations {}
    , data:
        \typeBinding constrItems meta gamma metaGamma ixArgs ->
          rec.data typeBinding constrItems meta gamma metaGamma ixArgs
            $ makeCommonDefinitionTransformations {}
    }

type RecConstructorSeparator a
  = Rec.RecConstructorSeparator a

type RecConstructorSeparator_Separator a
  = Rec.RecConstructorSeparator_Separator
      ( { insert :: Transformation
        , paste :: Transformation
        } ->
        a
      )

recConstructorSeparator ::
  forall a.
  { separator :: RecConstructorSeparator_Separator a } ->
  RecConstructorSeparator a
recConstructorSeparator rec =
  Rec.recConstructorSeparator
    { separator:
        \ixArgs ->
          rec.separator ixArgs
            { insert: unimplementedTransformation "ConstructorSeparator.insert"
            , paste: unimplementedTransformation "ConstructorSeparator.insert"
            }
    }

type RecConstructor a
  = Rec.RecConstructor a

type RecConstructor_Constructor a
  = Rec.RecConstructor_Constructor
      ( { rename :: Transformation
        , move :: Transformation
        } ->
        a
      )

-- registration already handled by recDefinitionItems
recConstructor ::
  forall a.
  { constructor :: RecConstructor_Constructor a } ->
  RecConstructor a
recConstructor rec =
  Rec.recConstructor
    { constructor:
        \termBinding paramItems meta typeId gamma alpha metaGamma metaGamma_param_t ixArgs ->
          rec.constructor termBinding paramItems meta typeId gamma alpha metaGamma metaGamma_param_t ixArgs
            { move: unimplementedTransformation "Constructor.move"
            , rename: unimplementedTransformation "Constructor.rename"
            }
    }

type RecParameterSeparator a
  = Rec.RecParameterSeparator a

type RecParameterSeparator_Separator a
  = Rec.RecParameterSeparator_Separator
      ( { insert :: Transformation
        , paste :: Transformation
        } ->
        a
      )

recParameterSeparator ::
  forall a.
  { separator :: RecParameterSeparator_Separator a } ->
  RecParameterSeparator a
recParameterSeparator rec =
  Rec.recParameterSeparator
    { separator:
        \ixArgs ->
          rec.separator ixArgs
            { insert: unimplementedTransformation "ParametereSeparator.insert"
            , paste: unimplementedTransformation "ParametereSeparator.paste" }
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

makeCommonTypeTransformations :: forall r. Record r -> CommonTypeTransformationsArgs -> CommonTypeTransformations r
makeCommonTypeTransformations r commonArgs =
  unsafeUnion r
    { enArrow:
        let
          hole = mkHoleType (freshHoleId unit) Set.empty

          termName = defaultTermName
        in
          makeTypeTransformation \transInputs ->
            union
              { type_: mkArrow (mkParam termName hole) commonArgs.type_
              , typeChange: InsertArg hole
              }
              (delete _type_ commonArgs)
    , dig:
        let
          holeId = freshHoleId unit

          hole = mkHoleType holeId Set.empty
        in
          makeTypeTransformation \transInputs ->
            union
              { type_: hole
              , typeChange: Dig holeId
              }
              (delete _type_ commonArgs)
    }

type RecType a
  = Rec.RecType a

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
        \prm beta meta gamma metaGamma ixArgs ->
          let
            ix = toDownwardIndex ixArgs.ix
          in
            rec.arrow prm beta meta gamma metaGamma ixArgs
              $ makeCommonTypeTransformations
                  { delete: makeTypeTransformation \transInputs -> { gamma, ix, type_: beta, typeChange: RemoveArg } }
                  { gamma, ix, type_: ArrowType prm beta meta }
    , data:
        \typeId meta gamma metaGamma ixArgs ->
          rec.data typeId meta gamma metaGamma ixArgs
            $ makeCommonTypeTransformations {} { gamma, ix: toDownwardIndex ixArgs.ix, type_: DataType typeId meta }
    , hole:
        \holeId wkn meta gamma metaGamma ixArgs ->
          rec.hole holeId wkn meta gamma metaGamma ixArgs
            $ makeCommonTypeTransformations {} { gamma, ix: toDownwardIndex ixArgs.ix, type_: HoleType holeId wkn meta }
    , proxyHole: rec.proxyHole
    }

type CommonTermTransformations r
  = { copy :: Transformation 
    , enLambda :: Transformation
    , dig :: Transformation
    | r
    }

type CommonTermTransformationsArgs
  = {}

makeCommonTermTransformations :: forall r. Record r -> CommonTermTransformationsArgs -> CommonTermTransformations r
makeCommonTermTransformations r commonArgs =
  unsafeUnion r
    { copy: unimplementedTransformation "Term.copy"
    , enLambda: unimplementedTransformation "Term.enLambda"
    , dig: unimplementedTransformation "Term.dig"
    }

type RecTerm a
  = Rec.RecTerm a

type RecTerm_Lambda a
  = Rec.RecTerm_Lambda
      ( CommonTermTransformations
          ( unLambda :: Transformation
          , etaContract :: Transformation
          ) ->
        a
      )

type RecTerm_Neutral a
  = Rec.RecTerm_Neutral
      ( CommonTermTransformations
          ( etaExpand :: Transformation
          ) ->
        a
      )

type RecTerm_Match a
  = Rec.RecTerm_Match (CommonTermTransformations () -> a)

type RecTerm_Hole a
  = Rec.RecTerm_Hole (CommonTermTransformations ( fill :: Transformation ) -> a)

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
        \termId block meta gamma param beta metaGamma ixArgs ->
          rec.lambda termId block meta gamma param beta metaGamma ixArgs
            $ makeCommonTermTransformations
                { unLambda: unimplementedTransformation "LambdaTerm.unLambda"
                , etaContract: unimplementedTransformation "LambdaTerm.etaContract"
                }
                {}
    , neutral:
        \termId argItems meta gamma alpha metaGamma ixArgs ->
          rec.neutral termId argItems meta gamma alpha metaGamma ixArgs
            $ makeCommonTermTransformations
                { etaExpand: unimplementedTransformation "NeutralTerm.etaExpand"
                }
                {}
    , match:
        \typeId term caseItems meta gamma alpha metaGamma constrIds ixArgs ->
          rec.match typeId term caseItems meta gamma alpha metaGamma constrIds ixArgs
            $ makeCommonTermTransformations {} {}
    , hole:
        \meta gamma alpha metaGamma ixArgs ->
          rec.hole meta gamma alpha metaGamma ixArgs
            $ makeCommonTermTransformations
                { fill: unimplementedTransformation "HoleTerm.fill"
                }
                {}
    }

type RecArgItems a
  = Rec.RecArgItems a

type RecArgItems_Nil a
  = Rec.RecArgItems_Nil
      ( { insert :: Transformation
        } ->
        a
      )

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
    { nil:
        \gamma alpha metaGamma ixArgs ->
          rec.nil gamma alpha metaGamma ixArgs
            { insert: unimplementedTransformation "NilArgItems.insert" }
    , cons:
        \argItem argItems meta prm alpha metaGamma ixArgs ->
          rec.cons argItem argItems meta prm alpha metaGamma ixArgs
            {}
    }

type RecCase a
  = Rec.RecCase a

type RecCase_Case a
  = Rec.RecCase_Case ({} -> a)

recCase ::
  forall a.
  { case_ :: RecCase_Case a } ->
  RecCase a
recCase rec =
  Rec.recCase
    { case_:
        \termIdItems block meta typeId constrId gamma alpha metaGamma ixArgs ->
          rec.case_ termIdItems block meta typeId constrId gamma alpha metaGamma ixArgs
            {}
    }

type RecParameter a
  = Rec.RecParameter a

type RecParameter_Parameter a
  = Rec.RecParameter_Parameter ({} -> a)

recParameter ::
  forall a.
  { parameter :: RecParameter_Parameter a } ->
  RecParameter a
recParameter rec =
  Rec.recParameter
    { parameter:
        \alpha meta gamma metaGamma ixArgs ->
          rec.parameter alpha meta gamma metaGamma ixArgs
            {}
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
    { typeBinding:
        \typeId meta gamma metaGamma ixArgs ->
          rec.typeBinding typeId meta gamma metaGamma ixArgs
            {}
    }

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
    { termBinding:
        \typeId meta gamma metaGmma ixArgs ->
          rec.termBinding typeId meta gamma metaGmma ixArgs
            {}
    }

type RecTermId a
  = Rec.RecTermId a

type RecTermId_TermId a
  = Rec.RecTermId_TermId ({} -> a)

recTermId :: forall a. { termId :: RecTermId_TermId a } -> RecTermId a
recTermId rec =
  Rec.recTermId
    { termId:
        \termId gamma metaGamma ixArgs ->
          rec.termId termId gamma metaGamma ixArgs
            {}
    }
