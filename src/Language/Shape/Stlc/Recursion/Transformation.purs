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
import Data.List.Unsafe (List)
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
import Language.Shape.Stlc.RenderingTypes (Environment)
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

unimplementedTransformation :: forall st. String -> Transformation st
unimplementedTransformation label = \_ _ -> unsafeCrashWith $ "unimplemented transformation: " <> label

-- transformations only operate on the prestate
type Transformation st
  = TransformationInputs ->
    RenderingTypes.Prestate st -> Maybe (RenderingTypes.Prestate st)

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

makeModuleTransformation :: forall st. ModuleTransformation -> Transformation st
makeModuleTransformation makeTransArgs transInputs st = do
  let
    transArgs = makeTransArgs transInputs
  Debug.traceM
    $ String.joinWith "\n"
        [ "transformation"
        , "module: " <> show st.module_
        , "gamma: " <> show transArgs.gamma
        , "syntax: " <> show transArgs.syntax
        , "change: " <> show transArgs.change
        , "ix: " <> show transArgs.ix
        ]
  
  let changeHistory' = (transArgs.syntax /\ transArgs.change /\ transArgs.ix) List.: st.changeHistory
  Debug.traceM $ "===[ changeHistory ] ================================================"
  Debug.traceM $ show changeHistory'

  (module' /\ ix' /\ holeSub) <- chAtModule st.module_ transArgs.gamma transArgs.syntax transArgs.change transArgs.ix
  -- TODO: let module' = subModule holeSub module_
  pure st { module_ = module', ix_cursor = ix', changeHistory = changeHistory' }

makeTypeTransformation :: forall st. TypeTransformation -> Transformation st
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
  forall st a.
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
  forall st a.
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

type RecDefinitionItems_DefinitionItems st a
  = Rec.RecDefinitionItems_DefinitionItems
      ( { moveDefinition :: Transformation st
        } ->
        a
      )

recDefinitionItems ::
  forall st a.
  { definitionItems :: RecDefinitionItems_DefinitionItems st a } ->
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

type RecDefinitionSeparator_Separator st a
  = Rec.RecDefinitionSeparator_Separator
      ( { insertTermDefinition :: Transformation st
        , insertDataDefinition :: Transformation st
        , paste :: Transformation st
        } ->
        a
      )

recDefinitionSeparator ::
  forall st a.
  { separator :: RecDefinitionSeparator_Separator st a } ->
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

type CommonDefinitionTransformations st r
  = { rename :: Transformation st
    , delete :: Transformation st
    | r
    }

makeCommonDefinitionTransformations :: forall st r. Record r -> CommonDefinitionTransformations st r
makeCommonDefinitionTransformations r =
  unsafeUnion r
    { rename: unimplementedTransformation "transformation: Definition.rename"
    , delete: unimplementedTransformation "transformation: Definition.delete"
    }

type RecDefinition a
  = Rec.RecDefinition a

type RecDefinition_TermDefinition st a
  = Rec.RecDefinition_TermDefinition
      ( CommonDefinitionTransformations st () ->
        a
      )

type RecDefinition_DataDefinition st a
  = Rec.RecDefinition_DataDefinition
      ( CommonDefinitionTransformations st () ->
        a
      )

recDefinition ::
  forall st a.
  { term :: RecDefinition_TermDefinition st a
  , data :: RecDefinition_DataDefinition st a
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

type RecConstructorSeparator_Separator st a
  = Rec.RecConstructorSeparator_Separator
      ( { insert :: Transformation st
        , paste :: Transformation st
        } ->
        a
      )

recConstructorSeparator ::
  forall st a.
  { separator :: RecConstructorSeparator_Separator st a } ->
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

type RecConstructor_Constructor st a
  = Rec.RecConstructor_Constructor
      ( { rename :: Transformation st
        , move :: Transformation st
        } ->
        a
      )

-- registration already handled by recDefinitionItems
recConstructor ::
  forall st a.
  { constructor :: RecConstructor_Constructor st a } ->
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

type RecParameterSeparator_Separator st a
  = Rec.RecParameterSeparator_Separator
      ( { insert :: Transformation st
        } ->
        a
      )

recParameterSeparator ::
  forall st a.
  { separator :: RecParameterSeparator_Separator st a } ->
  RecParameterSeparator a
recParameterSeparator rec =
  Rec.recParameterSeparator
    { separator:
        \ixArgs ->
          rec.separator ixArgs
            { insert: unimplementedTransformation "ParametereSeparator.insert " }
    }

type CommonTypeTransformationsArgs
  = { gamma ∷ Context
    , ix ∷ DownwardIndex
    , type_ ∷ Type
    }

type CommonTypeTransformations st r
  = { enArrow :: Transformation st
    , dig :: Transformation st
    | r
    }

makeCommonTypeTransformations :: forall st r. Record r -> CommonTypeTransformationsArgs -> CommonTypeTransformations st r
makeCommonTypeTransformations r commonArgs =
  unsafeUnion r
    { enArrow:
        let
          hole = mkHoleType (freshHoleId unit) Set.empty

          termName = TermName Nothing
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

type RecType_Arrow st a
  = Rec.RecType_Arrow (CommonTypeTransformations st ( delete :: Transformation st ) -> a)

type RecType_Data st a
  = Rec.RecType_Data (CommonTypeTransformations st () -> a)

type RecType_Hole st a
  = Rec.RecType_Hole (CommonTypeTransformations st () -> a)

type RecType_ProxyHole st a
  = Rec.RecType_ProxyHole a

recType ::
  forall st a.
  { arrow :: RecType_Arrow st a
  , data :: RecType_Data st a
  , hole :: RecType_Hole st a
  , proxyHole :: RecType_ProxyHole st a
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

type CommonTermTransformations st r
  = { enLambda :: Transformation st
    , dig :: Transformation st
    | r
    }

type CommonTermTransformationsArgs
  = {}

makeCommonTermTransformations :: forall st r. Record r -> CommonTermTransformationsArgs -> CommonTermTransformations st r
makeCommonTermTransformations r commonArgs =
  unsafeUnion r
    { copy: unimplementedTransformation "Term.copy"
    , enLambda: unimplementedTransformation "Term.enLambda"
    , dig: unimplementedTransformation "Term.dig"
    }

type RecTerm a
  = Rec.RecTerm a

type RecTerm_Lambda st a
  = Rec.RecTerm_Lambda
      ( CommonTermTransformations st
          ( unLambda :: Transformation st
          , etaContract :: Transformation st
          ) ->
        a
      )

type RecTerm_Neutral st a
  = Rec.RecTerm_Neutral
      ( CommonTermTransformations st
          ( etaExpand :: Transformation st
          ) ->
        a
      )

type RecTerm_Match st a
  = Rec.RecTerm_Match (CommonTermTransformations st () -> a)

type RecTerm_Hole st a
  = Rec.RecTerm_Hole (CommonTermTransformations st ( fill :: Transformation st ) -> a)

recTerm ::
  forall st a.
  { lambda :: RecTerm_Lambda st a
  , neutral :: RecTerm_Neutral st a
  , match :: RecTerm_Match st a
  , hole :: RecTerm_Hole st a
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

type RecArgItems_Nil st a
  = Rec.RecArgItems_Nil
      ( { insert :: Transformation st
        } ->
        a
      )

type RecArgItems_Cons st a
  = Rec.RecArgItems_Cons ({} -> a)

recArgItems ::
  forall st a.
  { nil :: RecArgItems_Nil st a
  , cons :: RecArgItems_Cons st a
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

type RecCase_Case st a
  = Rec.RecCase_Case ({} -> a)

recCase ::
  forall st a.
  { case_ :: RecCase_Case st a } ->
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

type RecParameter_Parameter st a
  = Rec.RecParameter_Parameter ({} -> a)

recParameter ::
  forall st a.
  { parameter :: RecParameter_Parameter st a } ->
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

type RecTypeBinding_TypeBinding st a
  = Rec.RecTypeBinding_TypeBinding ({} -> a)

recTypeBinding ::
  forall st a.
  { typeBinding :: RecTypeBinding_TypeBinding st a
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

type RecTermBinding_TermBinding st a
  = Rec.RecTermBinding_TermBinding ({} -> a)

recTermBinding ::
  forall st a.
  { termBinding :: RecTermBinding_TermBinding st a
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

type RecTermId_TermId st a
  = Rec.RecTermId_TermId ({} -> a)

recTermId :: forall st a. { termId :: RecTermId_TermId st a } -> RecTermId a
recTermId rec =
  Rec.recTermId
    { termId:
        \termId gamma metaGamma ixArgs ->
          rec.termId termId gamma metaGamma ixArgs
            {}
    }
