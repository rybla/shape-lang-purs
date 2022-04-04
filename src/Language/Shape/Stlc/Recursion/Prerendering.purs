module Language.Shape.Stlc.Recursion.Prerender where

import Data.Either
import Data.Maybe
import Data.Tuple.Nested
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.RenderingAux hiding (indent, indentation)
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Typing
import Prelude
import Prelude
import Prim hiding (Type)
import Control.Monad.State (State, runState)
import Data.Array (concat, concatMap)
import Data.Foldable (foldl)
import Data.List.Unsafe as List
import Data.Map (Map)
import Data.Map as Map
import Debug as Debug
import Language.Shape.Stlc.Changes as Ch
import Language.Shape.Stlc.Holes (HoleSub)
import Language.Shape.Stlc.Recursion.MetaContext (MetaContext)
import Language.Shape.Stlc.Recursion.Transformation (Transformation)
import Language.Shape.Stlc.Recursion.Transformation as Rec
import Prim.Row (class Union)
import React (ReactElement)
import Undefined (undefined)
import Unsafe as Unsafe
import Unsafe.Coerce (unsafeCoerce)

unsafeSubrecord :: forall row1 row2 row3. Union row1 row2 row3 => Record row3 -> Record row1
unsafeSubrecord = unsafeCoerce

type Prerender a
  = a -> Words

type Words
  = Array Word

data Word
  = WordReactElement ReactElement
  | WordTermName String
  | WordTypeName String
  | WordNewline Int
  | WordSelection ({ ix :: UpwardIndex, isSelected :: Boolean }) Words

selection :: forall r. { ix :: UpwardIndex, isSelected :: Boolean | r } -> Words -> Word
selection args = WordSelection (unsafeSubrecord args)

indent :: forall r. { indented :: Boolean | r } -> MetaContext -> Words
indent { indented } metaGamma =
  if indented then
    [ WordNewline metaGamma.indentation ]
  else
    []

indentParent :: forall r. { indented :: Boolean | r } -> MetaContext -> Words
indentParent { indented } metaGamma =
  if indented then
    [ WordNewline (metaGamma.indentation - 1) ]
  else
    []

-- | Recursion principles for prerendering. "prerendering" is the conversion of
-- the syntax into the following form: a list of lines where each line is a list of words,
-- and each word is a string (or special inline-block)
type RecModule
  = Rec.RecModule (Prerender Words -> Words)

type RecModule_Module
  = Rec.RecModule_Module (Prerender { defItems :: Words } -> Words)

recModule ::
  { module_ :: RecModule_Module } ->
  RecModule
recModule rec =
  Rec.recModule
    { module_:
        \defItems meta gamma metaGamma ixArgs transArgs prernd ->
          rec.module_ defItems meta gamma metaGamma ixArgs transArgs \prerndSub ->
            prernd [ selection ixArgs $ prerndSub.defItems ]
    }

type RecBlock
  = Rec.RecBlock (Prerender Words -> Words)

type RecBlock_Block
  = Rec.RecBlock_Block (Prerender { defItems :: Words, term :: Words } -> Words)

recBlock ::
  { block :: RecBlock_Block } ->
  RecBlock
recBlock rec =
  Rec.recBlock
    { block:
        \defItems term meta gamma alpha metaGamma ixArgs transArgs prernd ->
          rec.block defItems term meta gamma alpha metaGamma ixArgs transArgs \prerndSub ->
            prernd
              [ selection ixArgs
                  $ indent { indented: true } metaGamma
                  <> [ WordReactElement keyword.let_ ]
                  <> prerndSub.defItems
                  <> indent { indented: true } metaGamma
                  <> [ WordReactElement keyword.in_ ]
                  <> prerndSub.term
              ]
    }

type RecDefinitionItems
  = Rec.RecDefinitionItems (Prerender Words -> Words)

type RecDefinitionItems_DefinitionItems
  = Rec.RecDefinitionItems_DefinitionItems (Prerender { defItems :: Words } -> Words)

recDefinitionItems ::
  { definitionItems :: RecDefinitionItems_DefinitionItems } ->
  RecDefinitionItems
recDefinitionItems rec =
  Rec.recDefinitionItems
    { definitionItems:
        \defItems gamma metaGamma ixArgs transArgs prernd ->
          rec.definitionItems defItems gamma metaGamma ixArgs transArgs \prerndSub ->
            prernd
              [ selection ixArgs $ prerndSub.defItems ]
    }

type RecDefinitionSeparator
  = Rec.RecDefinitionSeparator (Prerender Words -> Words)

type RecDefinitionSeparator_Separator
  = Rec.RecDefinitionSeparator_Separator (Words -> Words)

recDefinitionSeparator ::
  { separator :: RecDefinitionSeparator_Separator } ->
  RecDefinitionSeparator
recDefinitionSeparator rec =
  Rec.recDefinitionSeparator
    { separator:
        \ixArgs transArgs prernd ->
          rec.separator ixArgs transArgs
            $ prernd [ selection ixArgs [ WordReactElement punctuation.sep ] ]
    }

type RecDefinition
  = Rec.RecDefinition (Prerender Words -> Words)

type RecDefinition_TermDefinition
  = Rec.RecDefinition_TermDefinition (Prerender { termId :: Words, type_ :: Words, term :: Words } -> Words)

type RecDefinition_DataDefinition
  = Rec.RecDefinition_DataDefinition (Prerender { typeId :: Words, constrItems :: Words } -> Words)

recDefinition ::
  { term :: RecDefinition_TermDefinition
  , data :: RecDefinition_DataDefinition
  } ->
  RecDefinition
recDefinition rec =
  Rec.recDefinition
    { term:
        \termBinding type_ term meta gamma metaGamma ixArgs transArgs prernd ->
          rec.term termBinding type_ term meta gamma metaGamma ixArgs transArgs \prerndSub ->
            prernd
              [ selection ixArgs
                  $ indent { indented: true } metaGamma
                  <> [ WordReactElement keyword.val ]
                  <> prerndSub.termId
                  <> [ WordReactElement punctuation.colon ]
                  <> prerndSub.type_
                  <> indent { indented: true } metaGamma
                  <> [ WordReactElement keyword.let_ ]
                  <> prerndSub.termId
                  <> [ WordReactElement punctuation.termdef ]
                  <> prerndSub.term
              ]
    , data:
        \typeBinding constrItems meta gamma metaGamma ixArgs transArgs prernd ->
          rec.data typeBinding constrItems meta gamma metaGamma ixArgs transArgs \prerndSub ->
            prernd
              [ selection ixArgs
                  $ indent { indented: true } metaGamma
                  <> [ WordReactElement keyword.data_ ]
                  <> prerndSub.typeId
                  <> [ WordReactElement punctuation.typedef ]
                  <> prerndSub.constrItems
              ]
    }

type RecConstructorSeparator
  = Rec.RecConstructorSeparator (Prerender Words -> Words)

type RecConstructorSeparator_Separator
  = Rec.RecConstructorSeparator_Separator (Prerender Words -> Words)

recConstructorSeparator ::
  { separator :: RecConstructorSeparator_Separator } ->
  RecConstructorSeparator
recConstructorSeparator rec =
  Rec.recConstructorSeparator
    { separator:
        \ixArgs transArgs prernd ->
          rec.separator ixArgs transArgs \prerndSub ->
            prernd [ selection ixArgs [ WordReactElement punctuation.sep ] ]
    }

type RecConstructor
  = Rec.RecConstructor (Prerender Words -> Words)

type RecConstructor_Constructor
  = Rec.RecConstructor_Constructor (Prerender { termBinding :: Words, paramItems :: Words, block :: Words } -> Words)

-- registration already handled by recDefinitionItems
recConstructor ::
  { constructor :: RecConstructor_Constructor } ->
  RecConstructor
recConstructor rec =
  Rec.recConstructor
    { constructor:
        \termBinding paramItems meta typeId gamma alpha metaGamma param_metaGamma_at ixArgs transArgs prernd ->
          rec.constructor termBinding paramItems meta typeId gamma alpha metaGamma param_metaGamma_at ixArgs transArgs \prerndSub ->
            prernd
              [ selection ixArgs
                  $ [ WordReactElement punctuation.alt ]
                  <> prerndSub.termBinding
                  <> prerndSub.paramItems
                  <> [ WordReactElement punctuation.mapsto ]
                  <> prerndSub.block
              ]
    }

type RecParameterSeparator
  = Rec.RecParameterSeparator (Prerender Words)

type RecParameterSeparator_Separator
  = Rec.RecParameterSeparator_Separator (Words)

recParameterSeparator ::
  { separator :: RecParameterSeparator_Separator } ->
  RecParameterSeparator
recParameterSeparator rec =
  Rec.recParameterSeparator
    { separator:
        undefined
    }

type RecType
  = Rec.RecType (Prerender Words)

type RecType_Arrow
  = Rec.RecType_Arrow (Words)

type RecType_Data
  = Rec.RecType_Data (Words)

type RecType_Hole
  = Rec.RecType_Hole (Words)

type RecType_ProxyHole
  = Rec.RecType_ProxyHole (Words)

recType ::
  { arrow :: RecType_Arrow
  , data :: RecType_Data
  , hole :: RecType_Hole
  , proxyHole :: RecType_ProxyHole
  } ->
  RecType
recType rec =
  Rec.recType
    { arrow:
        undefined
    , data: undefined
    , hole: undefined
    , proxyHole: undefined
    }

type RecTerm
  = Rec.RecTerm (Prerender Words -> Words)

type RecTerm_Lambda
  = Rec.RecTerm_Lambda (Prerender { termId :: Words, block :: Words } -> Words)

type RecTerm_Neutral
  = Rec.RecTerm_Neutral (Prerender { termId :: Words, argItems :: Words } -> Words)

type RecTerm_Match
  = Rec.RecTerm_Match (Prerender { term :: Words, caseItems :: Words } -> Words)

type RecTerm_Hole
  = Rec.RecTerm_Hole (Prerender { holeId :: Words } -> Words)

recTerm ::
  { lambda :: RecTerm_Lambda
  , neutral :: RecTerm_Neutral
  , match :: RecTerm_Match
  , hole :: RecTerm_Hole
  } ->
  RecTerm
recTerm rec =
  Rec.recTerm
    { lambda:
        \termId block meta gamma prm beta metaGamma ixArgs transArgs prernd ->
          rec.lambda termId block meta gamma prm beta metaGamma ixArgs transArgs \prerndSub ->
            prernd
              [ selection ixArgs
                  $ prerndSub.termId
                  <> [ WordReactElement punctuation.mapsto ]
                  <> prerndSub.block
              ]
    , neutral:
        \termId argItems meta gamma alpha metaGamma ixArgs transArgs prernd ->
          rec.neutral termId argItems meta gamma alpha metaGamma ixArgs transArgs \prerndSub ->
            prernd
              [ selection ixArgs
                  $ prerndSub.termId
                  <> prerndSub.argItems
              ]
    , match:
        \typeId term caseItems meta gamma alpha metaGamma constrIds ixArgs transArgs prernd ->
          rec.match typeId term caseItems meta gamma alpha metaGamma constrIds ixArgs transArgs \prerndSub ->
            prernd
              [ selection ixArgs
                  $ prerndSub.term
                  <> prerndSub.caseItems
              ]
    , hole:
        \meta gamma alpha metaGamma ixArgs transArgs prernd ->
          rec.hole meta gamma alpha metaGamma ixArgs transArgs \prerndSub ->
            prernd
              prerndSub.holeId
    }

type RecArgItems
  = Rec.RecArgItems (Prerender Words)

type RecArgItems_Nil
  = Rec.RecArgItems_Nil (Prerender Words)

type RecArgItems_Cons
  = Rec.RecArgItems_Cons (Prerender Words)

recArgItems ::
  { nil :: RecArgItems_Nil
  , cons :: RecArgItems_Cons
  } ->
  RecArgItems
recArgItems rec =
  Rec.recArgItems
    { nil: undefined
    , cons: undefined
    }

type RecCase
  = Rec.RecCase (Prerender Words)

type RecCase_Case
  = Rec.RecCase_Case (Words)

recCase ::
  { case_ :: RecCase_Case } ->
  RecCase
recCase rec =
  Rec.recCase
    { case_:
        undefined
    }

type RecParameter
  = Rec.RecParameter (Prerender Words -> Words)

type RecParameter_Parameter
  = Rec.RecParameter_Parameter (Prerender { type_ :: Words } -> Words)

recParameter ::
  { parameter :: RecParameter_Parameter } ->
  RecParameter
recParameter rec =
  Rec.recParameter
    { parameter:
        \type_ meta@{ name } gamma metaGamma ixArgs transArgs prernd ->
          rec.parameter type_ meta gamma metaGamma ixArgs transArgs \prerndSub ->
            prernd
              [ selection ixArgs
                  $ [ WordReactElement punctuation.lparen ]
                  <> undefined -- prerender name
                  <> [ WordReactElement punctuation.colon ]
                  <> prerndSub.type_
              ]
    }

type RecTypeBinding
  = Rec.RecTypeBinding (Prerender Words -> Words)

type RecTypeBinding_TypeBinding
  = Rec.RecTypeBinding_TypeBinding (Words -> Words)

recTypeBinding ::
  { typeBinding :: RecTypeBinding_TypeBinding
  } ->
  RecTypeBinding
recTypeBinding rec =
  Rec.recTypeBinding
    { typeBinding:
        \typeId meta@{ name } gamma metaGamma ixArgs transArgs prernd ->
          rec.typeBinding typeId meta gamma metaGamma ixArgs transArgs
            $ prernd [ selection ixArgs undefined ] -- prerender name
    }

type RecTermBinding
  = Rec.RecTermBinding (Prerender Words -> Words)

type RecTermBinding_TermBinding
  = Rec.RecTermBinding_TermBinding (Words -> Words)

recTermBinding ::
  { termBinding :: RecTermBinding_TermBinding
  } ->
  RecTermBinding
recTermBinding rec =
  Rec.recTermBinding
    { termBinding:
        \termId meta@{ name } gamma metaGamma ixArgs transArgs prernd ->
          rec.termBinding termId meta gamma metaGamma ixArgs transArgs
            $ prernd [ selection ixArgs undefined ] -- prerender name
    }

type RecTermId
  = Rec.RecTermId (Prerender Words -> Words)

type RecTermId_TermId
  = Rec.RecTermId_TermId (Words -> Words)

recTermId :: { termId :: RecTermId_TermId } -> RecTermId
recTermId rec =
  Rec.recTermId
    { termId:
        \termId gamma metaGamma ixArgs transArgs prernd ->
          rec.termId termId gamma metaGamma ixArgs transArgs
            $ prernd [ selection ixArgs undefined ] -- prerender name 
    }
