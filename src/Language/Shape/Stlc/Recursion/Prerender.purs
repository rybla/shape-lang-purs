module Language.Shape.Stlc.Recursion.Prerender where

import Data.Either
import Data.Maybe
import Data.Tuple.Nested
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.RenderingAux
import Language.Shape.Stlc.Syntax
import Language.Shape.Stlc.Typing
import Prelude
import Prelude
import Prim hiding (Type)
import Control.Monad.State (State, runState)
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
import React (ReactElement)
import Undefined (undefined)
import Unsafe as Unsafe

type Prerender a
  = a -> Words

type Words
  = Array Word

data Word
  -- = WordKeyword { key :: Symbol }
  -- | WordPunctuation { key :: Symbol }
  = WordReactElement ReactElement
  | WordTermName String
  | WordTypeName String
  | WordNewline Int

-- | Recursion principles for prerendering. "prerendering" is the conversion of
-- the syntax into the following form: a list of lines where each line is a list of words,
-- and each word is a string (or special inline-block)
type RecModule a
  = Rec.RecModule (a)

type RecModule_Module a
  = Rec.RecModule_Module
      (a)

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
  = Rec.RecBlock_Block
      (a)

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
  = Rec.RecDefinitionItems
      ( a
      )

type RecDefinitionItems_DefinitionItems a
  = Rec.RecDefinitionItems_DefinitionItems
      ( a
      )

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
  = Rec.RecDefinitionSeparator_Separator a

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
  = Rec.RecDefinition
      ( a
      )

type RecDefinition_TermDefinition a
  = Rec.RecDefinition_TermDefinition
      ( a
      )

type RecDefinition_DataDefinition a
  = Rec.RecDefinition_DataDefinition
      ( a
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
        undefined
    , data:
        undefined
    }

type RecConstructorSeparator a
  = Rec.RecConstructorSeparator a

type RecConstructorSeparator_Separator a
  = Rec.RecConstructorSeparator_Separator a

recConstructorSeparator ::
  forall a.
  { separator :: RecConstructorSeparator_Separator a } ->
  RecConstructorSeparator a
recConstructorSeparator rec =
  Rec.recConstructorSeparator
    { separator: undefined }

type RecConstructor a
  = Rec.RecConstructor
      ( a
      )

type RecConstructor_Constructor a
  = Rec.RecConstructor_Constructor
      ( a
      )

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
  = Rec.RecParameterSeparator_Separator a

recParameterSeparator ::
  forall a.
  { separator :: RecParameterSeparator_Separator a } ->
  RecParameterSeparator a
recParameterSeparator rec =
  Rec.recParameterSeparator
    { separator:
        undefined
    }

type RecType a
  = Rec.RecType (a)

type RecType_Arrow a
  = Rec.RecType_Arrow
      ( a
      )

type RecType_Data a
  = Rec.RecType_Data
      ( a
      )

type RecType_Hole a
  = Rec.RecType_Hole
      ( a
      )

type RecType_ProxyHole a
  = Rec.RecType_ProxyHole
      ( a
      )

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
        undefined
    , data: undefined
    , hole: undefined
    , proxyHole: undefined
    }

type RecTerm a
  = Rec.RecTerm (Prerender Words -> a)

type RecTerm_Lambda a
  = Rec.RecTerm_Lambda
      (Prerender { termId :: Words, block :: Words } -> a)

type RecTerm_Neutral a
  = Rec.RecTerm_Neutral
      (Words -> a)

type RecTerm_Match a
  = Rec.RecTerm_Match
      (Words -> a)

type RecTerm_Hole a
  = Rec.RecTerm_Hole
      (Words -> a)

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
        \termId block meta gamma prm beta metaGamma ixArgs transArgs pr ->
          rec.lambda termId block meta gamma prm beta metaGamma ixArgs transArgs
            ( \{ termId, block } ->
                pr
                  [ WordReactElement punctuation.lparen
                  -- TODO
                  ]
            )
    , neutral:
        undefined
    , match:
        undefined
    , hole:
        undefined
    }

type RecArgItems a
  = Rec.RecArgItems (a)

type RecArgItems_Nil (a :: Prim.Type)
  = Rec.RecArgItems_Nil a

type RecArgItems_Cons a
  = Rec.RecArgItems_Cons
      ( a
      )

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
  = Rec.RecCase
      ( a
      )

type RecCase_Case a
  = Rec.RecCase_Case
      ( a
      )

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
  = Rec.RecParameter_Parameter
      ( a
      )

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
  = Rec.RecTypeBinding_TypeBinding a

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
  = Rec.RecTermBinding_TermBinding a

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
  = Rec.RecTermId_TermId a

recTermId :: forall a. { termId :: RecTermId_TermId a } -> RecTermId a
recTermId rec = Rec.recTermId { termId: undefined }
