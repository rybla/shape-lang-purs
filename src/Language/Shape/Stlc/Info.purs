module Language.Shape.Stlc.Info where

import Control.Alt
import Data.Tuple
import Data.Tuple.Nested
import Language.Shape.Stlc.RenderingTypes
import Language.Shape.Stlc.Syntax
import Prelude
import Prim hiding (Type)

import Data.List.Unsafe (List, mapWithIndex)
import Data.List.Unsafe as List
import Data.Maybe (Maybe(..))
import Language.Shape.Stlc.Recursion.Action as RecAction
import Language.Shape.Stlc.Typing (Context)
import Undefined (undefined)

type Info
  = { syntax :: Maybe Syntax
    , actions :: Actions
    , gamma :: Maybe Context
    , type_ :: Maybe Type
    }

getInfoHelper_Syntax :: forall r. { isSelected :: Boolean | r } -> { syntax :: Syntax, actions :: Actions, gamma :: Context, type_ :: Maybe Type } -> Maybe Info -> Maybe Info
getInfoHelper_Syntax { isSelected } args mb_info =
  if isSelected then
    Just { syntax: Just args.syntax, actions: args.actions, gamma: Just args.gamma, type_: args.type_ }
  else
    mb_info

getInfoHelper_Separator :: forall r. { isSelected :: Boolean | r } -> { actions :: Actions } -> Maybe Info
getInfoHelper_Separator { isSelected } { actions } =
  if isSelected then
    Just
      { syntax: Nothing
      , actions
      , gamma: Nothing
      , type_: Nothing
      }
  else
    Nothing

getInfoAtModule :: RecAction.RecModule (Maybe Info)
getInfoAtModule =
  RecAction.recModule
    { module_:
        \defItems meta gamma metaGamma ixArgs trans this actions ->
          getInfoHelper_Syntax ixArgs
            { syntax: SyntaxModule (Module defItems meta), actions, gamma, type_: Nothing }
            $ getInfoAtDefinitionItems defItems gamma metaGamma { ix_parentBlock: ixArgs.ix, ix: ixArgs.ix_defItems, csr: ixArgs.csr_defItems } this
    }

getInfoAtBlock :: RecAction.RecBlock (Maybe Info)
getInfoAtBlock =
  RecAction.recBlock
    { block:
        \defItems term meta gamma alpha metaGamma ixArgs trans this actions ->
          getInfoHelper_Syntax ixArgs
            { syntax: SyntaxBlock (Block defItems term meta), actions, gamma, type_: Just alpha }
            $ getInfoAtDefinitionItems defItems gamma metaGamma { ix_parentBlock: ixArgs.ix, ix: ixArgs.ix_defItems, csr: ixArgs.csr_defItems } this
    }

getInfoAtDefinitionItems :: RecAction.RecDefinitionItems (Maybe Info)
getInfoAtDefinitionItems =
  RecAction.recDefinitionItems
    { definitionItems:
        \defItems gamma metaGamma ixArgs trans this actions ->
          getInfoHelper_Syntax ixArgs
            { syntax: SyntaxList (SyntaxDefinition <<< fromItem <$> defItems), actions, gamma, type_: Nothing }
            $ List.foldl (<|>) Nothing
                ( mapWithIndex
                    ( \i (def /\ _) ->
                        getInfoAtDefinition def gamma metaGamma { ix_parentBlock: ixArgs.ix_parentBlock, ix: ixArgs.ix_def_at i, csr: ixArgs.csr_def_at i } this
                          <|> getInfoAtDefinitionSeparator { ix_parentBlock: ixArgs.ix_parentBlock, ix: ixArgs.ix_defSep_at i, csr: ixArgs.csr_defSep_at i } this
                    )
                    defItems
                )
            <|> getInfoAtDefinitionSeparator { ix_parentBlock: ixArgs.ix_parentBlock, ix: ixArgs.ix_defSep_at (List.length defItems), csr: ixArgs.csr_defSep_at (List.length defItems) } this
    }

getInfoAtDefinitionSeparator :: RecAction.RecDefinitionSeparator (Maybe Info)
getInfoAtDefinitionSeparator =
  RecAction.recDefinitionSeparator
    { separator: \ixArgs trans this actions -> getInfoHelper_Separator ixArgs { actions } }

getInfoAtDefinition :: RecAction.RecDefinition (Maybe Info)
getInfoAtDefinition =
  RecAction.recDefinition
    { term:
        \termBinding type_ term meta gamma metaGamma ixArgs trans this actions ->
          getInfoHelper_Syntax ixArgs
            { syntax: SyntaxDefinition (TermDefinition termBinding type_ term meta), actions, gamma, type_: Nothing }
            ( getInfoAtType type_ gamma metaGamma.metaGamma_children { ix: ixArgs.ix_type, csr: ixArgs.csr_type } this
                <|> getInfoAtTerm term gamma type_ metaGamma.metaGamma_children { ix: ixArgs.ix_term, csr: ixArgs.csr_term } this
            )
    , data:
        \(typeBinding@(TypeBinding typeId _)) constrItems meta gamma metaGamma ixArgs trans this actions ->
          getInfoHelper_Syntax ixArgs
            { syntax: SyntaxDefinition (DataDefinition typeBinding constrItems meta), actions, gamma, type_: Nothing }
            (List.foldl (<|>) Nothing $ mapWithIndex (\i (constr /\ _) -> getInfoAtConstructor constr typeId gamma metaGamma.metaGamma_children { ix_parentBlock: ixArgs.ix_parentBlock, ix_parentDef: ixArgs.ix, ix: ixArgs.ix_constr_at i, csr: ixArgs.csr_constr_at i } this) constrItems)
    }

getInfoAtConstructor :: RecAction.RecConstructor (Maybe Info)
getInfoAtConstructor =
  RecAction.recConstructor
    { constructor:
        \termBinding paramItems meta typeId gamma alpha metaGamma metaGamma_param_t ixArgs trans this actions ->
          getInfoHelper_Syntax ixArgs
            { syntax: SyntaxConstructor (Constructor termBinding paramItems meta), actions, gamma, type_: Nothing }
            ( List.foldl (<|>) Nothing
                $ mapWithIndex
                    ( \i (param /\ _) ->
                        getInfoAtParameter param gamma metaGamma { ix: ixArgs.ix_param_at i, csr: ixArgs.csr_param_at i } this
                          <|> getInfoAtParameterSeparator { ix_parentBlock: ixArgs.ix_parentBlock, ix_parentDef: ixArgs.ix_parentDef, ix_parentConstr: ixArgs.ix, ix: ixArgs.ix_paramSep_at i, csr: ixArgs.csr_paramSep_at i } this
                    )
                    paramItems
            )
    }

getInfoAtConstructorSeparator :: RecAction.RecConstructorSeparator (Maybe Info)
getInfoAtConstructorSeparator =
  RecAction.recConstructorSeparator
    { separator: \ixArgs trans this actions -> getInfoHelper_Separator ixArgs { actions } }

getInfoAtType :: RecAction.RecType (Maybe Info)
getInfoAtType =
  RecAction.recType
    { arrow:
        \prm beta meta gamma metaGamma ixArgs trans this actions ->
          getInfoHelper_Syntax ixArgs
            { syntax: SyntaxType (ArrowType prm beta meta), gamma, actions, type_: Nothing }
            ( getInfoAtParameter prm gamma metaGamma { ix: ixArgs.ix_param, csr: ixArgs.csr_param } this
                <|> getInfoAtType beta gamma metaGamma { ix: ixArgs.ix_type, csr: ixArgs.csr_type } this
            )
    , data:
        \typeId meta gamma metaGamma ixArgs trans this actions ->
          getInfoHelper_Syntax ixArgs
            { syntax: SyntaxType (DataType typeId meta), gamma, actions, type_: Nothing }
            Nothing
    , hole:
        \holeId wkn meta gamma metaGamma ixArgs trans this actions ->
          getInfoHelper_Syntax ixArgs
            { syntax: SyntaxType (HoleType holeId wkn meta), gamma, actions, type_: Nothing }
            Nothing
    , proxyHole: undefined
    }

getInfoAtTerm :: RecAction.RecTerm (Maybe Info)
getInfoAtTerm = 
  RecAction.recTerm
      { lambda:
          \termId block meta gamma param beta metaGamma ixArgs trans this actions ->
            undefined
      , neutral:
          \termId argItems meta gamma alpha metaGamma ixArgs trans this actions ->
            undefined
      , match:
          \typeId term caseItems meta gamma alpha metaGamma constrIds ixArgs trans this actions ->
            undefined
      , hole:
          \meta gamma alpha metaGamma ixArgs trans this actions ->
            undefined
      }

getInfoAtArgItems :: RecAction.RecArgItems (Maybe Info)
getInfoAtArgItems = undefined

getInfoAtCase :: RecAction.RecCase (Maybe Info)
getInfoAtCase = undefined

getInfoAtParameter :: RecAction.RecParameter (Maybe Info)
getInfoAtParameter = undefined

getInfoAtParameterSeparator :: RecAction.RecParameterSeparator (Maybe Info)
getInfoAtParameterSeparator = undefined

getInfoAtTypeBinding :: RecAction.RecTypeBinding (Maybe Info)
getInfoAtTypeBinding = undefined

getInfoAtTermBinding :: RecAction.RecTermBinding (Maybe Info)
getInfoAtTermBinding = undefined

getInfoAtTermId :: RecAction.RecTermId (Maybe Info)
getInfoAtTermId = undefined
