module Language.Shape.Stlc.IndexMetadata where

import Data.List.Unsafe
import Data.Tuple.Nested
import Language.Shape.Stlc.Syntax
import Prelude
import Language.Shape.Stlc.Index
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol)
import Data.Unfoldable (replicate)
import Debug as Debug
import Language.Shape.Stlc.Metadata as Meta
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe as Unsafe

toggleIndentedMetadataAt :: DownwardIndex -> Syntax -> Syntax
toggleIndentedMetadataAt ix syn = case unsnocDownwardIndex ix of
  Nothing -> syn -- cannot indent at top level
  Just { ix', step } ->
    let
      res :: Maybe (Either Syntax Syntax)
      res = case lookupSyntaxAt ix' syn /\ step of
        SyntaxModule (Module defItems meta) /\ IndexStep StepModule 0 -> Nothing
        SyntaxBlock (Block defItems a meta) /\ IndexStep StepBlock 1 -> Nothing
        SyntaxBlock (Block defItems a meta) /\ IndexStep StepBlock 0 -> pure <<< Left $ toggleIndentedMetadataAt ix' syn
        SyntaxDefinition (TermDefinition x alpha a meta) /\ IndexStep StepTermDefinition 0 -> pure <<< pure $ SyntaxDefinition (TermDefinition x alpha a (Meta.toggle Meta._indented meta))
        SyntaxDefinition (TermDefinition x alpha a meta) /\ IndexStep StepTermDefinition 1 -> pure <<< pure $ SyntaxDefinition (TermDefinition x alpha a (Meta.toggle Meta._indented meta))
        SyntaxDefinition (TermDefinition x alpha a meta) /\ IndexStep StepTermDefinition 2 -> pure <<< pure $ SyntaxDefinition (TermDefinition x alpha a (Meta.toggle Meta._indented meta))
        SyntaxDefinition (DataDefinition t constrItems meta) /\ IndexStep StepDataDefinition 0 -> Nothing
        SyntaxDefinition (DataDefinition t constrItems meta) /\ IndexStep StepDataDefinition 1 -> Nothing
        SyntaxConstructor (Constructor x paramItems meta) /\ IndexStep StepConstructor 0 -> Nothing
        SyntaxConstructor (Constructor x paramItems meta) /\ IndexStep StepConstructor 1 -> Nothing
        SyntaxType (ArrowType param beta meta) /\ IndexStep StepArrowType 0 -> pure <<< Left $ toggleIndentedMetadataAt ix' syn
        SyntaxType (ArrowType param beta meta) /\ IndexStep StepArrowType 1 -> pure <<< pure $ SyntaxType (ArrowType param beta (Meta.toggle Meta._indented meta))
        SyntaxTerm (LambdaTerm x block meta) /\ IndexStep StepLambdaTerm 0 -> pure <<< pure $ SyntaxTerm (LambdaTerm x block (Meta.toggle Meta._indented meta))
        SyntaxTerm (LambdaTerm x block meta) /\ IndexStep StepLambdaTerm 1 -> pure <<< pure $ SyntaxTerm (LambdaTerm x block (Meta.toggle Meta._indented meta))
        SyntaxTerm (NeutralTerm x argItems meta) /\ IndexStep StepNeutralTerm 0 -> pure <<< Left $ toggleIndentedMetadataAt ix' syn
        SyntaxTerm (NeutralTerm x argItems meta) /\ IndexStep StepNeutralTerm 1 -> pure <<< Left $ toggleIndentedMetadataAt ix' syn
        SyntaxTerm (MatchTerm typeId a caseItems meta) /\ IndexStep StepMatchTerm 0 -> pure <<< Left $ toggleIndentedMetadataAt ix' syn
        SyntaxTerm (MatchTerm typeId a caseItems meta) /\ IndexStep StepMatchTerm 1 -> pure <<< Left $ toggleIndentedMetadataAt ix' syn
        SyntaxCase (Case xItems block meta) /\ IndexStep StepCase 0 -> pure <<< pure $ SyntaxCase (Case xItems block (Meta.toggle Meta._indented meta))
        SyntaxCase (Case xItems block meta) /\ IndexStep StepCase 1 -> pure <<< pure $ SyntaxCase (Case xItems block (Meta.toggle Meta._indented meta))
        SyntaxParameter (Parameter alpha meta) /\ IndexStep StepParameter 0 -> pure <<< Left $ toggleIndentedMetadataAt ix' syn
        -- items
        SyntaxDefinitionItem (def /\ meta) /\ IndexStep StepDefinitionItem 0 -> Nothing
        SyntaxConstructorItem (constr /\ meta) /\ IndexStep StepConstructorItem 0 -> pure <<< pure $ SyntaxConstructorItem (constr /\ (Meta.toggle Meta._indented meta))
        SyntaxCaseItem (case_ /\ meta) /\ IndexStep StepCaseItem 0 -> pure <<< pure $ SyntaxCaseItem (case_ /\ (Meta.toggle Meta._indented meta))
        SyntaxParameterItem (param /\ meta) /\ IndexStep StepParameterItem 0 -> pure <<< pure $ SyntaxParameterItem (param /\ (Meta.toggle Meta._indented meta))
        SyntaxArgItem (arg /\ meta) /\ IndexStep StepArgItem 0 -> pure <<< pure $ SyntaxArgItem (arg /\ (Meta.toggle Meta._indented meta))
        SyntaxTermIdItem (termId /\ meta) /\ IndexStep StepTermIdItem 0 -> pure <<< pure $ SyntaxTermIdItem (termId /\ (Meta.toggle Meta._indented meta))
        -- list
        SyntaxList (Cons h t) /\ IndexStep StepCons 0 -> pure <<< Left $ toggleIndentedMetadataAt ix' syn
        SyntaxList (Cons h t) /\ IndexStep StepCons 1 -> pure <<< Left $ toggleIndentedMetadataAt ix' syn
        syn /\ step -> unsafeCrashWith $ "toggleIndentedMetadataAt: impossible: " <> show (step /\ syn)
    in
      case res of
        Nothing -> syn
        Just (Left syn') -> syn'
        Just (Right syn') -> modifySyntaxAt ix' (\_ -> syn') syn
