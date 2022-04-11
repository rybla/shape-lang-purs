module Language.Shape.Stlc.IndexMetadata where

import Data.List.Unsafe
import Data.Tuple.Nested
import Data.Variant
import Language.Shape.Stlc.Index
import Language.Shape.Stlc.Syntax
import Prelude
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol)
import Data.Unfoldable (replicate)
import Debug as Debug
import Language.Shape.Stlc.IndexSyntax
import Language.Shape.Stlc.Metadata as Meta
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe as Unsafe

_nonindentable = Proxy :: Proxy "nonindentable"

_indentable_here = Proxy :: Proxy "indentable_here"

_indentable_up = Proxy :: Proxy "indentable_up"

_indentable_down = Proxy :: Proxy "indentable_down"

toggleIndentedMetadataAt :: DownwardIndex -> Syntax -> Syntax
toggleIndentedMetadataAt ix syn = case unsnocDownwardIndex ix of
  Nothing -> syn -- cannot indent at top level
  Just { ix', step } ->
    let
      -- res :: Maybe (Maybe Syntax)
      res :: Variant ( nonindentable :: Unit, indentable_here :: Syntax, indentable_up :: Unit, indentable_down :: IndexStep )
      res = case lookupSyntaxAt ix' syn /\ step of
        SyntaxModule (Module defItems meta) /\ IndexStep StepModule 0 -> inj _nonindentable unit
        SyntaxBlock (Block defItems a meta) /\ IndexStep StepBlock 0 -> inj _indentable_up unit
        SyntaxBlock (Block defItems a meta) /\ IndexStep StepBlock 1 -> inj _indentable_here $ SyntaxBlock (Block defItems a (Meta.toggle Meta._indented meta))
        SyntaxDefinition (TermDefinition x alpha a meta) /\ IndexStep StepTermDefinition 0 -> inj _nonindentable unit
        SyntaxDefinition (TermDefinition x alpha a meta) /\ IndexStep StepTermDefinition 1 -> inj _indentable_here $ SyntaxDefinition (TermDefinition x alpha a (Meta.toggle Meta._indented_type meta))
        SyntaxDefinition (TermDefinition x alpha a meta) /\ IndexStep StepTermDefinition 2 -> inj _indentable_here $ SyntaxDefinition (TermDefinition x alpha a (Meta.toggle Meta._indented_term meta))
        SyntaxDefinition (DataDefinition t constrItems meta) /\ IndexStep StepDataDefinition 0 -> inj _nonindentable unit
        SyntaxDefinition (DataDefinition t constrItems meta) /\ IndexStep StepDataDefinition 1 -> inj _nonindentable unit
        SyntaxConstructor (Constructor x paramItems meta) /\ IndexStep StepConstructor 0 -> inj _nonindentable unit
        SyntaxConstructor (Constructor x paramItems meta) /\ IndexStep StepConstructor 1 -> inj _nonindentable unit
        SyntaxType (ArrowType param beta meta) /\ IndexStep StepArrowType 0 -> inj _indentable_up unit
        SyntaxType (ArrowType param beta meta) /\ IndexStep StepArrowType 1 -> inj _indentable_here $ SyntaxType (ArrowType param beta (Meta.toggle Meta._indented meta))
        SyntaxTerm (LambdaTerm x block meta) /\ IndexStep StepLambdaTerm 0 -> inj _nonindentable unit
        SyntaxTerm (LambdaTerm x block meta) /\ IndexStep StepLambdaTerm 1 -> inj _indentable_down (IndexStep StepBlock 1)
        SyntaxTerm (NeutralTerm x argItems meta) /\ IndexStep StepNeutralTerm 0 -> inj _indentable_up unit
        SyntaxTerm (NeutralTerm x argItems meta) /\ IndexStep StepNeutralTerm 1 -> inj _indentable_up unit
        SyntaxTerm (MatchTerm typeId a caseItems meta) /\ IndexStep StepMatchTerm 0 -> inj _nonindentable unit
        SyntaxTerm (MatchTerm typeId a caseItems meta) /\ IndexStep StepMatchTerm 1 -> inj _indentable_up unit
        SyntaxCase (Case xItems block meta) /\ IndexStep StepCase 0 -> inj _nonindentable unit
        SyntaxCase (Case xItems block meta) /\ IndexStep StepCase 1 -> inj _indentable_down (IndexStep StepBlock 1)
        SyntaxParameter (Parameter alpha meta) /\ IndexStep StepParameter 0 -> inj _indentable_up unit
        -- items
        SyntaxDefinitionItem (def /\ meta) /\ IndexStep StepDefinitionItem 0 -> inj _nonindentable unit
        SyntaxConstructorItem (constr /\ meta) /\ IndexStep StepConstructorItem 0 -> inj _indentable_here $ SyntaxConstructorItem (constr /\ (Meta.toggle Meta._indented meta))
        SyntaxCaseItem (case_ /\ meta) /\ IndexStep StepCaseItem 0 -> inj _indentable_here $ SyntaxCaseItem (case_ /\ (Meta.toggle Meta._indented meta))
        SyntaxParameterItem (param /\ meta) /\ IndexStep StepParameterItem 0 -> inj _indentable_here $ SyntaxParameterItem (param /\ (Meta.toggle Meta._indented meta))
        SyntaxArgItem (arg /\ meta) /\ IndexStep StepArgItem 0 -> inj _indentable_here $ SyntaxArgItem (arg /\ (Meta.toggle Meta._indented meta))
        SyntaxTermIdItem (termId /\ meta) /\ IndexStep StepTermIdItem 0 -> inj _indentable_here $ SyntaxTermIdItem (termId /\ (Meta.toggle Meta._indented meta))
        -- list
        SyntaxList (Cons h t) /\ IndexStep StepCons 0 -> inj _indentable_up unit
        SyntaxList (Cons h t) /\ IndexStep StepCons 1 -> inj _indentable_up unit
        syn /\ step -> unsafeCrashWith $ "toggleIndentedMetadataAt: impossible: " <> show (step /\ syn)
    in
      case_
        # on _nonindentable (\_ -> syn)
        # on _indentable_here (\syn' -> modifySyntaxAt ix' (\_ -> syn') syn)
        # on _indentable_up (\_ -> toggleIndentedMetadataAt ix' syn)
        # on _indentable_down (\step -> toggleIndentedMetadataAt (snocDownwardIndex ix step) syn)
        $ res

-- case res of
--   Nothing -> syn
--   Just Nothing -> toggleIndentedMetadataAt ix' syn
--   Just (Just syn') -> modifySyntaxAt ix' (\_ -> syn') syn
