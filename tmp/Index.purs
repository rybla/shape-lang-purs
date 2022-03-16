module Language.Shape.Stlc.Index where

import Data.Tuple.Nested
import Language.Shape.Stlc.Syntax
import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List.Unsafe (List)
import Data.List.Unsafe as List
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (fst, snd)
import Debug as Debug
import Language.Shape.Stlc.Syntax as Syntax
import Partial.Unsafe as Partial
import Undefined (undefined)
import Unsafe (error)
import Unsafe as Unsafe
import Unsafe.Coerce (unsafeCoerce)

-- the list continues on "upward" i.e. toward the most global
-- the head is the most local step
newtype UpwardIndex
  = UpwardIndex (List IndexStep)

-- the list continues on "downward" i.e. toward the most local
-- the head is the global step
newtype DownwardIndex
  =  DownwardIndex (List IndexStep)

data IndexStep
  = Module_Definition Int
  | Block_Definition Int
  | Block_Term
  | TermDefinition_TermBinding
  | TermDefinition_Type
  | TermDefinition_Term
  | DataDefinition_TypeBinding
  | DataDefinition_Constructor Int
  | Constructor_TermBinding
  | Constructor_Parameter Int
  | LambdaTerm_TermId
  | LambdaTerm_Block
  | MatchTerm_Term
  | MatchTerm_Case Int
  | NeutralTerm_TermId
  | NeutralTerm_Args
  | ConsArgs_Term
  | ConsArgs_Args
  | Case_TermId Int
  | Case_Term
  | ArrowType_Parameter
  | ArrowType_Type
  | Parameter_Type

derive instance Generic IndexStep _
instance Show IndexStep where show step = genericShow step
instance Eq IndexStep where eq step step' = genericEq step step' 

pushDownwardIndex :: DownwardIndex -> IndexStep -> DownwardIndex
pushDownwardIndex (DownwardIndex steps) step = DownwardIndex $ List.Cons step steps

pushUpwardIndex :: IndexStep -> UpwardIndex -> UpwardIndex
pushUpwardIndex step (UpwardIndex steps) = UpwardIndex $ List.Cons step steps

toUpwardIndex :: DownwardIndex -> UpwardIndex 
toUpwardIndex (DownwardIndex steps) = UpwardIndex $ List.reverse steps

toDownwardIndex :: UpwardIndex -> DownwardIndex 
toDownwardIndex (UpwardIndex steps) = DownwardIndex $ List.reverse steps

unconsDownwardIndex :: DownwardIndex -> Maybe {step::IndexStep, ix'::DownwardIndex}
unconsDownwardIndex (DownwardIndex steps) = do
  {head: step, tail: steps'} <- List.uncons steps
  Just {step, ix': DownwardIndex steps'}

unconsUpwardIndex :: UpwardIndex -> Maybe {step::IndexStep, ix'::UpwardIndex}
unconsUpwardIndex (UpwardIndex ix) = do
  {head: step, tail: steps'} <- List.uncons ix
  Just {step, ix': UpwardIndex steps'}

infix 5 pushDownwardIndex as :>
infix 5 pushUpwardIndex as <:

modifySyntaxAt :: DownwardIndex -> (Syntax -> Syntax) -> Syntax -> Syntax
modifySyntaxAt ix f syn = case unconsDownwardIndex ix of
  Nothing -> f syn 
  Just {step, ix'} -> case syn /\ step of
    SyntaxModule (Module defs meta) /\ (Module_Definition i_def) -> SyntaxModule $ Module (List.updateAt' i_def (toDefinition $ modifySyntaxAt ix f $ SyntaxDefinition (List.index' defs i_def)) defs) meta
    SyntaxBlock (Block defs a meta) /\ (Block_Definition i_def) -> SyntaxBlock $ Block (List.updateAt' i_def (toDefinition $ modifySyntaxAt ix f $ SyntaxDefinition (List.index' defs i_def)) defs) a meta
    SyntaxBlock (Block defs a meta) /\ Block_Term -> SyntaxBlock $ Block defs (toTerm $ modifySyntaxAt ix f $ SyntaxTerm a) meta
    SyntaxDefinition (TermDefinition termBinding alpha a meta) /\ TermDefinition_TermBinding -> SyntaxDefinition $ TermDefinition (toTermBinding $ modifySyntaxAt ix f $ SyntaxTermBinding termBinding) alpha a meta
    SyntaxDefinition (TermDefinition termBinding alpha a meta) /\ TermDefinition_Type -> SyntaxDefinition $ TermDefinition termBinding (toType $ modifySyntaxAt ix f $ SyntaxType alpha) a meta 
    SyntaxDefinition (TermDefinition termBinding alpha a meta) /\ TermDefinition_Term -> SyntaxDefinition $ TermDefinition termBinding alpha (toTerm $ modifySyntaxAt ix f $ SyntaxTerm a) meta
    SyntaxDefinition (DataDefinition typeBinding constrs meta) /\ DataDefinition_TypeBinding -> SyntaxDefinition $ DataDefinition (toTypeBinding $ modifySyntaxAt ix f $ SyntaxTypeBinding typeBinding) constrs meta
    SyntaxDefinition (DataDefinition typeBinding constrs meta) /\ (DataDefinition_Constructor i_constr) -> SyntaxDefinition $ DataDefinition typeBinding (List.updateAt' i_constr (toConstructor $ modifySyntaxAt ix f $ SyntaxConstructor (List.index' constrs i_constr)) constrs) meta
    SyntaxConstructor (Constructor termBinding prms meta) /\ Constructor_TermBinding -> SyntaxConstructor $ Constructor (toTermBinding $ modifySyntaxAt ix f $ SyntaxTermBinding termBinding) prms meta
    SyntaxConstructor (Constructor termBinding prms meta) /\ (Constructor_Parameter i_prm) -> SyntaxConstructor $ Constructor termBinding (List.updateAt' i_prm (toParameter $ modifySyntaxAt ix f $ SyntaxParameter $ List.index' prms i_prm) prms) meta
    SyntaxTerm (LambdaTerm termId block meta) /\ LambdaTerm_TermId -> SyntaxTerm $ LambdaTerm (toTermId $ modifySyntaxAt ix f $ SyntaxTermId termId) block meta
    SyntaxTerm (LambdaTerm termId block meta) /\ LambdaTerm_Block -> SyntaxTerm $ LambdaTerm termId (toBlock $ modifySyntaxAt ix f $ SyntaxBlock block) meta
    SyntaxTerm (MatchTerm typeId term cases meta) /\ MatchTerm_Term -> SyntaxTerm $ MatchTerm typeId (toTerm $ modifySyntaxAt ix f $ SyntaxTerm term) cases meta
    SyntaxTerm (MatchTerm typeId term cases meta) /\ (MatchTerm_Case i_case) -> SyntaxTerm $ MatchTerm typeId term (List.updateAt' i_case (toCase $ modifySyntaxAt ix f $ SyntaxCase $ List.index' cases i_case) cases) meta
    SyntaxTerm (NeutralTerm termId args meta) /\ NeutralTerm_TermId -> SyntaxTerm $ NeutralTerm (toTermId $ modifySyntaxAt ix f $ SyntaxTermId termId) args meta
    SyntaxTerm (NeutralTerm termId args meta) /\ NeutralTerm_Args -> SyntaxTerm $ NeutralTerm termId (toArgs $ modifySyntaxAt ix f $ SyntaxArgs args) meta
    SyntaxArgs (ConsArgs a args meta) /\ ConsArgs_Term -> SyntaxArgs $ ConsArgs (toTerm $ modifySyntaxAt ix f $ SyntaxTerm a) args meta
    SyntaxArgs (ConsArgs a args meta) /\ ConsArgs_Args -> SyntaxArgs $ ConsArgs a (toArgs $ modifySyntaxAt ix f $ SyntaxArgs args) meta
    SyntaxCase (Case termIds term meta) /\ (Case_TermId i_termId) -> SyntaxCase $ Case (List.updateAt' i_termId (toTermId $ modifySyntaxAt ix f $ SyntaxTermId (List.index' termIds i_termId)) termIds) term meta
    SyntaxCase (Case termIds term meta) /\ Case_Term -> SyntaxCase $ Case termIds (toTerm $ modifySyntaxAt ix f $ SyntaxTerm term) meta
    SyntaxType (ArrowType prm beta meta) /\ ArrowType_Parameter -> SyntaxType $ ArrowType (toParameter $ modifySyntaxAt ix f $ SyntaxParameter prm) beta meta
    SyntaxType (ArrowType prm beta meta) /\ ArrowType_Type -> SyntaxType $ ArrowType prm (toType $ modifySyntaxAt ix f $ SyntaxType beta) meta
    SyntaxParameter (Parameter alpha meta) /\ Parameter_Type -> SyntaxParameter $ Parameter (toType $ modifySyntaxAt ix f $ SyntaxType alpha) meta
    _ -> Unsafe.error "modifySyntaxAt: impossible"

lookupSyntaxAt :: DownwardIndex -> Syntax -> Syntax
lookupSyntaxAt ix syn = case unconsDownwardIndex ix of
  Nothing -> syn
  Just {step, ix'} -> case syn /\ step of
    SyntaxModule (Module defs meta) /\ (Module_Definition i_def) -> lookupSyntaxAt ix $ SyntaxDefinition (List.index' defs i_def)
    SyntaxBlock (Block defs a meta) /\ (Block_Definition i_def) -> lookupSyntaxAt ix $ SyntaxDefinition (List.index' defs i_def)
    SyntaxBlock (Block defs a meta) /\ Block_Term -> lookupSyntaxAt ix $ SyntaxTerm a
    SyntaxDefinition (TermDefinition termBinding alpha a meta) /\ TermDefinition_TermBinding -> lookupSyntaxAt ix $ SyntaxTermBinding termBinding
    SyntaxDefinition (TermDefinition termBinding alpha a meta) /\ TermDefinition_Type -> lookupSyntaxAt ix $ SyntaxType alpha
    SyntaxDefinition (TermDefinition termBinding alpha a meta) /\ TermDefinition_Term -> lookupSyntaxAt ix $ SyntaxTerm a
    SyntaxDefinition (DataDefinition typeBinding constrs meta) /\ DataDefinition_TypeBinding -> lookupSyntaxAt ix $ SyntaxTypeBinding typeBinding
    SyntaxDefinition (DataDefinition typeBinding constrs meta) /\ (DataDefinition_Constructor i_constr) -> lookupSyntaxAt ix $ SyntaxConstructor (List.index' constrs i_constr)
    SyntaxConstructor (Constructor termBinding prms meta) /\ Constructor_TermBinding -> lookupSyntaxAt ix $ SyntaxTermBinding termBinding
    SyntaxConstructor (Constructor termBinding prms meta) /\ (Constructor_Parameter i_prm) -> lookupSyntaxAt ix $ SyntaxParameter $ List.index' prms i_prm
    SyntaxTerm (LambdaTerm termId block meta) /\ LambdaTerm_TermId -> lookupSyntaxAt ix $ SyntaxTermId termId
    SyntaxTerm (LambdaTerm termId block meta) /\ LambdaTerm_Block -> lookupSyntaxAt ix $ SyntaxBlock block
    SyntaxTerm (MatchTerm typeId term cases meta) /\ MatchTerm_Term -> lookupSyntaxAt ix $ SyntaxTerm term
    SyntaxTerm (MatchTerm typeId term cases meta) /\ (MatchTerm_Case i_case) -> lookupSyntaxAt ix $ SyntaxCase $ List.index' cases i_case
    SyntaxTerm (NeutralTerm termId args meta) /\ NeutralTerm_TermId -> lookupSyntaxAt ix $ SyntaxTermId termId
    SyntaxTerm (NeutralTerm termId args meta) /\ NeutralTerm_Args -> lookupSyntaxAt ix $ SyntaxArgs args
    SyntaxArgs (ConsArgs a args meta) /\ ConsArgs_Term -> lookupSyntaxAt ix $ SyntaxTerm a
    SyntaxArgs (ConsArgs a args meta) /\ ConsArgs_Args -> lookupSyntaxAt ix $ SyntaxArgs args
    SyntaxCase (Case termIds term meta) /\ (Case_TermId i_termId) -> lookupSyntaxAt ix $ SyntaxTermId (List.index' termIds i_termId)
    SyntaxCase (Case termIds term meta) /\ Case_Term -> lookupSyntaxAt ix $ SyntaxTerm term
    SyntaxType (ArrowType prm beta meta) /\ ArrowType_Parameter -> lookupSyntaxAt ix $ SyntaxParameter prm
    SyntaxType (ArrowType prm beta meta) /\ ArrowType_Type -> lookupSyntaxAt ix $ SyntaxType beta
    SyntaxParameter (Parameter alpha meta) /\ Parameter_Type -> lookupSyntaxAt ix $ SyntaxType alpha
    _ -> Debug.trace (ix /\ syn) \_ -> Unsafe.error "lookupSyntaxAt: impossible"

data Direction = Up | Down | Left | Right

moveDownwardIndex :: Direction -> Module -> DownwardIndex -> DownwardIndex 
moveDownwardIndex dir mod ix = case dir of 
  Up -> case unconsDownwardIndex ix of 
    Nothing -> ix
    Just {ix'} -> ix'
  Down ->
    case lookupSyntaxAt ix (SyntaxModule mod) of 
      SyntaxModule (Module defs _) -> if List.length defs > 0 then ix :> Module_Definition 0 else ix 
      SyntaxBlock (Block defs a _) -> if List.length defs > 0 then ix :> Block_Definition 0 else ix
      SyntaxDefinition (TermDefinition _ _ _ _) -> ix :> TermDefinition_TermBinding
      SyntaxDefinition (DataDefinition _ _ _) -> ix :> DataDefinition_TypeBinding
      SyntaxConstructor (Constructor _ _ _) -> ix :> Constructor_TermBinding
      SyntaxTerm (LambdaTerm _ _ _) -> ix :> LambdaTerm_TermId
      SyntaxTerm (Syntax.HoleTerm _) -> ix
      SyntaxTerm (MatchTerm _ _ _ _) -> ix :> MatchTerm_Term
      SyntaxTerm (NeutralTerm _ _ _) -> ix :> NeutralTerm_TermId
      SyntaxArgs Syntax.NoneArgs -> ix
      SyntaxArgs (ConsArgs _ _ _) -> ix :> ConsArgs_Term
      SyntaxCase (Case termIds _ _) -> if List.length termIds > 0 then ix :> Case_TermId 0 else ix :> Case_Term
      SyntaxType (ArrowType _ _ _)  -> ix :> ArrowType_Parameter
      SyntaxType (DataType _ _)  -> ix
      SyntaxType (HoleType _ _ _)  -> ix
      SyntaxType (ProxyHoleType _)  -> ix
      SyntaxParameter (Parameter _ _) -> ix :> Parameter_Type
      SyntaxTermBinding termBinding -> ix
      SyntaxTypeBinding typeBinding -> ix
      SyntaxTermId termId -> ix
  Left ->
    case unconsDownwardIndex ix of 
      Nothing -> ix
      Just {step, ix'} ->
        case step of
          Module_Definition i_def ->
            case lookupSyntaxAt ix' (SyntaxModule mod) of 
              SyntaxModule (Module defs _) -> if 0 <= i_def - 1 then ix' :> Module_Definition (i_def - 1) else ix
              _ -> Unsafe.error "moveDownwardIndex.Left: impossible"
          Block_Definition i_def ->
            case lookupSyntaxAt ix' (SyntaxModule mod) of 
              SyntaxBlock (Block defs _ _) -> if 0 <= i_def - 1 then ix' :> Module_Definition (i_def - 1) else ix
              _ -> Unsafe.error "moveDownwardIndex.Left: impossible"
          Block_Term ->
            case lookupSyntaxAt ix' (SyntaxModule mod) of 
              SyntaxBlock (Block defs _ _) -> if 0 < List.length defs then ix' :> Module_Definition (List.length defs - 1) else ix
              _ -> Unsafe.error "moveDownwardIndex.Left: impossible"
          TermDefinition_TermBinding -> ix
          TermDefinition_Type -> ix' :> TermDefinition_TermBinding
          TermDefinition_Term  -> ix' :> TermDefinition_Type
          DataDefinition_TypeBinding -> ix
          DataDefinition_Constructor i_constr -> 
            case lookupSyntaxAt ix' (SyntaxModule mod) of 
              SyntaxDefinition (DataDefinition _ constrs _) ->
                if 0 <= i_constr - 1 then ix' :> DataDefinition_Constructor (i_constr - 1) else ix' :> DataDefinition_TypeBinding
              _ -> Unsafe.error "moveDownwardIndex.Left: impossible"
          Constructor_TermBinding -> ix
          Constructor_Parameter i_prm ->
            case lookupSyntaxAt ix' (SyntaxModule mod) of 
              SyntaxConstructor (Constructor _ prms _) -> if 0 <= i_prm - 1 then ix' :> Constructor_Parameter (i_prm - 1) else ix' :> Constructor_TermBinding
              _ -> Unsafe.error "moveDownwardIndex.Left: impossible"
          LambdaTerm_TermId -> ix
          LambdaTerm_Block -> ix' :> LambdaTerm_TermId
          MatchTerm_Term -> ix
          MatchTerm_Case i_case ->
            case lookupSyntaxAt ix' (SyntaxModule mod) of 
              SyntaxTerm (MatchTerm _ _ cases _) -> if 0 <= i_case - 1 then ix' :> MatchTerm_Case (i_case - 1) else ix' :> MatchTerm_Term
              _ -> Unsafe.error "moveDownwardIndex.Left: impossible"
          NeutralTerm_TermId -> ix
          NeutralTerm_Args -> ix' :> NeutralTerm_TermId
          ConsArgs_Term ->
            case unconsDownwardIndex ix' of
              Just {ix': ix''} ->
                case lookupSyntaxAt ix'' (SyntaxModule mod) of 
                  SyntaxArgs (ConsArgs _ _ _)  -> ix'' :> ConsArgs_Term
                  _ -> ix
              _ -> Unsafe.error "moveDownwardIndex.Left: impossible"
          ConsArgs_Args -> ix' :> ConsArgs_Term
          Case_TermId i_termId -> 
            case lookupSyntaxAt ix' (SyntaxModule mod) of 
              SyntaxCase (Case _ _ _) -> if 0 <= i_termId then ix' :> Case_TermId (i_termId - 1) else ix
              _ -> Unsafe.error "moveDownwardIndex.Left: impossible"
          Case_Term ->
            case lookupSyntaxAt ix' (SyntaxModule mod) of 
              SyntaxCase (Case termIds _ _) -> if 0 < List.length termIds then ix' :> Case_TermId (List.length termIds - 1) else ix
              _ -> Unsafe.error "moveDownwardIndex.Left: impossible"
          ArrowType_Parameter ->
            case unconsDownwardIndex ix' of 
              Just {ix': ix''} ->
                case lookupSyntaxAt ix'' (SyntaxModule mod) of 
                  SyntaxType (ArrowType _ _ _) -> ix'' :> ArrowType_Parameter
                  _ -> Unsafe.error "moveDownwardIndex.Left: impossible"
              _ -> Unsafe.error "moveDownwardIndex.Left: impossible"
          ArrowType_Type -> ix' :> ArrowType_Parameter
          Parameter_Type -> ix' -- goes "up" to parameter
  Right -> Unsafe.error "not implemented: moveDownwardIndex Right"
    case unconsDownwardIndex ix of 
      Nothing -> ix
      Just {step, ix'} ->
        case step of
          Module_Definition i_def ->
            case lookupSyntaxAt ix' (SyntaxModule mod) of 
              SyntaxModule (Module defs _) -> if i_def + 1 < List.length defs then ix' :> Module_Definition (i_def + 1) else ix
              _ -> Unsafe.error "moveDownwardIndex.Right: impossible"
          Block_Definition i_def ->
            case lookupSyntaxAt ix' (SyntaxModule mod) of 
              SyntaxBlock (Block defs _ _) -> if i_def + 1 < List.length defs then ix' :> Module_Definition (i_def + 1) else ix' :> Block_Term
              _ -> Unsafe.error "moveDownwardIndex.Right: impossible"
          Block_Term -> ix
          TermDefinition_TermBinding -> ix' :> TermDefinition_Type
          TermDefinition_Type -> ix' :> TermDefinition_Term
          TermDefinition_Term  -> ix
          DataDefinition_TypeBinding -> 
            case lookupSyntaxAt ix' (SyntaxModule mod) of 
              SyntaxDefinition (DataDefinition _ constrs _) ->
                if 0 < List.length constrs then ix' :> DataDefinition_Constructor 0 else ix
              _ -> Unsafe.error "moveDownwardIndex.Right: impossible"
          DataDefinition_Constructor i_constr -> 
            case lookupSyntaxAt ix' (SyntaxModule mod) of 
              SyntaxDefinition (DataDefinition _ constrs _) ->
                if i_constr + 1 < List.length constrs then ix' :> DataDefinition_Constructor (i_constr + 1) else ix
              _ -> Unsafe.error "moveDownwardIndex.Right: impossible"
          Constructor_TermBinding -> 
            case lookupSyntaxAt ix' (SyntaxModule mod) of 
              SyntaxConstructor (Constructor _ prms _) -> if 0 < List.length prms then ix' :> Constructor_Parameter 0 else ix
              _ -> Unsafe.error "moveDownwardIndex.Right: impossible"
          Constructor_Parameter i_prm ->
            case lookupSyntaxAt ix' (SyntaxModule mod) of 
              SyntaxConstructor (Constructor _ prms _) -> if i_prm + 1 < List.length prms then ix' :> Constructor_Parameter (i_prm + 1) else ix
              _ -> Unsafe.error "moveDownwardIndex.Right: impossible"
          LambdaTerm_TermId -> ix' :> LambdaTerm_Block
          LambdaTerm_Block -> ix
          MatchTerm_Term -> 
            case lookupSyntaxAt ix' (SyntaxModule mod) of 
              SyntaxTerm (MatchTerm _ _ cases _) -> if 0 < List.length cases then ix' :> MatchTerm_Case 0 else ix
              _ -> Unsafe.error "moveDownwardIndex.Right: impossible"
          MatchTerm_Case i_case ->
            case lookupSyntaxAt ix' (SyntaxModule mod) of 
              SyntaxTerm (MatchTerm _ _ cases _) -> if i_case + 1 < List.length cases then ix' :> MatchTerm_Case (i_case + 1) else ix
              _ -> Unsafe.error "moveDownwardIndex.Right: impossible"
          NeutralTerm_TermId -> ix' :> NeutralTerm_Args
          NeutralTerm_Args -> ix
          ConsArgs_Term -> ix' :> ConsArgs_Args
          ConsArgs_Args -> 
            case lookupSyntaxAt ix (SyntaxModule mod) of 
              SyntaxArgs (ConsArgs _ (ConsArgs _ _ _) _) -> ix :> ConsArgs_Term -- goes down into next ConsArgs
              SyntaxArgs (ConsArgs _ NoneArgs _) -> ix -- cannot go down to NoneArgs
              _ -> Unsafe.error "moveDownwardIndex.Right: impossible"
          Case_TermId i_termId -> 
            case lookupSyntaxAt ix' (SyntaxModule mod) of 
              SyntaxCase (Case termIds _ _) -> if i_termId + 1 < List.length termIds then ix' :> Case_TermId (i_termId + 1) else ix' :> Case_Term
              _ -> Unsafe.error "moveDownwardIndex.Right: impossible"
          Case_Term -> ix
          ArrowType_Parameter -> ix' :> ArrowType_Type
          ArrowType_Type ->
            case lookupSyntaxAt ix (SyntaxModule mod) of 
              SyntaxType (ArrowType _ _ _) -> ix :> ArrowType_Parameter
              SyntaxType _ -> ix
              _ -> Unsafe.error "moveDownwardIndex.Right: impossible"
          Parameter_Type -> ix' -- goes "up" to parameter