module Language.Shape.Stlc.Index where

import Data.Array.Unsafe
import Data.Tuple.Nested
import Language.Shape.Stlc.Syntax
import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List.Unsafe as List
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (fst, snd)
import Language.Shape.Stlc.Syntax as Syntax
import Partial.Unsafe as Partial
import Undefined (undefined)
import Unsafe (error)
import Unsafe as Unsafe
import Unsafe.Coerce (unsafeCoerce)

type Index
  = Array IndexStep

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
  | HoleTerm
  | MatchTerm_Term
  | MatchTerm_Case Int
  | NeutralTerm_TermId
  | NeutralTerm_Args
  | NoneArgs
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

pushIndex :: Index -> IndexStep -> Index
pushIndex = snoc

infix 5 pushIndex as :>

-- returns new syntax and module with new syntax updated in it
visitSyntaxAt :: Index -> (Syntax -> Syntax) -> Module -> Syntax /\ Module
visitSyntaxAt ix f mod = goModule 0 mod
  where 
  l = length ix 

  visit :: forall a. Int -> a -> (a -> Syntax) -> (Syntax -> a) -> (a -> Module) -> (IndexStep -> Syntax /\ Module) -> Syntax /\ Module
  visit i a toSyntax fromSyntax wrap k = if i == l then syntax /\ wrap (fromSyntax syntax) else k (index' ix i)
    where syntax = f $ toSyntax a 

  goModule i mod@(Module defs meta) = visit i mod SyntaxModule toModule identity
    case _ of 
      Module_Definition i_def -> goDefinition (i + 1) (List.index' defs i_def) \def' -> Module (List.updateAt' i_def def' defs) meta
      _ -> Unsafe.error "impossible"

  goBlock i block@(Block defs a meta) wrap = visit i block SyntaxBlock toBlock wrap
    case _ of 
      Block_Definition i_def -> goDefinition (i + 1) (List.index' defs i_def) \def' -> wrap $ Block (List.updateAt' i_def def' defs) a meta
      _ -> Unsafe.error "impossible"

  goDefinition i def wrap = case def of 
    TermDefinition termBinding alpha a meta -> visit i def SyntaxDefinition toDefinition wrap
      case _ of 
        TermDefinition_TermBinding -> goTermBinding (i + 1) termBinding \termBinding' -> wrap $ TermDefinition termBinding' alpha a meta
        TermDefinition_Type -> goType (i + 1) alpha \alpha' -> wrap $ TermDefinition termBinding alpha' a meta
        TermDefinition_Term -> goTerm (i + 1) a \a' -> wrap $ TermDefinition termBinding alpha a' meta
        _ -> Unsafe.error "impossible"
    DataDefinition typeBinding constrs meta -> visit i def SyntaxDefinition toDefinition wrap
      case _ of 
        DataDefinition_TypeBinding -> goTypeBinding (i + 1) typeBinding \typeBinding' -> wrap $ DataDefinition typeBinding' constrs meta
        DataDefinition_Constructor i_constr -> goConstructor (i + 1) (List.index' constrs i_constr) \constr' -> wrap $ DataDefinition typeBinding (List.updateAt' i_constr constr' constrs) meta
        _ -> Unsafe.error "impossible"

  goConstructor i constr@(Constructor termBinding prms meta) wrap = visit i constr SyntaxConstructor toConstructor wrap
    case _ of 
      Constructor_TermBinding -> goTermBinding (i + 1) termBinding \termBinding' -> wrap $ Constructor termBinding' prms meta
      Constructor_Parameter i_prm -> goParameter (i + 1) (List.index' prms i_prm) \prm' -> wrap $ Constructor termBinding (List.updateAt' i_prm prm' prms) meta
      _ -> Unsafe.error "impossible"
  
  goType i type_@(ArrowType prm beta meta) wrap = visit i type_ SyntaxType toType wrap 
    case _ of
      ArrowType_Parameter -> goParameter (i + 1) prm \prm' -> wrap $ ArrowType prm' beta meta
      ArrowType_Type -> goType (i + 1) beta \beta' -> wrap $ ArrowType prm beta' meta 
      _ -> Unsafe.error "impossible"
  
  goType i type_@(DataType typeId meta) wrap = visit i type_ SyntaxType toType wrap
    case _ of 
      _ -> Unsafe.error "impossible"
  
  goType i type_@(HoleType holeId wkn meta) wrap = visit i type_ SyntaxType toType wrap
    case _ of
      _ -> Unsafe.error "impossible"
  
  goType i type_@(ProxyHoleType holeId) wrap = visit i type_ SyntaxType toType wrap 
    case _ of 
      _ -> Unsafe.error "impossible"
  
  goTerm i term wrap = case term of 
    LambdaTerm termId block meta -> visit i term SyntaxTerm toTerm wrap
      case _ of 
        LambdaTerm_TermId -> goTermId (i + 1) termId \termId' -> wrap $ LambdaTerm termId' block meta
        LambdaTerm_Block -> goBlock (i + 1) block \block' -> wrap $ LambdaTerm termId block' meta 
        _ -> Unsafe.error "impossible"
    NeutralTerm termId args meta -> visit i term SyntaxTerm toTerm wrap 
      case _ of 
        NeutralTerm_TermId -> goTermId (i + 1) termId \termId' -> wrap $ NeutralTerm termId' args meta
        NeutralTerm_Args -> goArgs (i + 1) args \args' -> wrap $ NeutralTerm termId args' meta
        _ -> Unsafe.error "impossible"
    MatchTerm typeId term cases meta -> visit i term SyntaxTerm toTerm wrap 
      case _ of 
        MatchTerm_Term -> goTerm (i + 1) term \term' -> wrap $ MatchTerm typeId term' cases meta
        MatchTerm_Case i_case -> goCase (i + 1) (List.index' cases i_case) \case' -> wrap $ MatchTerm typeId term (List.updateAt' i_case case' cases) meta 
        _ -> Unsafe.error "impossible"
    Syntax.HoleTerm meta -> visit i term SyntaxTerm toTerm wrap 
      case _ of
        _ -> Unsafe.error "impossible"
  
  goParameter i prm@(Parameter alpha meta) wrap = visit i prm SyntaxParameter toParameter wrap 
    case _ of 
      Parameter_Type -> goType (i + 1) alpha \alpha' -> wrap $ Parameter alpha' meta
      _ -> Unsafe.error "impossible"
  
  goCase i case_@(Case termIds term meta) wrap = visit i case_ SyntaxCase toCase wrap 
    case _ of 
      Case_TermId i_termId -> goTermId (i + 1) (List.index' termIds i_termId) \termId' -> wrap $ Case (List.updateAt' i_termId termId' termIds) term meta
      Case_Term -> goTerm (i + 1) term \term' -> wrap $ Case termIds term' meta
      _ -> Unsafe.error "impossible"
  
  goArgs i args wrap = case args of 
    ConsArgs a args meta -> visit i args SyntaxArgs toArgs wrap
      case _ of 
        ConsArgs_Term -> goTerm (i + 1) a \a' -> wrap $ ConsArgs a' args meta
        ConsArgs_Args -> goArgs (i + 1) args \args' -> wrap $ ConsArgs a args' meta
        _ -> Unsafe.error "impossible"
    Syntax.NoneArgs -> Unsafe.error "impossible"
  
  goTermBinding i termBinding wrap = visit i termBinding SyntaxTermBinding toTermBinding wrap 
    case _ of 
      _ -> Unsafe.error "impossible"
  
  goTypeBinding i typeBinding wrap = visit i typeBinding SyntaxTypeBinding toTypeBinding wrap  
    case _ of 
      _ -> Unsafe.error "impossible"
  
  goTermId i termId wrap = visit i termId SyntaxTermId toTermId wrap
    case _ of 
      _ -> Unsafe.error "impossible"

getSyntaxAt :: Index -> Module -> Syntax
getSyntaxAt ix mod = fst $ visitSyntaxAt ix identity mod 

modifySyntaxAt :: Index -> (Syntax -> Syntax) -> Module -> Module 
modifySyntaxAt ix f mod = snd $ visitSyntaxAt ix identity mod

data Direction = Up | Down | Left | Right

moveIndex :: Direction -> Index -> Index 
moveIndex dir ix = ix 

moveIndexUp :: Module -> Index -> Index 
moveIndexUp _ ix = case unsnoc ix of 
  Nothing -> []
  Just {init: ix'} -> ix' 

moveIndexLeft :: Module -> Index -> Index 
moveIndexLeft mod ix = ix 

moveIndexRight :: Module -> Index -> Index 
moveIndexRight mod ix = ix 

moveIndexDown :: Module -> Index -> Index 
moveIndexDown mod ix = ix 