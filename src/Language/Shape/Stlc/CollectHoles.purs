module Language.Shape.Stlc.CollectHoles where

import Language.Shape.Stlc.Syntax
import Prelude
import Data.List.Unsafe as List
import Data.Set (Set)
import Data.Set as Set

type Holes
  = Set HoleId

collectHoles :: Module -> Holes
collectHoles _ = Set.empty
-- collectHoles (Module defItems _) = Set.unions $ (goDef <<< fromItem) <$> defItems
--   where
--   goDef = case _ of
--     TermDefinition _ type_ term _ -> Set.unions [ goType type_, goTerm term :: Holes ]
--     DataDefinition _ constrItems _ -> Set.unions $ (goConstr <<< fromItem) <$> constrItems

--   goConstr (Constructor _ paramItems _) = Set.unions $ (goParam <<< fromItem) <$> paramItems

--   goParam (Parameter type_ _) = goType type_

--   goType = case _ of
--     ArrowType param type_ _ -> Set.unions [ goParam param, goType type_ ]
--     DataType _ _ -> Set.empty
--     HoleType holeId _ _ -> Set.singleton holeId
--     ProxyHoleType holeId -> Set.singleton holeId

--   goTerm = case _ of
--     LambdaTerm _ block _ -> goBlock block
--     NeutralTerm _ argItems _ -> Set.unions $ (goTerm <<< fromItem) <$> argItems
--     MatchTerm _ term caseItems _ -> Set.unions $ List.singleton (goTerm term) <> ((goCase <<< fromItem) <$> caseItems)
--     HoleTerm _ -> Set.empty

--   goCase (Case _ block _) = goBlock block

--   goBlock (Block _ term _) = Set.unions $ ((goDef <<< fromItem) <$> defItems) <> List.singleton (goTerm term)
