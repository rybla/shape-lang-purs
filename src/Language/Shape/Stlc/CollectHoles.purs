module Language.Shape.Stlc.CollectHoles where

import Language.Shape.Stlc.Syntax
import Prelude
import Data.List.Unsafe 

type Holes
  = List HoleId

concatHoles :: List Holes -> Holes
concatHoles = foldl appendHoles Nil

appendHoles :: Holes -> Holes -> Holes 
appendHoles holes1 holes2 = foldl (\holes1' holeId -> if not (holeId `elem` holes1') then holeId : holes1' else holes1') holes1 holes2

collectHoles :: Module -> Holes
collectHoles (Module defItems _) = concatHoles <<< fromFoldable $ (goDef <<< fromItem) <$> defItems
  where
  goDef = case _ of
    TermDefinition _ type_ term _ -> concatHoles <<< fromFoldable $ [ goType type_, goTerm term :: Holes ]
    DataDefinition _ constrItems _ -> concatHoles <<< fromFoldable $ (goConstr <<< fromItem) <$> constrItems

  goConstr (Constructor _ paramItems _) = concatHoles <<< fromFoldable $ (goParam <<< fromItem) <$> paramItems

  goParam (Parameter type_ _) = goType type_

  goType = case _ of
    ArrowType param type_ _ -> concatHoles <<< fromFoldable $ [ goParam param, goType type_ ]
    DataType _ _ -> Nil
    HoleType holeId _ _ -> singleton holeId
    ProxyHoleType holeId -> singleton holeId

  goTerm = case _ of
    LambdaTerm _ block _ -> goBlock block
    NeutralTerm _ argItems _ -> concatHoles <<< fromFoldable $ (goTerm <<< fromItem) <$> argItems
    MatchTerm _ term caseItems _ -> concatHoles <<< fromFoldable $ singleton (goTerm term) <> ((goCase <<< fromItem) <$> caseItems)
    HoleTerm _ -> Nil

  goCase (Case _ block _) = goBlock block

  goBlock (Block defItems term _) = concatHoles <<< fromFoldable $ ((goDef <<< fromItem) <$> defItems) <> singleton (goTerm term)

