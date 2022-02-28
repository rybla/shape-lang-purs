module App.State where

import Language.Shape.Stlc.Syntax
import Prelude
import Data.List (List)
import Data.List as List

type State
  = { module_ :: Module
    , mode :: Mode
    , console :: Console
    }

data Mode
  = Normal
  | EditTermName ((TermName -> TermName) -> Module)

type Console
  = List String

initialState :: State
initialState =
  { module_: initialModule
  , mode: Normal
  , console: mempty
  }

initialModule :: Module
initialModule =
  makeModule
    $ List.fromFoldable
        [ makeTermDefinition
            (makeTermUniqueBinding $ freshTermId unit)
            (makeBaseType $ makeHoleType unit)
            (makeNeutralTerm makeHoleTerm)
        ]

logConsole :: String -> State -> State
logConsole msg st = st { console = msg List.: st.console }

-- Module
--   $ List.fromFoldable
--       [ DataDefinition nat_name nat_id
--           $ List.fromFoldable
--               [ Constructor zero_name zero_id List.Nil
--               , Constructor suc_name suc_id $ List.singleton (Tuple n_name (BaseType (DataType nat_id)))
--               ]
--       , (TermDefinition identity_name identity_id)
--           ( ArrowType
--               (List.fromFoldable [ Tuple n_name (BaseType (DataType nat_id)) ])
--               (DataType nat_id)
--           )
--           ( LambdaTerm
--               (List.fromFoldable [ n_id ])
--               -- (Block List.Nil List.Nil (ApplicationTerm n_id List.Nil))
--               ( Block List.Nil List.Nil
--                   ( MatchTerm (DataType nat_id) (ApplicationTerm n_id List.Nil)
--                       ( List.fromFoldable
--                           [ Case
--                               (List.fromFoldable [])
--                               (Block List.Nil List.Nil (ApplicationTerm zero_id List.Nil))
--                           , Case
--                               (List.fromFoldable [ n'_id ])
--                               ( Block List.Nil List.Nil
--                                   (ApplicationTerm suc_id (List.fromFoldable [ NeutralTerm (ApplicationTerm n'_id List.Nil) ]))
--                               )
--                           ]
--                       )
--                   )
--               )
--           )
--       ]
-- where
-- Tuple nat_name nat_id = Tuple (TypeName "Nat") (TypeId 0)
-- Tuple zero_name zero_id = Tuple (TermName "Zero") (TermId 1)
-- Tuple suc_name suc_id = Tuple (TermName "Suc") (TermId 2)
-- Tuple identity_name identity_id = Tuple (TermName "identity") (TermId 3)
-- Tuple n_name n_id = Tuple (TermName "n") (TermId 4)
-- Tuple n'_name n'_id = Tuple (TermName "n") (TermId 5)
