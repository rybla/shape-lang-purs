module Language.Shape.Stlc.Initial where

import Prelude
import Data.List
import Data.Tuple.Nested
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Prelude
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect (Effect)

makeTypeVar :: String -> TypeId /\ TypeName
makeTypeVar label = freshTypeId unit /\ TypeName (Just label)

makeTermVar :: String -> TermId /\ TermName
makeTermVar label = freshTermId unit /\ TermName (Just label)

module_ :: Module
module_ =
  let
    nat_id /\ nat_name = makeTypeVar "Nat"

    zero_id /\ zero_name = makeTermVar "zero"

    suc_id /\ suc_name = makeTermVar "suc"

    add_id /\ add_name = makeTermVar "add"
  in
    mkModule
      ( mkItems
          [ let -- Nat
              _ /\ n_name = makeTermVar "n"
            in
              mkDefItem $ mkDataDef (mkTypeBind nat_id nat_name)
                $ mkItems
                    [ mkConstrItem $ mkConstr (mkTermBind zero_id zero_name)
                        $ mkItems []
                    , mkConstrItem $ mkConstr (mkTermBind suc_id suc_name)
                        $ mkItems [ mkParamItem $ mkParam n_name (mkData nat_id) ]
                    ]
          , let -- add
              m_id /\ m_name = makeTermVar "m"

              m'_id /\ m'_name = makeTermVar "m'"

              n_id /\ n_name = makeTermVar "n"
            in
              mkDefItem
                $ mkTermDef (mkTermBind add_id add_name)
                    (mkParam m_name (mkData nat_id) `mkArrow` (mkParam n_name (mkData nat_id) `mkArrow` mkData nat_id))
                    ( mkLambda m_id $ mkBlock (mkItems []) $ mkLambda n_id $ mkBlock (mkItems []) $ mkMatch nat_id (mkNeutral m_id (mkItems []))
                        $ mkItems
                            [ mkCaseItem $ mkCase (mkItems []) $ mkBlock (mkItems []) $ mkNeutral n_id (mkItems [])
                            , mkCaseItem $ mkCase (mkItems [ mkTermIdItem m'_id ]) $ mkBlock (mkItems [])
                                $ mkNeutral suc_id
                                $ mkItems
                                    [ mkArgItem
                                        $ mkNeutral add_id
                                        $ mkItems
                                            [ mkArgItem $ mkNeutral m'_id (mkItems [])
                                            , mkArgItem $ mkNeutral n_id (mkItems [])
                                            ]
                                    ]
                            ]
                    )
          ]
      )
