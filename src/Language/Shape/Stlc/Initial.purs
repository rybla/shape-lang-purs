module Language.Shape.Stlc.Initial where

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
    h1_id = freshHoleId unit

    h2_id = freshHoleId unit

    nat_id /\ nat_name = makeTypeVar "Nat"

    zero_id /\ zero_name = makeTermVar "0"

    suc_id /\ suc_name = makeTermVar "+N 1"

    identity_id /\ identity_name = makeTermVar "identity"

    n_name = TermName (Just "n")

    a_id /\ a_name = makeTermVar "a"

    const_id /\ const_name = makeTermVar "const"

    x1_id /\ x1_name = makeTermVar "x"

    x2_id /\ x2_name = makeTermVar "x"

    tmp1_id /\ tmp1_name = makeTermVar "tmp1"

    x3_id /\ x3_name = makeTermVar "x"

    int_id /\ int_name = makeTypeVar "Int"

    positive_id /\ positive_name = makeTermVar "+"

    negative_id /\ negative_name = makeTermVar "-I -1"

    addNat_id /\ addNat_name = makeTermVar "+N"

    addInt_id /\ addInt_name = makeTermVar "+I"
  in
    Module
      ( fromFoldable
          [ DataDefinition (TypeBinding nat_id defaultTypeBindingMetadata { name = nat_name })
              ( fromFoldable
                  [ Constructor
                      (TermBinding zero_id defaultTermBindingMetadata { name = zero_name })
                      Nil
                      defaultConstructorMetadata
                      /\ defaultConstructorItemMetadata
                  , Constructor
                      (TermBinding suc_id defaultTermBindingMetadata { name = suc_name })
                      (fromFoldable [ Parameter (DataType nat_id defaultDataTypeMetadata) defaultParameterMetadata { name = n_name } /\ defaultParameterItemMetadata ])
                      defaultConstructorMetadata
                      /\ defaultConstructorItemMetadata
                  ]
              )
              defaultDataDefinitionMetadata
              /\ defaultDefinitionItemMetadata
          , let
              m_id /\ m_name = makeTermVar "m"

              m'_id /\ _ = makeTermVar "m'"

              n_id /\ n_name = makeTermVar "n"
            in
              mkDefItem -- addNat
                $ mkTermDef
                    (mkTermBind addNat_id addNat_name)
                    (mkArrow (mkParam m_name $ mkData nat_id) $ mkArrow (mkParam n_name $ mkData nat_id) $ mkData nat_id)
                    ( mkLambda m_id $ mkBlock Nil $ mkLambda n_id $ mkBlockInd Nil
                        $ mkMatch nat_id (mkNeutral m_id Nil)
                        $ fromFoldable
                            [ mkCaseItem $ mkCase Nil $ mkBlock Nil $ mkNeutral n_id Nil
                            , mkCaseItem $ mkCase (singleton $ mkTermIdItem m'_id) $ mkBlock Nil
                                $ mkNeutral suc_id
                                $ singleton
                                $ mkArgItem
                                $ mkNeutral addNat_id
                                $ fromFoldable [ mkArgItem $ mkNeutral m'_id Nil, mkArgItem $ mkNeutral n_id Nil ]
                            ]
                    )
          , TermDefinition
              (TermBinding identity_id defaultTermBindingMetadata { name = identity_name })
              ( ArrowType
                  (Parameter (DataType nat_id defaultDataTypeMetadata) defaultParameterMetadata { name = a_name })
                  (DataType nat_id defaultDataTypeMetadata)
                  defaultArrowTypeMetadata
              )
              ( LambdaTerm
                  a_id
                  ( Block Nil
                      (NeutralTerm a_id Nil defaultNeutralTermMetadata)
                      defaultBlockMetadata
                  )
                  defaultLambdaTermMetadata { indented = false }
              )
              defaultTermDefinitionMetadata
                { indented = false }
              /\ defaultDefinitionItemMetadata
          , TermDefinition
              (TermBinding const_id defaultTermBindingMetadata { name = const_name })
              ( ArrowType
                  (Parameter (DataType nat_id defaultDataTypeMetadata) defaultParameterMetadata { name = x1_name })
                  ( ArrowType
                      (Parameter (DataType nat_id defaultDataTypeMetadata) defaultParameterMetadata { name = x2_name })
                      (DataType nat_id defaultDataTypeMetadata)
                      defaultArrowTypeMetadata
                  )
                  defaultArrowTypeMetadata
              )
              ( LambdaTerm
                  x1_id
                  ( Block Nil
                      -- ( LambdaTerm x2_id
                      --     ( Block Nil
                      --         ( NeutralTerm x1_id Nil
                      --             defaultNeutralTermMetadata
                      --         )
                      --         defaultBlockMetadata
                      --     )
                      --     defaultLambdaTermMetadata { indented = true }
                      -- )
                      (HoleTerm defaultHoleTermMetadata)
                      defaultBlockMetadata
                  )
                  defaultLambdaTermMetadata
              )
              defaultTermDefinitionMetadata
              /\ defaultDefinitionItemMetadata
          , let
              identity_id /\ identity_name = makeTermVar "identity"

              x_id /\ x_name = makeTermVar "x"

              x'_id /\ x'_name = makeTermVar "x'"
            in
              TermDefinition
                (TermBinding identity_id defaultTermBindingMetadata { name = identity_name })
                ( ArrowType
                    (Parameter (DataType nat_id defaultDataTypeMetadata) defaultParameterMetadata { name = x_name })
                    (DataType nat_id defaultDataTypeMetadata)
                    defaultArrowTypeMetadata
                )
                ( LambdaTerm
                    x_id
                    ( Block Nil
                        ( MatchTerm nat_id (NeutralTerm x_id Nil defaultNeutralTermMetadata)
                            ( fromFoldable
                                [ Case -- zero
                                    Nil
                                    (Block Nil (NeutralTerm zero_id Nil defaultNeutralTermMetadata) defaultBlockMetadata)
                                    defaultCaseMetadata
                                    /\ defaultCaseItemMetadata { indented = true }
                                , Case -- suc
                                    ((x'_id /\ defaultTermIdItemMetadata) : Nil)
                                    (Block Nil (NeutralTerm suc_id ((NeutralTerm x'_id Nil defaultNeutralTermMetadata /\ defaultArgItemMetadata) : Nil) defaultNeutralTermMetadata) defaultBlockMetadata)
                                    defaultCaseMetadata
                                    /\ defaultCaseItemMetadata { indented = true }
                                ]
                            )
                            defaultMatchTermMetadata
                        )
                        defaultBlockMetadata { indented = true }
                    )
                    defaultLambdaTermMetadata
                )
                defaultTermDefinitionMetadata
                /\ defaultDefinitionItemMetadata
          , mkDefItem -- data Int
              $ mkDataDef
                  (mkTypeBind int_id int_name)
                  ( fromFoldable
                      [ mkConstrItem
                          $ mkConstr
                              (mkTermBind positive_id positive_name)
                              (fromFoldable [ mkParamItem $ mkParam n_name $ mkData nat_id ])
                      , mkConstrItem
                          $ mkConstr
                              (mkTermBind negative_id negative_name)
                              (fromFoldable [ mkParamItem $ mkParam n_name $ mkData nat_id ])
                      ]
                  )
          , let
              i_id /\ i_name = makeTermVar "i"

              i'1_id /\ i'1_name = makeTermVar "i'"

              i'2_id /\ i'2_name = makeTermVar "i'"

              j_id /\ j_name = makeTermVar "j"

              j'1_id /\ j'1_name = makeTermVar "j'"

              j'2_id /\ j'2_name = makeTermVar "j'"
            in
              mkDefItem -- def addInt
                $ mkTermDef
                    (mkTermBind addInt_id addInt_name)
                    (mkArrow (mkParam i_name $ mkData int_id) $ mkArrow (mkParam j_name $ mkData int_id) $ mkData int_id)
                    ( mkLambda i_id $ mkBlock Nil $ mkLambda j_id $ mkBlockInd Nil
                        $ mkMatch int_id (mkNeutral i_id Nil)
                        $ fromFoldable
                            [ mkCaseItem $ mkCase (singleton $ mkTermIdItem i'1_id) $ mkBlockInd Nil
                                $ mkMatch int_id (mkNeutral j_id Nil)
                                $ fromFoldable
                                    -- +i'1 + +j'1 = +(i'1 + j'1)
                                    [ mkCaseItem $ mkCase (singleton $ mkTermIdItem j'1_id) $ mkBlock Nil
                                        $ mkNeutral positive_id
                                        $ singleton
                                        $ mkArgItem
                                        $ mkNeutral addNat_id
                                        $ fromFoldable
                                            [ mkArgItem $ mkNeutral i'1_id Nil
                                            , mkArgItem $ mkNeutral j'1_id Nil
                                            ]
                                    -- +i'1 + (-1 -j'2) = ...
                                    , mkCaseItem $ mkCase (singleton $ mkTermIdItem j'2_id) $ mkBlock Nil mkHoleTerm
                                    ]
                            , mkCaseItem $ mkCase (singleton $ mkTermIdItem i'2_id) $ mkBlockInd Nil
                                $ mkMatch int_id (mkNeutral j_id Nil)
                                $ fromFoldable
                                    -- (-1 - i'2) + +j'1 = ...
                                    [ mkCaseItem $ mkCase (singleton $ mkTermIdItem j'1_id) $ mkBlock Nil mkHoleTerm
                                    -- (-1 - i'2) + (-1 -j'2) = (-1 - (-1 - (i'2 + j'2)))
                                    , mkCaseItem $ mkCase (singleton $ mkTermIdItem j'2_id) $ mkBlock Nil
                                        $ mkNeutral negative_id
                                        $ singleton
                                        $ mkArgItem
                                        $ mkNeutral negative_id
                                        $ singleton
                                        $ mkArgItem
                                        $ mkNeutral addNat_id
                                        $ fromFoldable
                                            [ mkArgItem $ mkNeutral i'2_id Nil
                                            , mkArgItem $ mkNeutral j'2_id Nil
                                            ]
                                    ]
                            ]
                    )
          ]
      )
      defaultModuleMetadata
