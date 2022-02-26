module Main where

import Language.Shape.Stlc.Syntax
import Prelude
import Data.List as List
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Language.Shape.Stlc.Renderer (renderModule)
import Partial.Unsafe (unsafePartial)
import Undefined (undefined)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

type State
  = { module_ :: Module }

data Action
  = Pass

initialModule :: Module
initialModule =
  Module
    $ List.fromFoldable
        [ DataDefinition nat_name nat_id
            $ List.fromFoldable
                [ Constructor zero_name zero_id List.Nil
                , Constructor suc_name suc_id $ List.singleton (Tuple n_name (BaseType (DataType nat_id)))
                ]
        , (TermDefinition identity_name identity_id)
            ( ArrowType
                (List.fromFoldable [ Tuple n_name (BaseType (DataType nat_id)) ])
                (DataType nat_id)
            )
            ( LambdaTerm
                (List.fromFoldable [ n_id ])
                -- (Block List.Nil List.Nil (ApplicationTerm n_id List.Nil))
                ( Block List.Nil List.Nil
                    ( MatchTerm (DataType nat_id) (ApplicationTerm n_id List.Nil)
                        ( List.fromFoldable
                            [ Case
                                (List.fromFoldable [])
                                (Block List.Nil List.Nil (ApplicationTerm zero_id List.Nil))
                            , Case
                                (List.fromFoldable [ n'_id ])
                                ( Block List.Nil List.Nil
                                    (ApplicationTerm suc_id (List.fromFoldable [ NeutralTerm (ApplicationTerm n'_id List.Nil) ]))
                                )
                            ]
                        )
                    )
                )
            )
        ]
  where
  Tuple nat_name nat_id = Tuple (TypeName "Nat") (TypeId 0)

  Tuple zero_name zero_id = Tuple (TermName "Zero") (TermId 1)

  Tuple suc_name suc_id = Tuple (TermName "Suc") (TermId 2)

  Tuple identity_name identity_id = Tuple (TermName "identity") (TermId 3)

  Tuple n_name n_id = Tuple (TermName "n") (TermId 4)

  Tuple n'_name n'_id = Tuple (TermName "n") (TermId 5)

component :: forall query input output m. H.Component query input output m
component =
  H.mkComponent
    { initialState:
        \_ -> { module_: initialModule }
    , render: render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. State -> HH.HTML (H.ComponentSlot cs m Action) Action
render st =
  HH.div
    [ HP.class_ (HH.ClassName "app") ]
    [ renderModule st.module_ ]

handleAction :: forall cs output m. Action -> H.HalogenM State Action cs output m Unit
handleAction = case _ of
  Pass -> H.modify_ identity
