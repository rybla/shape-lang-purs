module Test.Main where

import Data.Tuple.Nested
import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Prelude

import Data.Array (foldM)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map.Unsafe as Map
import Data.Maybe (Maybe(..))
import Data.Set (fromFoldable)
import Data.Tuple (Tuple(..))
import Data.UUID as UUID
import Debug as Debug
import Effect (Effect)
import Language.Shape.Stlc.Index (nilIxDown)
import Language.Shape.Stlc.Recursor.Action (applyChange)
import Language.Shape.Stlc.Types (History)
import Unsafe (fromJust)

main :: Effect Unit
main = runHistory (Map.lookup' "enlambda 1" histories)

runHistory :: History -> Effect Unit
runHistory ((term /\ type_) /\ changes) = do
  let res =
        foldM
          ( \st change ->
              case applyChange change st of 
                Just st' -> Right st'
                Nothing -> Left (st /\ change)
          )
          { term, type_, ix: nilIxDown, history: (term /\ type_) /\ [] }
          changes
  case res of 
    Right _st -> do
      Debug.traceM $ "===[ runHistory success ]==="
    Left (st /\ change) -> do 
      Debug.traceM $ "===[ runHistory failure ]==="
      Debug.traceM $ "===[ runHistory failure: last valid state ]==="
      Debug.traceM $ show st
      Debug.traceM $ "===[ runHistory failure: last change attempted on last valid state ]==="
      Debug.traceM $ show change
  pure unit

histories :: Map String History 
histories = Map.fromFoldable [
  "enlambda 1" /\ (Tuple (Tuple (Data { body: (Let { body: (Hole { meta: HoleMetadata {} }), impl: (Hole { meta: HoleMetadata {} }), meta: LetMetadata { indentArg: false, indentBody: true, name: Name Nothing }, sign: (HoleType { holeId: (HoleId (fromJust (UUID.parseUUID "5c841c63-13c6-453b-8c67-3c5c8d5a73d1"))), meta: HoleTypeMetadata {}, weakening: (fromFoldable []) }), termBind: { meta: TermBindMetadata { name: Name (Just "x") }, termId: (TermId (fromJust (UUID.parseUUID "a0029cb6-2d72-4f3d-a48a-ade925485970"))) } }), meta: DataMetadata { indentSum: false, name: Name Nothing }, sumItems: ({ meta: SumItemMetadata { indented: false }, paramItems: Nil, termBind: { meta: TermBindMetadata { name: Name (Just "zero") }, termId: (TermId (fromJust (UUID.parseUUID "f274499d-272f-4310-9b16-2396fb46b769"))) } } : { meta: SumItemMetadata { indented: false }, paramItems: ({ meta: ParamItemMetadata { indented: false }, type_: (DataType { meta: DataTypeMetadata {}, typeId: (TypeId (fromJust (UUID.parseUUID "6d01d795-527a-41c6-be50-f074fac3f4d5"))) }) } : Nil), termBind: { meta: TermBindMetadata { name: Name (Just "suc") }, termId: (TermId (fromJust (UUID.parseUUID "d560b67e-6aa2-458c-95ce-56a161ba97fd"))) } } : Nil), typeBind: { meta: TypeBindMetadata { name: Name (Just "Nat") }, typeId: (TypeId (fromJust (UUID.parseUUID "6d01d795-527a-41c6-be50-f074fac3f4d5"))) } }) (ArrowType { cod: (HoleType { holeId: (HoleId (fromJust (UUID.parseUUID "ca49bae6-bf46-47f6-9c03-6b795ca8a563"))), meta: HoleTypeMetadata {}, weakening: (fromFoldable []) }), dom: (HoleType { holeId: (HoleId (fromJust (UUID.parseUUID "1b2586aa-3bcf-4577-a75a-10319666d530"))), meta: HoleTypeMetadata {}, weakening: (fromFoldable []) }), meta: ArrowTypeMetadata {} })) [])
]