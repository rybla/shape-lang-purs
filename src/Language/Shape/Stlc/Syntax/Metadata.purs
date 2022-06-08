module Language.Shape.Stlc.Syntax.Metadata where

import Language.Shape.Stlc.Metadata
import Language.Shape.Stlc.Syntax
import Prelude
import Data.Newtype (over)
import Undefined (undefined)

-- TODO: now just have to implement this for each Syntax constructor... but there really isn't better way, believe me I've tried oh god I've tried. Maybe something with fundef typeclass magic, but Idk if that counts as better
indentTerm :: Term -> Term
indentTerm term = case term of
  Lam lam -> Lam lam { meta = over LamMetadata (\o -> o { indentBody = not o.indentBody }) lam.meta }
  Let let_ -> Let let_ { meta = over LetMetadata (\o -> o { indentSign = not o.indentSign, indentImpl = not o.indentImpl, indentBody = not o.indentBody }) let_.meta }
  Buf buf -> Buf buf { meta = over BufMetadata (\o -> o { indentSign = not o.indentSign, indentImpl = not o.indentImpl, indentBody = not o.indentBody }) buf.meta }
  Data data_ -> Data data_ { meta = over DataMetadata (\o -> o { indentSum = not o.indentSum, indentBody = not o.indentBody }) data_.meta }
  Match data_ -> Match data_ { meta = over MatchMetadata (\o -> o { indentCases = not o.indentCases }) data_.meta }
  _ -> term
