module Language.Shape.Stlc.ChangeAtIndex where

import Prelude

import Data.Tuple (Tuple(..))
import Language.Shape.Stlc.Changes (TypeChange)
import Language.Shape.Stlc.Syntax (Module(..), Syntax(..))
import Undefined (undefined)
import Unsafe (error)

data Index

chAtIndex :: Module -> Index -> Syntax -> TypeChange -> Tuple Module Index
chAtIndex m i s tc
    = case chAtIndexImpl (SyntaxModule m) i s tc of
           (Tuple (SyntaxModule m) i') -> Tuple m i'
           _ -> error "no"

chAtIndexImpl :: Syntax -> Index -> Syntax -> TypeChange -> Tuple Syntax Index
chAtIndexImpl = undefined
