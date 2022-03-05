module Language.Shape.Stlc.MetadataFamily where

import Prelude
import Data.Symbol (SProxy(..))
import Type.Proxy (Proxy(..))

class Metadata :: Symbol -> Type -> Constraint
class Metadata k v | k -> v

-- Module with metadata
data Module
  = Module {- definitions, buffers -} (forall a. Metadata "Module" a => a)
