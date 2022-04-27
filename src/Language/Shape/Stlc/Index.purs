module Language.Shape.Stlc.Index where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Show.Generic (genericShow)

newtype IxUp
  = IxUp (List IxStep)

derive newtype instance showIxUp :: Show IxUp

newtype IxDown
  = IxDown (List IxStep)

derive newtype instance showIxDown :: Show IxDown

data IxStep
  = IxStep IxStepLabel Int

derive instance genericIxStep :: Generic IxStep _

instance showIxStep :: Show IxStep where
  show x = genericShow x

data IxStepLabel
  = IxStepArrowType
  | IxStepLam
  | IxStepNeu
  | IxStepLet
  | IxStepBuf
  | IxStepData
  | IxStepMatch
  | IxStepList

derive instance genericIxStepLabel :: Generic IxStepLabel _

instance showIxStepLabel :: Show IxStepLabel where
  show x = genericShow x

ixStepLabelChildren :: IxStepLabel -> Int
ixStepLabelChildren ixStepLabel = case ixStepLabel of
  IxStepArrowType -> 2
  IxStepLam -> 2
  IxStepNeu -> 2
  IxStepLet -> 3
  IxStepBuf -> 2
  IxStepData -> 3
  IxStepMatch -> 2
  IxStepList -> 2
