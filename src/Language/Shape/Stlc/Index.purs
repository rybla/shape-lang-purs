module Language.Shape.Stlc.Index where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), singleton, (:))
import Data.Newtype (class Newtype, over, over2, wrap)
import Data.Show.Generic (genericShow)
import Undefined (undefined)

newtype IxUp
  = IxUp (List IxStep)

derive instance newTypeIxUp :: Newtype IxUp _

derive newtype instance showIxUp :: Show IxUp

derive newtype instance eqIxUp :: Eq IxUp

instance semigroupIxUp :: Semigroup IxUp where
  -- appends in reverse order
  append = over2 wrap (flip append)

instance monoidIxUp :: Monoid IxUp where
  mempty = wrap Nil

newtype IxDown
  = IxDown (List IxStep)

derive instance newTypeIxDown :: Newtype IxDown _

derive newtype instance showIxDown :: Show IxDown

derive newtype instance eqIxDown :: Eq IxDown

instance semigroupIxDown :: Semigroup IxDown where
  append = over2 wrap append

instance monoidIxDown :: Monoid IxDown where
  mempty = wrap Nil

data IxStep
  = IxStep IxStepLabel Int

derive instance genericIxStep :: Generic IxStep _

instance eqIxStep :: Eq IxStep where
  eq x = genericEq x

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
  | IxStepArgItem
  | IxStepSumItem
  | IxStepCaseItem
  | IxStepParam
  | IxStepTermBind
  | IxStepList

derive instance genericIxStepLabel :: Generic IxStepLabel _

instance eqIxStepLabel :: Eq IxStepLabel where
  eq x = genericEq x

instance showIxStepLabel :: Show IxStepLabel where
  show x = genericShow x

ixStepLabelChildren :: IxStepLabel -> Int
ixStepLabelChildren ixStepLabel = case ixStepLabel of
  IxStepArrowType -> 2
  IxStepLam -> 2
  IxStepNeu -> 2
  IxStepLet -> 4
  IxStepBuf -> 2
  IxStepData -> 3
  IxStepMatch -> 2
  IxStepArgItem -> 1
  IxStepSumItem -> 2
  IxStepCaseItem -> 2
  IxStepParam -> 1
  IxStepTermBind -> 1
  IxStepList -> 2

ixStepArrowType =
  { dom: IxStep IxStepArrowType 0
  , cod: IxStep IxStepArrowType 1
  }

ixStepLam =
  { termBind: IxStep IxStepLam 0
  , body: IxStep IxStepLam 1
  }

ixStepNeu =
  { termId: IxStep IxStepNeu 0
  , argItems: IxStep IxStepNeu 1
  }

ixStepLet =
  { termBind: IxStep IxStepLet 0
  , type_: IxStep IxStepLet 1
  , term: IxStep IxStepLet 2
  , body: IxStep IxStepLet 3
  }

ixStepBuf =
  { term: IxStep IxStepBuf 0
  , body: IxStep IxStepBuf 1
  }

ixStepData =
  { typeBind: IxStep IxStepData 0
  , sum: IxStep IxStepData 1
  , body: IxStep IxStepData 2
  }

ixStepMatch =
  { term: IxStep IxStepMatch 0
  , caseItems: IxStep IxStepMatch 1
  }

ixStepSumItem =
  { termBind: IxStep IxStepSumItem 0
  , params: IxStep IxStepSumItem 1
  }

ixStepCaseItem =
  { termBinds: IxStep IxStepCaseItem 0
  , body: IxStep IxStepCaseItem 1
  }

ixStepParam =
  { type_: IxStep IxStepParam 0
  }

ixStepTermBind =
  { termId: IxStep IxStepTermBind 0
  }

ixStepList =
  { head: IxStep IxStepList 0
  , tail: IxStep IxStepList 1
  }

ixUpListItem :: Int -> IxUp
ixUpListItem i = undefined

ixDownListItem :: Int -> IxDown
ixDownListItem 0 = wrap (singleton ixStepList.head)

ixDownListItem i = over wrap (ixStepList.tail : _) (ixDownListItem (i - 1))
