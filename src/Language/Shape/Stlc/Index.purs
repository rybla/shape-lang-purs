module Language.Shape.Stlc.Index where

import Data.Foldable
import Prelude
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), range, reverse, singleton, snoc, (:))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over, over2, wrap)
import Data.Show.Generic (genericShow)
import Undefined (undefined)

-- | IxUp
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

-- | IxDown
newtype IxDown
  = IxDown (List IxStep)

derive instance newTypeIxDown :: Newtype IxDown _

derive newtype instance showIxDown :: Show IxDown

derive newtype instance eqIxDown :: Eq IxDown

instance semigroupIxDown :: Semigroup IxDown where
  append = over2 wrap append

instance monoidIxDown :: Monoid IxDown where
  mempty = wrap Nil

-- | utilities
nilIxUp = IxUp Nil

nilIxDown = IxDown Nil

-- | conversions IxDown <-> IxUp
toIxUp :: IxDown -> IxUp
toIxUp (IxDown steps) = IxUp (reverse steps)

toIxDown :: IxUp -> IxDown
toIxDown (IxUp steps) = IxDown (reverse steps)

-- | IxStep
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
  | IxStepTermBind
  | IxStepArgItem
  | IxStepSumItem
  | IxStepCaseItem
  | IxStepParamItem
  | IxStepTermBindItem
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
  IxStepBuf -> 3
  IxStepData -> 3
  IxStepMatch -> 2
  IxStepArgItem -> 1
  IxStepSumItem -> 2
  IxStepCaseItem -> 2
  IxStepParamItem -> 1
  IxStepTermBindItem -> 1
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
  , sign: IxStep IxStepLet 1
  , impl: IxStep IxStepLet 2
  , body: IxStep IxStepLet 3
  }

ixStepBuf =
  { sign: IxStep IxStepBuf 1
  , impl: IxStep IxStepBuf 0
  , body: IxStep IxStepBuf 2
  }

ixStepData =
  { typeBind: IxStep IxStepData 0
  , sumItems: IxStep IxStepData 1
  , body: IxStep IxStepData 2
  }

ixStepMatch =
  { term: IxStep IxStepMatch 0
  , caseItems: IxStep IxStepMatch 1
  }

ixStepArgItem =
  { term: IxStep IxStepSumItem 0
  }

ixStepSumItem =
  { termBind: IxStep IxStepSumItem 0
  , paramItems: IxStep IxStepSumItem 1
  }

ixStepCaseItem =
  { termBindItems: IxStep IxStepCaseItem 0
  , body: IxStep IxStepCaseItem 1
  }

ixStepParamItem =
  { type_: IxStep IxStepParamItem 0
  }

ixStepTermBindItem =
  { termBind: IxStep IxStepTermBindItem 0
  }

ixStepTermBind =
  { termId: IxStep IxStepTermBind 0
  }

ixStepList =
  { head: IxStep IxStepList 0
  , tail: IxStep IxStepList 1
  }

-- TODO: do I actually use there anywhere?

-- ixUpListItem :: Int -> IxUp
-- ixUpListItem i
--   | i == 0 = wrap (singleton ixStepList.head)
--   | otherwise = over wrap (flip snoc ixStepList.tail) (ixUpListItem (i - 1))

-- ixDownListItem :: Int -> IxDown
-- ixDownListItem i
--   | i == 0 = wrap (singleton ixStepList.head)
--   | otherwise = over wrap (Cons ixStepList.tail) (ixDownListItem (i - 1))

-- ixUpListItems :: Int -> List IxUp
-- ixUpListItems l = go nilIxUp l
--   where
--   go :: IxUp -> Int -> List IxUp
--   go ixUp i
--     | i == 0 = Nil
--     | otherwise =
--       Cons
--         (over IxUp (Cons ixStepList.head) ixUp)
--         (go (over IxUp (Cons ixStepList.tail) ixUp) (i - 1))
