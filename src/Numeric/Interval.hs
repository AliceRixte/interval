{-# LANGUAGE DeriveFunctor #-}

module Numeric.Interval where

import Data.Typeable
import Control.Exception
import Data.Coerce

import Data.AEq

data IntervalBound a = OpenBound !a | ClosedBound !a
  deriving (Show, Eq)

instance Functor IntervalBound where
  fmap f (OpenBound a) = OpenBound (f a)
  fmap f (ClosedBound a) = ClosedBound (f a)


-- TODO : comment is not matching with the Just EQ

-- | Compares two interval bounds. When both bounds are equal, this only returns
-- EQ when both bounds are closed, and otherwise it returns Nothing. As a
-- consequence, this can be used to detect ill formed intervals.
--
compareBounds :: Ord a
  => IntervalBound a -> IntervalBound a -> Maybe Ordering
compareBounds (ClosedBound a) (ClosedBound b) =
  if a == b then
    Just EQ
  else
    Just (compare a b)
compareBounds (OpenBound a) (OpenBound b) =
  if a == b then
    Just EQ
  else
    Just (compare a b)
compareBounds (OpenBound a) (ClosedBound b) =
  if a == b then
    Just EQ
  else
    Just (compare a b)
compareBounds (ClosedBound a) (OpenBound b) =
  if a == b then
    Just EQ
  else
    Just (compare a b)


-- | Get the value of an interval bound.
boundValue :: IntervalBound a -> a
boundValue (OpenBound a) = a
boundValue (ClosedBound a) = a

-- | A newtype wrapper for the lower bound of an interval.
--
-- This is necessary in order to instanciate 'Ord'
--
newtype LowerBound a = LowerBound (IntervalBound a)
  deriving (Show, Eq, Functor)

-- | When equal, a closed lower bound is considered strictly less than an open
-- lower bound.
--
instance Ord a => Ord (LowerBound a) where
  compare (LowerBound (OpenBound a)) (LowerBound (OpenBound b)) =
    compare a b
  compare (LowerBound (ClosedBound a)) (LowerBound (ClosedBound b)) =
    compare a b
  compare (LowerBound (OpenBound a)) (LowerBound (ClosedBound b)) =
    if a == b then
      GT
    else
      compare a b
  compare (LowerBound (ClosedBound a)) (LowerBound (OpenBound b)) =
    if a == b then
      LT
    else
      compare a b
  {-# INLINE compare #-}

-- | The minimum of two lower bounds. When equal, a closed lower bound is
-- considered strictly less than an open lower bound.
--
minLowerBound :: Ord a => IntervalBound a -> IntervalBound a -> IntervalBound a
minLowerBound a b = coerce (min (LowerBound a) (LowerBound b))

-- | The maximum of two lower bounds. When equal, a closed lower bound is
-- considered strictly less than an open lower bound.
--
maxLowerBound :: Ord a => IntervalBound a -> IntervalBound a -> IntervalBound a
maxLowerBound a b = coerce (max (LowerBound a) (LowerBound b))



-- | A newtype wrapper for the upper bound of an interval.
--
-- This is necessary in order to instanciate 'Ord'
--
newtype UpperBound a = UpperBound (IntervalBound a)
  deriving (Show, Eq, Functor)

-- | When equal, a closed upper bound is considered strictly greater than a
-- open upper bound.
--
instance Ord a => Ord (UpperBound a) where
  compare (UpperBound (ClosedBound a)) (UpperBound (ClosedBound b)) =
    compare a b
  compare (UpperBound (OpenBound a)) (UpperBound (OpenBound b)) =
    compare a b
  compare (UpperBound (OpenBound a)) (UpperBound (ClosedBound b)) =
    if a == b then
      LT
    else
      compare a b
  compare (UpperBound (ClosedBound a)) (UpperBound (OpenBound b)) =
    if a == b then
      GT
    else
      compare a b
  {-# INLINE compare #-}

-- | The maximum of two upper bounds. When equal, a closed upper bound is
-- considered strictly greater than an open upper bound.
--
minUpperBound :: Ord a => IntervalBound a -> IntervalBound a -> IntervalBound a
minUpperBound a b = coerce (min (UpperBound a) (UpperBound b))

-- | The minimum of two upper bounds. When equal, a closed upper bound is
-- considered strictly greater than an open upper bound.
maxUpperBound :: Ord a => IntervalBound a -> IntervalBound a -> IntervalBound a
maxUpperBound a b = coerce (max (UpperBound a) (UpperBound b))

-- | An interval can be either empty, a singleton, or a pair composed of two
-- bounds, that maybe open or closed.
--
-- Singleton intervals are represented as closed intervals [a,a] with the same
-- lower and upper bounds.
--
data Interval a =
    EmptyInterval
  | Interval (IntervalBound a) (IntervalBound a)
  deriving (Eq)

instance Show a => Show (Interval a) where
  showsPrec _ EmptyInterval = showString "EmptyInterval"
  showsPrec n (Interval (OpenBound a) (OpenBound b)) =
    showParen (n > 5) $
      showsPrec 5 a . showString " <..< " . showsPrec 5 b
  showsPrec n (Interval (ClosedBound a) (OpenBound b)) =
    showParen (n > 5) $
      showsPrec 5 a . showString " <=..< " . showsPrec 5 b
  showsPrec n (Interval (OpenBound a) (ClosedBound b)) =
    showParen (n > 5) $
      showsPrec 5 a . showString " <..<= " . showsPrec 5 b
  showsPrec n (Interval (ClosedBound a) (ClosedBound b)) =
    showParen (n > 5) $
      showsPrec 5 a . showString " <=..<= " . showsPrec 5 b


data EmptyIntervalException = EmptyIntervalException
  deriving (Typeable)

instance Show EmptyIntervalException where
  show EmptyIntervalException = "Empty interval"

data SingletonIntervalException = SingletonIntervalException
  deriving (Typeable)

instance Show SingletonIntervalException where
  show SingletonIntervalException = "Singleton interval"



instance Exception EmptyIntervalException

-- | A singleton interval, i.e. a closed interval with the same lower and upper
-- bounds.
--
singletonInterval :: Ord a => a -> Interval a
singletonInterval a = Interval (ClosedBound a) (ClosedBound a)

-- | Test whether an interval is a singleton.
isSingleton :: Eq a => Interval a -> Bool
isSingleton (Interval (ClosedBound a) (ClosedBound b)) =
  a == b
isSingleton _ = False



-- | @a '<..<' b@ is the open interval @]a,b[@. When @a >= b@, it returns the
-- empty interval.
--
(<..<) :: Ord a => a -> a -> Interval a
a <..< b
  | a < b = Interval (OpenBound a) (OpenBound b)
  | otherwise = EmptyInterval
infix 5 <..<

-- | @a '<=..<' b@ is the interval @[a,b[@. When @a >= b@, it returns the
-- empty interval.
--
(<=..<) :: Ord a => a -> a -> Interval a
a <=..<  b
  | a < b = Interval (ClosedBound a) (OpenBound b)
  | otherwise = EmptyInterval
infix 5 <=..<

-- | @a '<..<=' b@ is the interval @]a,b]@. When @a >= b@, it returns the
-- empty interval.
(<..<=) :: Ord a => a -> a -> Interval a
a <..<= b
  | a < b = Interval (OpenBound a) (ClosedBound b)
  | otherwise = EmptyInterval
infix 5 <..<=

-- | @a '<=..<=' b@ is the closed interval @[a,b]@. When @a > b@, it returns
-- the empty interval.
--
(<=..<=) :: Ord a => a -> a -> Interval a
a <=..<= b
  | a < b = Interval (ClosedBound a) (ClosedBound b)
  | otherwise = EmptyInterval
infix 5 <=..<=

-- | Test whether an interval is empty.
--
null :: Interval a -> Bool
null EmptyInterval = True
null _ = False

-- | Get lower bound of an interval.
--
-- Raise an exception if the interval is empty.
--
lowerBound :: Interval a -> IntervalBound a
lowerBound EmptyInterval = throw EmptyIntervalException
lowerBound (Interval a _) = a

-- | Get upper bound of an interval.
--
-- Raise an exception when the interval is empty.
--
upperBound :: Interval a -> IntervalBound a
upperBound EmptyInterval = throw EmptyIntervalException
upperBound (Interval _ b) = b

-- | Get the value of the lower bound of an interval (which is its infimum).
--
-- Raise an exception when the interval is empty.
--
infimum :: Interval a -> a
infimum = boundValue . lowerBound

-- | Get the value of the upper bound of an interval (which is its supremum).
--
-- Raise an exception when the interval is empty.
--
supremum :: Interval a -> a
supremum = boundValue . upperBound

-- | Width of an interval
--
-- Raise an exception when the interval is empty.
--
width :: Num a => Interval a -> a
width EmptyInterval = throw EmptyIntervalException
width (Interval l u) = boundValue u - boundValue l

-- | Is the point inside the interval?
--
member :: Ord a => a -> Interval a -> Bool
member _ EmptyInterval = False
member a (Interval (OpenBound b) (OpenBound c)) = a > b && a < c
member a (Interval (ClosedBound b) (OpenBound c)) = a >= b && a < c
member a (Interval (OpenBound b) (ClosedBound c)) = a > b && a <= c
member a (Interval (ClosedBound b) (ClosedBound c)) = a >= b && a <= c

-- | Is the point one of the bounds of the interval?
--
isBound :: Ord a => a -> Interval a -> Bool
isBound _ EmptyInterval = False
isBound a (Interval l u) = a == boundValue l || a == boundValue u

findApproxBound :: (AEq a, Ord a) => a -> Interval a -> Maybe a
findApproxBound _ EmptyInterval = Nothing
findApproxBound a (Interval l u)
  | a ~== boundValue l = Just (boundValue l)
  | a ~== boundValue u = Just (boundValue u)
  | otherwise = Nothing




-- | Make an interval out of two bounds.
--
intervalFromUnorderedBounds :: Ord a
  => IntervalBound a -> IntervalBound a -> Interval a
intervalFromUnorderedBounds a b = case compareBounds a b of
  Nothing -> EmptyInterval
  Just GT -> Interval b a
  _ -> Interval a b

-- | Make an interval out of two bounds. If the first bound is greater than the
-- second bound, it returns the empty interval.
--
intervalFromBounds :: Ord a
  => IntervalBound a -> IntervalBound a -> Interval a
intervalFromBounds a b = case compareBounds a b of
  Nothing -> EmptyInterval
  Just GT -> EmptyInterval
  _ -> Interval a b

-- | Intersection of two intervals.
--
intersection :: Ord a
  => Interval a -> Interval a -> Interval a
intersection EmptyInterval _ = EmptyInterval
intersection _ EmptyInterval = EmptyInterval
intersection (Interval a1 b1) (Interval a2 b2) =
  intervalFromBounds (maxLowerBound a1 a2) (minLowerBound b1 b2)

-- | Hull of two intervals.
--
hull :: Ord a
  => Interval a -> Interval a -> Interval a
hull EmptyInterval b = b
hull a EmptyInterval = a
hull (Interval a1 b1) (Interval a2 b2) =
  intervalFromBounds (minLowerBound a1 a2) (maxLowerBound b1 b2)

-- | Apply a monotonic (not necessarily strictly) function to an interval.
--
mapMonotonic :: (Ord a, Ord b)
  => (a -> b) -> Interval a -> Interval b
mapMonotonic _ EmptyInterval = EmptyInterval
mapMonotonic f (Interval l u) =
  intervalFromUnorderedBounds (fmap f l) (fmap f u)