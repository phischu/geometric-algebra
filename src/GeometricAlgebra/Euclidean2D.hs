{-# LANGUAGE DeriveFunctor #-}
module GeometricAlgebra.Euclidean2D where

import Linear.V2 (
  V2(V2))
import Linear.Vector (
  Additive((^+^), zero))
import Linear.Metric (
  Metric(dot))
import Control.Applicative (
  Applicative(pure,(<*>)), liftA2)


-- | A 2-dimensional area.
data Area2 r = Area2 !r
  deriving (Show, Read, Eq, Ord, Functor)

instance Applicative Area2 where
  pure x = Area2 x
  Area2 f <*> Area2 x = Area2 (f x)

instance Additive Area2 where
  zero = pure 0
  (^+^) = liftA2 (+)

-- instance Num Area2

-- instance R1 Area2

instance Metric Area2 where
  dot (Area2 a) (Area2 b) = a * b



-- instance Outer V2 V2 Area2

outerV2V2 :: (Num r) => V2 r -> V2 r -> Area2 r
outerV2V2 (V2 a1 a2) (V2 b1 b2) = Area2 (a1 * b2 - a2 * b1)

unitArea2 :: (Num r) => Area2 r
unitArea2 = Area2 1

