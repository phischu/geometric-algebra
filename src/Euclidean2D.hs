{-# LANGUAGE DeriveFunctor #-}
module Euclidean2D where

import Linear.Vector (Additive((^+^), zero))
import Control.Applicative (Applicative(pure,(<*>)), liftA2)

data Vector2 r = Vector2 r r
  deriving (Show, Read, Eq, Ord, Functor)

instance Applicative Vector2 where
  pure a = Vector2 a a
  Vector2 f1 f2 <*> Vector2 x1 x2 = Vector2 (f1 x1) (f2 x2)

instance Additive Vector2 where
  zero = pure 0
  (^+^) = liftA2 (+)

data Bivector2 r = Bivector2 r
  deriving (Show, Read, Eq, Ord, Functor)

instance Applicative Bivector2 where
  pure x = Bivector2 x
  Bivector2 f <*> Bivector2 x = Bivector2 (f x)

instance Additive Bivector2 where
  zero = pure 0
  (^+^) = liftA2 (+)

data Multivector2 r = Multivector2 {
  _scalar2 :: r,
  _vector2 :: Vector2 r,
  _bivector2 :: Bivector2 r }

bivector2 :: (Num r) => Vector2 r -> Vector2 r -> Bivector2 r
bivector2 (Vector2 a1 a2) (Vector2 b1 b2) = Bivector2 (a1 * b2 - a2 * b1)

