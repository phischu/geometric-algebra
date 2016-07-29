module GeometricAlgebra.LinearFunction where

import Linear (
  Metric(dot),
  identity)

import Data.Traversable (
  Traversable)
import Control.Applicative (
  Applicative)


newtype Dual v r = Dual { unDual :: v r -> r }

toDual :: (Metric v, Num r) => v r -> Dual v r
toDual x = Dual (dot x)

fromDual :: (Applicative v, Traversable v, Num r) => Dual v r -> v r
fromDual x = fmap (unDual x) identity

-- data Linear v w r = Linear (v r -> w r)

-- newtype Orthonormal v w r = ..
-- Ortho: inner (o x) (o y) = inner x y
-- Normal: o e = e

-- determinant :: Linear v w r -> r
-- determinant f = apply f pseudoscalar

-- outermorphism :: Linear v w r -> Linear (Multivector v) (Multivector w) r



