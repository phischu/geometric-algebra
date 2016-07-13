{-# LANGUAGE DeriveFunctor #-}
module GeometricAlgebra.Homogeneous2D where

import Linear.Vector (
  Additive((^+^), zero))
import Linear.Metric (
  Metric(dot))
import Control.Applicative (
  Applicative(pure,(<*>)), liftA2)


-- | A point in 2-dimensional space. Possibly at infinity.
data Point2 r = Point2 !r !r !r
  deriving (Show, Read, Eq, Ord, Functor)

instance Applicative Point2 where
  pure x = Point2 x x x
  Point2 f1 f2 f3 <*> Point2 x1 x2 x3 = Point2 (f1 x1) (f2 x2) (f3 x3)

instance Additive Point2 where
  zero = pure 0
  (^+^) = liftA2 (+)

-- instance R1, R2, R3 Point2

instance Metric Point2 where
  dot (Point2 a1 a2 a3) (Point2 b1 b2 b3) = error "metric of points?"

-- instance Outer Point2 Point2
-- instance Weight, Attitude, ....

-- | A line in 2-dimensional space. Possibly at infinity.
data Line2 r = Line2 !r !r !r
  deriving (Show, Read, Eq, Ord, Functor)

instance Applicative Line2 where
  pure x = Line2 x x x
  Line2 f1 f2 f3 <*> Line2 x1 x2 x3 = Line2 (f1 x1) (f2 x2) (f3 x3)

instance Additive Line2 where
  zero = pure 0
  (^+^) = liftA2 (+)

-- Trivector?

{-
-- c * e0 + a * e1 + b * e2
data NormalizedLine r = NormalizedLine r r r

-- e01 + x e12 + y e20
data NormalizedPoint r = NormalizedPoint r r

-- Taken from Charles Gunn

isPointOnLine :: Point r -> Line r -> Bool
isPointOnLine (Point a) (Line l) = outerBivectorVector a l == Trivector 0

areCollinear :: Point r -> Point r -> Point r -> Bool
areCollinear (Point a) (Point b) (Point c) = dualOuter a (dualOuter b c) == Scalar 0

areConcurrent :: Line r -> Line r -> Line r -> Bool
areConcurrent (Line l) (Line m) (Line n) = outer l (outer m n) == Trivector 0

areParallelLines :: Line r -> Line r -> Bool
areParallelLines (Line l) (Line m) = outerVectorVector l m == Bivector 0

arePerpendicularLines :: Line r -> Line r -> Bool
arePerpendicularLines (Line l) (Line m) = inner l m == 0

lineThroughPoints :: Point r -> Point r -> Line r
lineThroughPoints (Point a) (Point b) = Line (dualOuter a b)

-- | A Line through the given point perpendicular to the given line.
altitude :: Point r -> Line r -> Line r
altitude (Point a) (Line l) = Line (vectorPart (geometric a l))

-- | A line through the given point parallel to the given line
parallelLine :: Point r -> Line r -> Line r
parallelLine (Point a) (Line l) = Line (geometric a (altitude a l))

-- | The 'foot' of the altitude
foot :: Point r -> Line r -> Point r
foot (Point a) (Line l) = Point (outer l (altitude a l))


perpendicularBisector :: Point r -> Point r -> Line r
perpendicularBisector = undefined -- vectorPart (geometric (dualOuter a b) (a + b))


-- | Derivation: define medians for three sides and show that they intersect
centroid :: Triangle r -> Point r
centroid = undefined -- Point (outer a b + outer b c + outer c a)

geometric :: Multivector r -> Multivector r -> Multivector r
geometric a b = add (inner a b) (outer a b)


-- inner

inner :: Multivector r -> Multivector r -> Multivector r
inner = undefined

-- The inner product ignores the projective component
innerVectorVector :: Vector r -> Vector r -> Scalar r
innerVectorVector (Vector _ a1 a2) (Vector _ b1 b2) = Scalar (a1 * b1 + a2 * b2)

-- outer

outer :: Multivector r -> Multivector r -> Multivector r
outer = undefined

outerBivectorVector :: Bivector r -> Vector r -> Trivector r
outerBivectorVector = undefined

-- dual

dualOuter :: Multivector r -> Multivector r -> Multivector r
dualOuter = undefined

-- add

add :: Multivector r -> Multivector r -> Multivector r
add = undefined

-- Blades

data OneBlade r = OneBlade (Vector r)

data TwoBlade r = TwoBlade (Vector r) (Vector r)

data ThreeBlade r = ThreeBlade (Vector r) (Vector r) (Vector r)

-- Reverse

-- PseudoScalar

unitPseudoscalar :: Multivector r
unitPseudoscalar = undefined

-- Quadrance

-- | The quadrance of two points is the square of the line joining the two points.
quadrance :: NormalizedPoint r -> NormalizedPoint r -> Multivector r
quadrance a b = geometric l l where
  l = dualOuter a b

-- Spread

spread :: NormalizedLine r -> NormalizedLine r -> Multivector r
spread l m = negate (geometric a a) where
  a = outer l m

-- cross (not the cross product) and twist

-- cross omitted
-- twist omitted

-}
