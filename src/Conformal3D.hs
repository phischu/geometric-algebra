module Conformal3D where


import Linear (V3)


data Basis = E1 | E2 | E3 | EI | E0

class E1 v where
  _e1 :: v r -> r

class EI v where
  _ei :: v r -> r

class EO v where
  _eo :: v r -> r

-- Sphere

-- PointSphere



-- Entities

-- | An element of the representational space with grade one.
data Vector a = Vector a a a a a

origin :: (Num a) => Vector a
origin = Vector 1 0 0 0 0

infinity :: (Num a) => Vector a
infinity = Vector 0 0 0 0 1

euclidean :: (Num a) => V3 a -> Vector a
euclidean = undefined

-- homogeneous :: Homogeneous.Vector r -> Vector r

-- data Bivector a = ...

-- | A point in 3D space. Could be at infinity. Has a weight.
data Point a = Point a a a a

data HomogeneousPoint a = HomogeneousPoint a a a a

-- Point pair

-- Sphere

-- PointSphere


-- FLATS

-- data FlatPoint r = FlatPoint r r r r

-- Line

-- line :: Point -> Point -> Line

-- Circle

-- Plane


-- VERSORS

-- Reflection

-- Inversion


-- ROTORS

-- Rotor

-- rotorFromVectors

-- rotorFromDualPlanes

-- Translator

-- translatorFromParallelDualPlanes

-- translatorFromPoints

-- Dilator

-- Motor

-- General Rotor

-- exponential :: Bivector r -> Rotor r

-- logarithm :: Rotor r -> Bivector r


-- Properties

-- inverse r == reverse r

-- isAdjoint r (inverse r)

-- isAdjoint f g = forall a b. inner a (g b) == inner (f a) b


-- PRODUCTS

-- geometric

-- inner

-- outer

-- contract

-- project

-- reject

-- commutator

-- meet

-- join

-- UNARY OPERATORS

-- reverseVector :: Vector a -> Vector a
-- reverseBivector :: Bivector a -> Bivector a
-- reverseRotor

-- inverse

-- dual

-- dualVector
-- dualSphere

