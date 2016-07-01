module Conformal3D where


data Basis = E1 | E2 | E3 | EI | E0

class E1 v where
  _e1 :: v r -> r

class EI v where
  _ei :: v r -> r

class EO v where
  _eo :: v r -> r


-- Entities

data Point a = Point a a a a a

data HomogeneousPoint a = HomogeneousPoint a a a a

-- Point pair

-- Line

-- line :: Point -> Point -> Line


-- Circle

-- Plane

-- Sphere

-- OPERATORS

-- Reflection

-- Inversion

-- Rotor

-- rotorFromVectors

-- rotorFromDualPlanes

-- Translator

-- translatorFromParallelDualPlanes

-- translatorFromPoints

-- Dilator

-- Motor

-- General Rotor

