{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Data.Foldable (traverse_)

type R = Rational

data Scalar = Scalar R
  deriving (Show, Read, Eq, Ord)
data Vector = Vector R R R
  deriving (Show, Read, Eq, Ord)
data Bivector = Bivector R R R
  deriving (Show, Read, Eq, Ord)
data Trivector = Trivector R
  deriving (Show, Read, Eq, Ord)

data FactoredBivector = FactoredBivector Vector Vector
  deriving (Show, Read, Eq, Ord)

scale :: Scalar -> Vector -> Vector
scale (Scalar a) (Vector b1 b2 b3) =
  Vector (a * b1) (a * b2) (a * b3)

area :: Vector -> Vector -> Bivector
area (Vector a1 a2 a3) (Vector b1 b2 b3) =
  Bivector (a1 * b2 - a2 * b1) (a2 * b3 - a3 - b2) (a3 * b1 - a1 * b3)

volume :: Vector -> Bivector -> Trivector
volume (Vector a1 a2 a3) (Bivector b1 b2 b3) =
  Trivector (a3 * b1 + a1 * b2 - a2 * b3)

instance Num Scalar where
  fromInteger a = Scalar (fromInteger a)

instance Fractional Scalar where
  fromRational a = Scalar (fromRational a)

instance Num Vector where
  (Vector a1 a2 a3) + (Vector b1 b2 b3) = Vector (a1 + b1) (a2 + b2) (a3 + b3)
  negate (Vector a1 a2 a3) = Vector (negate a1) (negate a2) (negate a3)

instance Num Bivector where
  (Bivector a1 a2 a3) + (Bivector b1 b2 b3) = Bivector (a1 + b1) (a2 + b2) (a3 + b3)
  negate (Bivector a1 a2 a3) = Bivector (negate a1) (negate a2) (negate a3)



e1 :: Vector
e1 = Vector 1 0 0

e2 :: Vector
e2 = Vector 0 1 0

e3 :: Vector
e3 = Vector 0 0 1


main :: IO ()
main = drill2

drill1 :: IO ()
drill1 = traverse_ putStrLn [
  show a, show b, show c, show d, show e, show f] where
    a = (e1 + e2) `area` (e1 + e3)
    b = (e1 + e2 + e3) `area` (2 `scale` e1)
    c = (e1 - e2) `area` (e1 - e2)
    d = (e1 + e2) `area` (0.5 `scale` e1 + 2 `scale` e2 + 3 `scale` e3)
    e = (e1 + e3) `volume` (e1 `area` e2)
    f = (e1 + e2) `volume` ((e1 `area` e2) + (e2 `area` e3))

drill2 :: IO ()
drill2 = putStrLn (show a) where
  plane = e1 `area` (e2 - e3)
  a = e1 `volume` plane
