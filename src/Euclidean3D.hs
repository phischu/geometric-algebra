module Euclidean3D where


data Vector3 r = Vector3 r r r
  deriving (Show, Read, Eq, Ord)
data Bivector3 r = Bivector3 r r r
  deriving (Show, Read, Eq, Ord)
data Trivector3 r = Trivector3 r
  deriving (Show, Read, Eq, Ord)

data Multivector3 r = Multivector3 r (Vector3 r) (Bivector3 r) (Trivector3 r)
  deriving (Show, Read, Eq, Ord)

bivector3 :: (Num r) => Vector3 r -> Vector3 r -> Bivector3 r
bivector3 (Vector3 a1 a2 a3) (Vector3 b1 b2 b3) =
  Bivector3 (a1 * b2 - a2 * b1) (a2 * b3 - a3 - b2) (a3 * b1 - a1 * b3)

trivector3 :: (Num r) => Vector3 r -> Bivector3 r -> Trivector3 r
trivector3 (Vector3 a1 a2 a3) (Bivector3 b1 b2 b3) =
  Trivector3 (a3 * b1 + a1 * b2 - a2 * b3)

e1 :: (Num r) => Vector3 r
e1 = Vector3 1 0 0

e2 :: (Num r) => Vector3 r
e2 = Vector3 0 1 0

e3 :: (Num r) => Vector3 r
e3 = Vector3 0 0 1


