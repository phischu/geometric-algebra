module GeometricAlgebra.Euclidean3D where


-- Types
-- Parameter 'r' is field.

data Scalar r = Scalar r
  deriving (Show, Read, Eq, Ord)

data Vector r = Vector r r r
  deriving (Show, Read, Eq, Ord)

data Bivector r = Bivector r r r
  deriving (Show, Read, Eq, Ord)

-- | Aka Pseudoscalar.
data Trivector r = Trivector r
  deriving (Show, Read, Eq, Ord)

data Multivector r = Multivector (Scalar r) (Vector r) (Bivector r) (Trivector r)
  deriving (Show, Read, Eq, Ord)

-- Getters and Constructors

multivectorScalar :: Multivector r -> Scalar r
multivectorScalar = undefined

scalarMultivector :: Scalar r -> Multivector r
scalarMultivector = undefined

-- Addition

add :: Multivector r -> Multivector r -> Multivector r
add = undefined

addVectorVector :: Vector r -> Vector r -> Vector r
addVectorVector = undefined

-- Geometric Product

geometric :: Multivector r -> Multivector r -> Multivector r
geometric = undefined

geometricVectorVector :: Vector r -> Vector r -> Rotor r
geometricVectorVector = undefined

-- Inner Product

inner :: Multivector r -> Multivector r -> Multivector r
inner = undefined

innerScalarScalar :: Scalar r -> Scalar r -> Scalar r
innerScalarScalar = undefined

innerVectorVector :: Vector r -> Vector r -> Scalar r
innerVectorVector = undefined

-- Outer Product

outer :: Multivector r -> Multivector r -> Multivector r
outer = undefined

outerScalarScalar :: Scalar r -> Scalar r -> Scalar r
outerScalarScalar = undefined

outerScalarVector :: Scalar r -> Vector r -> Vector r
outerScalarVector = undefined

outerVectorVector :: Vector r -> Vector r -> Bivector r
outerVectorVector = undefined

-- Inverse

inverse :: Multivector r -> Multivector r
inverse = undefined

inverseVector :: Vector r -> Vector r
inverseVector = undefined

-- Reverse

reverse :: Multivector r -> Multivector r
reverse = undefined

reverseBivector :: Bivector r -> Bivector r
reverseBivector = undefined

reverseTrivector :: Trivector r -> Trivector r
reverseTrivector = undefined


-- Dual

-- | dualVector a = geometricVectorTrivector a unitPseudoscalar
dualVector :: Vector r -> Bivector r
dualVector = undefined

-- | dualBivector b = gemoetricBivectorTrivector b unitPseudoscalar
dualBivector :: Bivector r -> Vector r
dualBivector = undefined

-- Rotors

data Rotor r = Rotor (Scalar r) (Bivector r)
  deriving (Show, Read, Eq, Ord)

rotorScalar :: Rotor r -> Scalar r
rotorScalar = undefined

geometricRotorRotor :: Rotor r -> Rotor r -> Rotor r
geometricRotorRotor = undefined

rotate :: Rotor r -> Multivector r -> Multivector r
rotate = undefined

-- Versors

data Versor r = Versor (Multivector r)
  deriving (Show, Read, Eq, Ord)

data Parity = Rotation | Reflection
  deriving (Show, Read, Eq, Ord)

geometricVersorVersor :: Versor r -> Versor r -> Versor r
geometricVersorVersor = undefined

versorParity :: Versor r -> Parity
versorParity = undefined

transform :: Versor r -> Multivector r -> Multivector r
transform = undefined

reflection :: Vector r -> Versor r
reflection = undefined

-- Exponential

exponential :: Multivector r -> Multivector r
exponential = undefined

-- Logarithm

-- logarithm :: Multivector r -> Multivector r
-- logarithm = undefined

-- Matrix

-- | A matrix is a triple of vectors.
data Matrix r = Matrix (Vector r) (Vector r) (Vector r)
  deriving (Show, Read, Eq, Ord)

-- | Represents a linear function as a matrix.
-- Linearity of the given function has to be ensured by the user.
linearMatrix :: (Vector r -> Vector r) -> Matrix r
linearMatrix = undefined

-- | Apply a matrix to a vector.
matrixLinear :: Matrix r -> Vector r -> Vector r
matrixLinear = undefined

rotorMatrix :: Rotor r -> Matrix r
rotorMatrix = undefined

-- Misc

unitPseudoscalar :: Trivector r
unitPseudoscalar = undefined

-- | The dual of the outer product.
cross :: Vector r -> Vector r -> Vector r
cross a b = dualBivector (outerVectorVector a b)

-- Meet

-- meet is outer

-- Join

-- join is dual of meet and therefore dual of outer

-- Examples

-- Light reflections at multiple surfaces
-- Point symmetry group characterization



