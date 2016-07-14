module Main where

import GeometricAlgebra.Euclidean2D (
  outerV2V2)

import Linear (
  V2(V2), (^+^))

import Test.Hspec (
  hspec, describe, it)
import Test.QuickCheck (
  property,
  Arbitrary(arbitrary, shrink), genericShrink)

import Control.Applicative (
  liftA2)


main :: IO ()
main = hspec (do
  describe "outerV2V2" (do
    it "is linear" (property isLinearOuterV2V2)))

isLinearOuterV2V2 :: V2 Rational -> V2 Rational -> V2 Rational -> Bool
isLinearOuterV2V2 a x y =
  outerV2V2 a (x ^+^ y) == outerV2V2 a x ^+^ outerV2V2 a y

instance (Arbitrary r) => Arbitrary (V2 r) where
  arbitrary = liftA2 V2 arbitrary arbitrary
  shrink = genericShrink

