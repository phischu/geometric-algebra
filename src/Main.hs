module Main where

import Euclidean2D (Vector2(Vector2))

import Diagrams.Backend.SVG (renderSVG, SVG)
import Diagrams.Prelude (
  P2, p2, r2,
  fc, red, lw, none, center,
  dims2D, Diagram, mconcat, hcat,
  strokeLoop, glueLine, lineFromSegments, straight,
  arrowV, arrowAt', arrowHead, spike,
  def, set)
import Linear (V2(V2), (^+^), zero, negated, (*^))

vector2Diagram :: Vector2 Double -> Diagram SVG
vector2Diagram (Vector2 a1 a2) = arrowV (V2 a1 a2)

bivector2Diagram :: Vector2 Double -> Vector2 Double -> Diagram SVG
bivector2Diagram a b = mconcat [arrow1, arrow2, arrow3, arrow4, area] where
  area = lw none (fc red (
      strokeLoop (glueLine (lineFromSegments segments))))
  segments = map straight [
    vectorV2 a,
    vectorV2 b,
    negated (vectorV2 a),
    negated (vectorV2 b)]
  invisibleArrow p v = lw none (arrowAt' arrowOpts (vectorP2 p) (vectorV2 v))
  arrow1 = invisibleArrow zero (0.5 *^ a)
  arrow2 = invisibleArrow a (0.5 *^ b)
  arrow3 = invisibleArrow (a ^+^ b) (0.5 *^ negated a)
  arrow4 = invisibleArrow b (0.5 *^ negated b)
  arrowOpts = set arrowHead spike def



drawOuterVectorVector2 :: Vector2 Double -> Vector2 Double -> Diagram SVG
drawOuterVectorVector2 a b = center (mconcat [
  vector2Diagram a, vector2Diagram b, bivector2Diagram a b])

vectorP2 :: Vector2 r -> P2 r
vectorP2 (Vector2 a1 a2) = p2 (a1,a2)

vectorV2 :: Vector2 r -> V2 r
vectorV2 (Vector2 a1 a2) = r2 (a1, a2)

example :: Diagram SVG
example = hcat [
  bivector2Diagram (Vector2 5 1) (Vector2 10 9),
  bivector2Diagram (Vector2 10 9) (Vector2 5 1),
  bivector2Diagram (Vector2 9 1) (Vector2 1 9)]

main :: IO ()
main = renderSVG "out" (dims2D 500 500) example

