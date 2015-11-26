module Main where

import Euclidean2D (Vector2(Vector2))

import Diagrams.Backend.SVG (renderSVG, SVG)
import Diagrams.Prelude (
  P2, fc, red, closeLine, strokeLoop, lw, none, center,
  dims2D, Diagram, arrowV, (|||), atop, fromVertices, p2,
  arc, unitX, direction, (@@), deg)
import Diagrams.TwoD.Arrow (
  arrowFromLocatedTrail)
import Linear (V2(V2), (^+^), zero)

drawVector2 :: Vector2 Double -> Diagram SVG
drawVector2 (Vector2 a1 a2) = arrowV (V2 a1 a2)

drawBivector2 :: Vector2 Double -> Vector2 Double -> Diagram SVG
drawBivector2 a b =
  center (drawVector2 a `atop` drawVector2 b `atop` parallelogram) where
    parallelogram = lw none (fc red (strokeLoop (closeLine (fromVertices [
      vectorPoint zero,
      vectorPoint a,
      vectorPoint (a ^+^ b),
      vectorPoint b]))))

vectorPoint :: Vector2 r -> P2 r
vectorPoint (Vector2 a1 a2) = p2 (a1,a2)

exampleArrow :: Diagram SVG
exampleArrow = arrowV (V2 5 8)

exampleArc :: Diagram SVG
exampleArc = arrowFromLocatedTrail (arc (direction unitX) (320 @@ deg))

example :: Diagram SVG
example = drawBivector2 (Vector2 5 8) (Vector2 10 9)

main :: IO ()
main = renderSVG "out" (dims2D 500 500) example

