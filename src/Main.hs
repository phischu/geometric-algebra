module Main where


import Diagrams.Backend.SVG (renderSVG, SVG)
import Diagrams.Prelude (
  dims2D, circle, Diagram, arrowV, (|||),
  arc, unitX, halfTurn, direction, (@@), deg)
import Diagrams.TwoD.Arrow (
  arrowFromLocatedTrail)
import Linear (V2(V2))

exampleArrow :: Diagram SVG
exampleArrow = arrowV (V2 5 8)

exampleArc :: Diagram SVG
exampleArc = arrowFromLocatedTrail (arc (direction unitX) (320 @@ deg))

example :: Diagram SVG
example = exampleArrow ||| exampleArc

main :: IO ()
main = renderSVG "out" (dims2D 500 500) example

