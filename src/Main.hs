module Main where


import Diagrams.Backend.Rasterific (renderRasterific, Rasterific)
import Diagrams.Prelude (
  P2, p2, r2,
  fc, red, lw, none, center,
  mkWidth, Diagram, mconcat,
  strokeLoop, glueLine, lineFromSegments, straight,
  arrowV, arrowAt', arrowHead, spike,
  hcat, vcat, hcat', pad, catMethod, CatMethod(Distrib), sep,
  def, set)
import Diagrams.TwoD.Layout.Grid (
  gridCat')
import Linear (
  V2(V2), (^+^), zero, negated, (*^))

vector2Diagram :: V2 Double -> Diagram Rasterific
vector2Diagram (V2 a1 a2) = arrowV (V2 a1 a2)

bivector2Diagram :: V2 Double -> V2 Double -> Diagram Rasterific
bivector2Diagram a b = mconcat [arrow1, arrow2, arrow3, arrow4, area] where
  area = lw none (fc red (
      strokeLoop (glueLine (lineFromSegments segments))))
  segments = map straight [a, b, negated a, negated b]
  invisibleArrow p v = lw none (arrowAt' arrowOpts (vectorP2 p) v)
  arrow1 = invisibleArrow zero (0.5 *^ a)
  arrow2 = invisibleArrow a (0.5 *^ b)
  arrow3 = invisibleArrow (a ^+^ b) (0.5 *^ negated a)
  arrow4 = invisibleArrow b (0.5 *^ negated b)
  arrowOpts = set arrowHead spike def



drawOuterVectorVector2 :: V2 Double -> V2 Double -> Diagram Rasterific
drawOuterVectorVector2 a b = center (mconcat [
  vector2Diagram a, vector2Diagram b, bivector2Diagram a b])

vectorP2 :: V2 r -> P2 r
vectorP2 (V2 a1 a2) = p2 (a1,a2)


figure1 :: Diagram Rasterific
figure1 = gridCat' 3 (concat (zipWith rowDiagram vectors1 vectors2)) where
  rowDiagram vector1 vector2 = map center [
    vector2Diagram vector1,
    vector2Diagram vector2,
    bivector2Diagram vector1 vector2]
  vectors1 = [V2 9 1, V2 5 5, V2 9 1]
  vectors2 = [V2 2 8, V2 (-5) 5, V2 (-9) 0]


figure2 :: Diagram Rasterific
figure2 = pad 1.1 (gridCat' 4 (map center (zipWith bivector2Diagram vectors1 vectors2))) where
  vectors1 = [V2 5 0, V2 2.5 0, V2 10  0, V2 5 0]
  vectors2 = [V2 0 5, V2 0  10, V2 0 2.5, V2 3 5]

figure3 :: Diagram Rasterific
figure3 = gridCat' 3 (concat (zipWith rowDiagram vectors1 vectors2)) where
  rowDiagram vector1 vector2 = map (center . pad 1.1) [
    vector2Diagram vector1,
    vector2Diagram vector2,
    bivector2Diagram vector1 vector2]
  vectors1 = [V2 5 (-1), V2 3 5]
  vectors2 = [V2 3 5, V2 5 (-1)]


main :: IO ()
main = do
  renderRasterific "out/figure1.png" (mkWidth 500) figure1
  renderRasterific "out/figure2.png" (mkWidth 500) figure2
  renderRasterific "out/figure3.png" (mkWidth 500) figure3

