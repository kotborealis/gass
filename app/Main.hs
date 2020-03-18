module Main where

import Lib
import System.Random
import Control.Monad
import Graphics.Gloss

window :: Display
window = InWindow "Gass" (800, 600) (200, 200)

renderAtom :: Atom -> Picture
renderAtom atom = translate x y $ circle 10
  where
    (Point x y) = position atom

render :: World -> Picture
render (World atoms _ _) = pictures (map renderAtom atoms)

main :: IO ()
main = do
    atoms <- replicateM 100 $ getStdRandom $ randomR (Atom (Point (-400) (-300)) (Point (-10) (-10)), Atom (Point 400 300) (Point 10 10))
    let world = World atoms 800 600
    display window white (render world)