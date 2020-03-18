module Main where

import GasSimulation
import System.Random
import Control.Monad
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

window :: Display
window = InWindow "Gass" (800, 600) (200, 200)

renderAtom :: Atom -> Picture
renderAtom atom =
  let (V2 x y) = position atom
      body = translate x y $ circle 10
      (V2 vx vy) = speed atom
      vectorSpeed = color aquamarine $ translate x y $ line [(0, 0), (vx, vy)]
  in pictures [ body, vectorSpeed ]


render :: World -> Picture
render (World atoms) = pictures (map renderAtom atoms)

main :: IO ()
main = do
--     let loAtom = Atom (V2 (-400) (-300)) (V2 (-10) (-10))
--     let hiAtom = Atom (V2 400 300) (V2 10 10)
--     atoms <- replicateM 200 $ getStdRandom $ randomR (loAtom, hiAtom)
    let loAtom = Atom (V2 0 0) (V2 80 0)
    let hiAtom = Atom (V2 120 0) (V2 0 0)
    let atoms = [loAtom, hiAtom]
    let world = World atoms
    simulate
        window
        white
        1
        world
        render
        (\_ -> updateWorld)