module Main where

import Lib

import Graphics.Gloss

window :: Display
window = InWindow "Gass" (800, 600) (200, 200)

renderAtom :: Atom -> Picture
renderAtom atom = translate x y $ circle 10
  where
    (Point x y) = position atom

render :: Picture
render = pictures (map renderAtom (atoms $ initialWorld 100))

main :: IO ()
main = display window white render