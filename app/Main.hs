module Main where

import           GasSimulation
import           System.Random
import           Control.Monad
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Control.Lens

window :: Display
window = InWindow "Gass" (800, 600) (200, 200)

renderAtom :: Atom -> Picture
renderAtom atom =
  let
    (V2 x y)    = atom ^. atomPosition
    body        = translate x y $ circle 10
    (V2 vx vy)  = atom ^. atomVelocity
    vectorSpeed = color aquamarine $ translate x y $ line [(0, 0), (vx, vy)]
  in
    pictures [body, vectorSpeed]

renderWall :: Wall -> Picture
renderWall wall = line [(x1, y1), (x2, y2)]
  where (Wall ((V2 x1 y1), (V2 x2 y2))) = wall

render :: World -> Picture
render world = pictures
  [ pictures $ map renderAtom (world ^. worldAtoms)
  , pictures $ map renderWall (world ^. worldWalls)
  ]

main :: IO ()
main = do
  let loAtom = Atom (V2 (-400) (-300)) (V2 (-50) (-50))
  let hiAtom = Atom (V2 400 300) (V2 50 50)
  atoms <- replicateM 100 $ getStdRandom $ randomR (loAtom, hiAtom)
  let walls =
        [ Wall (V2 (-500) (-500), V2 (500) (-500))
        , Wall (V2 (500) (-500), V2 (500) (500))
        , Wall (V2 (500) (500), V2 (-500) (500))
        , Wall (V2 (-500) (500), V2 (-500) (-500))
        ]
  let world = World atoms walls
  simulate window white 30 world render (const updateWorld)
