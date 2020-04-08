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

renderBounds :: World -> Picture

renderBounds world = 
  line [
    (btl ^. _1, btl ^. _2), 
    (bbr ^. _1, btl ^. _2), 
    (bbr ^. _1, bbr ^. _2), 
    (btl ^. _1, bbr ^. _2), 
    (btl ^. _1, btl ^. _2)
  ]
  where btl = world ^. (worldBounds . _1) :: V2 Float
        bbr = world ^. (worldBounds . _2) :: V2 Float


render :: World -> Picture
render world =
  pictures [pictures $ map renderAtom (world ^. worldAtoms), renderBounds world]

main :: IO ()
main = do
  let loAtom = Atom (V2 (-400) (-300)) (V2 (-50) (-50))
  let hiAtom = Atom (V2 400 300) (V2 50 50)
  atoms <- replicateM 100 $ getStdRandom $ randomR (loAtom, hiAtom)
  let world = World atoms (V2 (-500) (-500), V2 500 500)
  simulate window white 30 world render (const updateWorld)
