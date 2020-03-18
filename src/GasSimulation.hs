module GasSimulation
    ( V2(..),
      Atom(..),
      World(..),
      V2(..),
      updateWorld,
    ) where

import System.Random
import Linear.Metric
import Linear.V2

instance Random a => Random (V2 a) where
  random g = case random g of
   (a, g') -> case random g' of
     (b, g'') -> (V2 a b, g'')

  randomR (V2 a b, V2 c d) g = case randomR (a, c) g of
    (x, g') -> case randomR (b, d) g' of
      (y, g'') -> (V2 x y, g'')

atom_radius = 10

data Atom = Atom
  { position  :: V2 Float
  , speed     :: V2 Float
  } deriving (Show, Eq)

updateAtom :: Atom -> Atom
updateAtom atom = Atom {
      position = position atom + speed atom
    , speed = speed atom
  }

instance Random Atom where
    randomR (lo, hi) g =
        let (position', g1) = randomR (position lo, position hi) g
            (speed',    g2) = randomR (speed lo, speed hi) g1
        in (Atom position' speed', g2)

    random g =
        let (position', g1) = random g
            (speed',    g2) = random g1
        in (Atom position' speed', g2)

data World = World
  { atoms  :: [Atom]
  } deriving (Show)

updateWorld :: Float -> World -> World
updateWorld delta (World atoms) =
    let atoms' = map (updateAtom atoms) atoms
    in  World atoms'