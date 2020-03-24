{-# LANGUAGE RecordWildCards #-}

module GasSimulation
    ( V2(..),
      Atom(..),
      World(..),
      updateWorld,
    ) where

import System.Random
import Linear.Metric
import Linear.V2
import Linear.Vector

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

magnitude :: V2 Float -> Float
magnitude (V2 x y) = sqrt $ x*x + y*y

checkCollision :: Float -> Atom -> Atom -> Maybe (V2 Float)
checkCollision delta (Atom lp ls) (Atom rp rs)
    | (distance lp rp) - sumRadii > magnitude n = Nothing
    | d <= 0 = Nothing
    | f >= sumRadiiSquared = Nothing
    | t < 0 = Nothing
    | mag < distance' = Nothing
    | otherwise = Just (n ^* distance')
    where
        sumRadii = atom_radius * 2
        sumRadiiSquared = sumRadii ** 2
        c = lp - rp
        mv = (ls - rs) ^* delta
        n = normalize mv
        d = dot n c
        f = (magnitude c) * (magnitude c) - (d * d)
        t = sumRadiiSquared - f
        distance' = d - sqrt t
        mag = magnitude mv

updateAtom :: Float -> World -> Atom -> Atom
updateAtom delta world atom = case minimum ( map (checkCollision delta atom) (atoms world) ) of
  Just res -> atom { position = (position atom) + res }
  Nothing -> atom { position = (position atom) + (speed atom) ^* delta }


updateAtoms :: Float -> World -> World
updateAtoms delta world = world { atoms = map (updateAtom delta world) (atoms world) }

updateWorld :: Float -> World -> World
updateWorld delta world = updateAtoms delta world