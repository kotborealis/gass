module Lib
    ( Point(..),
      Atom(..),
      World(..),
    ) where

import System.Random

data Point = Point Float Float deriving (Show, Eq)

instance Random Point where
    randomR (Point lx ly, Point hx hy) g =
        let (x, g1) = randomR (lx, hx) g
            (y, g2) = randomR (ly, hy) g1
        in (Point x y, g2)

    random g =
        let (x, g1) = random g
            (y, g2) = random g1
        in (Point x y, g2)

data Atom = Atom
  { position :: Point
  , speed    :: Point
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
  , width  :: Float
  , height :: Float
  } deriving (Show)