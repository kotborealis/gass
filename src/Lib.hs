module Lib
    ( Point(..),
      Atom(..),
      World(..),
      initialWorld,
    ) where

import RandomUtils

data Point = Point Float Float deriving (Show, Eq)

data Atom = Atom Point deriving (Show, Eq)

data World = World
  { atoms  :: [Atom]
  , width  :: Int
  , height :: Int
  } deriving (Show)

randAtoms :: (Float, Float) -> [Atom]
randAtoms range = map (Atom . uncurry Point) $ rndPairsRs range rndGen

initialWorld :: Int -> World
initialWorld entities =
    World
    {
      atoms  = take entities $ randAtoms (-200, 200)
    , width  = 100
    , height = 100
    }