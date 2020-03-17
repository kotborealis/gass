module Lib
    ( Point(..),
      Atom(..),
      World(..),
      initialWorld,
    ) where

import RandomUtils

data Point = Point Float Float deriving (Show, Eq)

data Atom = Atom
  { position :: Point
  , speed    :: Point
  } deriving (Show, Eq)

data World = World
  { atoms  :: [Atom]
  , width  :: Int
  , height :: Int
  } deriving (Show)

normalize :: (Float, Float) -> (Float, Float) -> (Float, Float)
normalize (xr, yr) (xd, yd) = ((xd/xr), (yd/yr))

randAtoms :: (Float, Float) -> [Atom]
randAtoms range = map (\(position, speed) ->
      Atom { position = uncurry Point $ position, speed = uncurry Point $ normalize (10, 10) speed}
    ) atomsData
  where positions = rndPairsRs range rndGen
        speeds    = rndPairsRs range rndGen
        atomsData = zip positions speeds

initialWorld :: Int -> World
initialWorld entities =
    World
    {
      atoms  = take entities $ randAtoms (-200, 200)
    , width  = 100
    , height = 100
    }