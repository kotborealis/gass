module RandomUtils
  (
    rndGen,
    rndPairsRs
  )

where

import System.Random

rndSeed = 453452313992 :: Int
rndGen  = mkStdGen rndSeed

rndPairsRs :: (RandomGen g, Random a) => (a, a) -> g -> [(a, a)]
rndPairsRs range gen = zip xs ys
  where (a, b) = split gen
        xs = randomRs range a
        ys = randomRs range b