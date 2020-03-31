{-# LANGUAGE TemplateHaskell #-}

module GasSimulation
    ( V2(..)
    , Atom(..)
    , atomVelocity
    , atomPosition
    , World(..)
    , updateWorld
    )
where

import           System.Random
import           Linear
import           Linear.Metric
import           Linear.V2
import           Linear.Vector
import           Data.List
import           Data.Maybe
import           Debug.Trace
import           Control.Lens

-- Returns a list of 2-combinations without repetition.
pairs xs = [ (a, b) | (a : as) <- init . tails $ xs, b <- as ]

instance Random a => Random (V2 a) where
    random g = case random g of
        (a, g') -> case random g' of
            (b, g'') -> (V2 a b, g'')

    randomR (V2 a b, V2 c d) g = case randomR (a, c) g of
        (x, g') -> case randomR (b, d) g' of
            (y, g'') -> (V2 x y, g'')

atomRadius = 10

data Atom = Atom{
    _atomPosition  :: V2 Float,
    _atomVelocity  :: V2 Float
  } deriving (Show, Eq)

makeLenses ''Atom

instance Random Atom where
    randomR (lo, hi) g =
        let (atomPosition', g1) =
                    randomR (lo ^. atomPosition, hi ^. atomPosition) g
            (atomVelocity', g2) =
                    randomR (lo ^. atomVelocity, hi ^. atomVelocity) g1
        in  (Atom atomPosition' atomVelocity', g2)

    random g =
        let (atomPosition', g1) = random g
            (atomVelocity', g2) = random g1
        in  (Atom atomPosition' atomVelocity', g2)

data Collision = Collision {
    _collisionAtoms :: (Atom, Atom),
    _collisionTime :: Float,
    _collisionNormal :: V2 Float
} deriving (Eq, Show)

makeLenses ''Collision

instance Ord Collision where
    a <= b = a ^. collisionTime <= b ^. collisionTime

collisionAtoms_ :: Collision -> [Atom]
collisionAtoms_ collision = [a, b] where (a, b) = collision ^. collisionAtoms

data World = World {
    atoms  :: [Atom]
  } deriving (Eq, Show)

magnitude :: V2 Float -> Float
magnitude = distance Linear.zero

collideAtoms :: Float -> Atom -> Atom -> Maybe Collision
collideAtoms delta lha rha | magnitude move < dist               = Nothing
                           | d <= 0                              = Nothing
                           | f >= sumRadii ** 2                  = Nothing
                           | t < 0                               = Nothing
                           | magnitude move < distUntilCollision = Nothing
                           | otherwise = Just collision

  where
    move               = (lha ^. atomVelocity - rha ^. atomVelocity) ^* delta
    dist = distance (lha ^. atomPosition) (rha ^. atomPosition) - sumRadii
    sumRadii           = atomRadius * 2
    moveNormalized     = normalize move
    center             = rha ^. atomPosition - lha ^. atomPosition
    normal             = normalize center
    d                  = dot moveNormalized center
    f                  = magnitude center ** 2 - d ** 2
    t                  = sumRadii ** 2 - f
    distUntilCollision = d - sqrt t
    time               = delta * (distUntilCollision / magnitude move)
    collision          = Collision (lha, rha) time normal


calculateCollisions :: Float -> [Atom] -> [Collision]
calculateCollisions delta atoms =
    mapMaybe (uncurry (collideAtoms delta)) $ pairs atoms

integrateAtoms :: Float -> [Atom] -> [Atom]
integrateAtoms delta = map (integrateAtom delta)

integrateAtom :: Float -> Atom -> Atom
integrateAtom 0     atom = atom
integrateAtom delta atom = atom & atomPosition .~ atomPosition''
  where
    atomPosition'  = atom ^. atomPosition
    atomVelocity'  = atom ^. atomVelocity
    -- TODO lens
    atomPosition'' = atomPosition' + (atomVelocity' ^* delta)

runPhysics :: Float -> [Atom] -> [Atom]
runPhysics 0 atoms = atoms
runPhysics delta atoms | null collisions = integrateAtoms delta atoms
                       | tFirst > delta  = integrateAtoms delta atoms
                       | otherwise       = runPhysics delta' atoms'

  where
    collisions        = calculateCollisions delta atoms
    firstCollision    = minimum collisions
    tFirst            = firstCollision ^. collisionTime
    tFirst'           = tFirst - 1.0e-3
    delta'            = delta - tFirst'
    nonCollidingAtoms = atoms \\ collisionAtoms_ firstCollision
    atoms' =
        integrateAtoms tFirst' nonCollidingAtoms
            ++ resolveCollision tFirst' firstCollision

resolveCollision :: Float -> Collision -> [Atom]
resolveCollision delta collision 
    | separatingVelocity > 0 = [a', b']
    | otherwise = [a' & atomVelocity .~ v1', b' & atomVelocity .~ v2']
  where
    (a, b)             = collision ^. collisionAtoms
    a'                 = integrateAtom delta a
    b'                 = integrateAtom delta b
    v1                 = a ^. atomVelocity
    v2                 = b ^. atomVelocity
    n                  = collision ^. collisionNormal
    separatingVelocity = (-1) * dot (v1 - v2) n
    a1                 = dot v1 n
    a2                 = dot v2 n
    p                  = a1 - a2
    v1'                = v1 - p *^ n
    v2'                = v2 + p *^ n

updateWorld :: Float -> World -> World
updateWorld delta world = world { atoms = runPhysics delta (atoms world) }