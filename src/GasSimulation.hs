module GasSimulation
    ( V2(..)
    , Atom(..)
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
    atomPosition  :: V2 Float,
    atomVelocity     :: V2 Float
  } deriving (Show, Eq)

instance Random Atom where
    randomR (lo, hi) g =
        let (atomPosition', g1) = randomR (atomPosition lo, atomPosition hi) g
            (atomVelocity', g2) = randomR (atomVelocity lo, atomVelocity hi) g1
        in  (Atom atomPosition' atomVelocity', g2)

    random g =
        let (atomPosition', g1) = random g
            (atomVelocity', g2) = random g1
        in  (Atom atomPosition' atomVelocity', g2)

data Collision = Collision {
    collisionAtoms :: (Atom, Atom),
    collisionResolution :: V2 Float,
    collisionTime :: Float
} deriving (Eq, Show)

instance Ord Collision where
    a <= b = collisionTime a <= collisionTime b

collisionAtoms_ :: Collision -> [Atom]
collisionAtoms_ collision = [a, b] where (a, b) = collisionAtoms collision

data World = World {
    atoms  :: [Atom]
  } deriving (Eq, Show)

magnitude :: V2 Float -> Float
magnitude = distance Linear.zero

collideAtoms :: Float -> Atom -> Atom -> Maybe Collision
collideAtoms delta lha rha
    | magnitude move < dist = Nothing
    | d <= 0 = Nothing
    | f >= sumRadii ** 2 = Nothing
    | t < 0 = Nothing
    | magnitude move < distUntilCollision = Nothing
    | otherwise = Just collision

  where
    move               = (atomVelocity lha - atomVelocity rha) ^* delta
    dist = distance (atomPosition lha) (atomPosition rha) - sumRadii
    sumRadii           = atomRadius * 2
    moveNormalized     = normalize move
    center             = atomPosition rha - atomPosition lha
    d                  = dot moveNormalized center
    f                  = (magnitude center) ** 2 - d ** 2
    t                  = sumRadii ** 2 - f
    distUntilCollision = d - sqrt t
    resolution         = moveNormalized ^* distUntilCollision
    time               = delta * (magnitude resolution / magnitude move)
    collision          = Collision (lha, rha) resolution time


calculateCollisions :: Float -> [Atom] -> [Collision]
calculateCollisions delta atoms =
    mapMaybe (uncurry (collideAtoms delta)) $ pairs atoms

integrateAtoms :: Float -> [Atom] -> [Atom]
integrateAtoms delta = map (integrateAtom delta)

integrateAtom :: Float -> Atom -> Atom
integrateAtom 0     atom = atom
integrateAtom delta atom = atom { atomPosition = atomPosition'' }
  where
    atomPosition'  = atomPosition atom
    atomVelocity'  = atomVelocity atom
    atomPosition'' = atomPosition' + (atomVelocity' ^* delta)

runPhysics :: Float -> [Atom] -> [Atom]
runPhysics 0 atoms = atoms
runPhysics delta atoms | null collisions = integrateAtoms delta atoms
                       | tFirst > delta  = integrateAtoms delta atoms
                       | otherwise       = runPhysics delta' atoms'

  where
    collisions        = calculateCollisions delta atoms
    firstCollision    = minimum collisions
    tFirst            = collisionTime firstCollision
    tFirst'           = tFirst - 1.0e-3
    delta'            = delta - tFirst'
    nonCollidingAtoms = atoms \\ collisionAtoms_ firstCollision
    atoms' =
        integrateAtoms tFirst' nonCollidingAtoms
            ++ resolveCollision tFirst' firstCollision

resolveCollision :: Float -> Collision -> [Atom]
resolveCollision delta collision = [
        a {atomVelocity = negate (atomVelocity a)}, 
        b {atomVelocity = negate (atomVelocity b)}
    ]
    where (a, b) = collisionAtoms collision

updateWorld :: Float -> World -> World
updateWorld delta world = world
    { atoms = runPhysics
                  (Debug.Trace.trace ("upate world delta: " ++ show delta) delta
                  )
                  (atoms world)
    }
