{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -Wall #-}

module GasSimulation
    ( V2(..)
    , Atom(..)
    , atomVelocity
    , atomPosition
    , World(..)
    , worldAtoms
    , worldWalls
    , updateWorld
    , Wall(..)
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
  }
  deriving (Show, Eq)

makeLenses ''Atom

data Wall = Wall (V2 Float, V2 Float) deriving (Show, Eq)

data PhysicsEntity = PhysicsAtom Atom | PhysicsWall Wall deriving (Show, Eq)

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
    _collisionEntities :: (PhysicsEntity, PhysicsEntity),
    _collisionTime :: Float,
    _collisionNormal :: V2 Float
} deriving (Eq, Show)

makeLenses ''Collision

instance Ord Collision where
    a <= b = a ^. collisionTime <= b ^. collisionTime

collisionAtoms :: Collision -> [Atom]
collisionAtoms (Collision (PhysicsAtom a, PhysicsWall _) _ _) = [a]
collisionAtoms (Collision (PhysicsAtom a, PhysicsAtom b) _ _) = [a, b]

data World = World {
    _worldAtoms  :: [Atom],
    _worldWalls :: [Wall]
  } deriving (Eq, Show)

makeLenses ''World

magnitude :: V2 Float -> Float
magnitude = distance Linear.zero

collideEntities :: Float -> PhysicsEntity -> PhysicsEntity -> Maybe Collision

collideEntities delta (PhysicsAtom lha) (PhysicsAtom rha)
    | magnitude move < dist               = Nothing
    | d <= 0                              = Nothing
    | f >= sumRadii ** 2                  = Nothing
    | t < 0                               = Nothing
    | magnitude move < distUntilCollision = Nothing
    | otherwise                           = Just collision

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
    collision = Collision (PhysicsAtom lha, PhysicsAtom rha) time normal

collideEntities _ (PhysicsWall _) (PhysicsWall _) = Nothing

collideEntities delta w@(PhysicsWall _) a@(PhysicsAtom _) =
    collideEntities delta a w

collideEntities delta (PhysicsAtom atom) pw@(PhysicsWall (Wall (w1, w2)))
    | d < 0     = Nothing
    | otherwise = Just collision
  where
    move      = atom ^. atomVelocity ^* delta
    p1        = w1 - atom ^. atomPosition - move
    p2        = w2 - atom ^. atomPosition - move
    k         = p2 - p1
    a         = (k ^. _1) ** 2 + (k ^. _2) ** 2
    b         = 2 * (k ^. _1 * p1 ^. _1 + k ^. _2 * p1 ^. _2)
    c         = (p1 ^. _1) ** 2 + (p1 ^. _2) ** 2 - atomRadius ** 2
    d         = b ** 2 - 4 * a * c
    u1        = ((-1) * b + sqrt d) / (2 * a)
    u2        = ((-1) * b - sqrt d) / (2 * a)
    u         = (u1 + u2) / 2
    cp        = w1 + k ^* u
    center      = cp - atom ^. atomPosition
    n = normalize center
    distUntilCollision = magnitude (center - move) - atomRadius
    time               = delta * (distUntilCollision / magnitude move)
    collision = Collision (PhysicsAtom atom, pw) time n



calculateCollisions :: Float -> [PhysicsEntity] -> [Collision]
calculateCollisions delta entities =
    mapMaybe (uncurry (collideEntities delta)) $ pairs entities

integrateAtoms :: Float -> [Atom] -> [Atom]
integrateAtoms delta = map (integrateAtom delta)

integrateAtom :: Float -> Atom -> Atom
integrateAtom 0 atom = atom
integrateAtom delta atom =
    atom & atomPosition %~ (+ atom ^. atomVelocity ^* delta)

runPhysics :: Float -> World -> World
runPhysics 0 world = world
runPhysics delta world
    | null collisions = world & worldAtoms %~ integrateAtoms delta
    | tFirst > delta  = world & worldAtoms %~ integrateAtoms delta
    | otherwise       = runPhysics delta' (world & worldAtoms .~ atoms')

  where
    atoms      = world ^. worldAtoms
    walls      = world ^. worldWalls
    collisions = calculateCollisions
        delta
        (map PhysicsAtom atoms ++ map PhysicsWall walls)
    firstCollision    = minimum collisions
    tFirst            = firstCollision ^. collisionTime
    delta'            = delta - tFirst
    nonCollidingAtoms = atoms \\ collisionAtoms firstCollision
    atoms'            = integrateAtoms tFirst nonCollidingAtoms
        ++ resolveCollision firstCollision

resolveCollision :: Collision -> [Atom]
resolveCollision collision@(Collision (PhysicsAtom a, PhysicsAtom b) _ _)
    | separatingVelocity > 0 = [a', b']
    | otherwise = [a' & atomVelocity .~ v1', b' & atomVelocity .~ v2']
  where
    delta              = collision ^. collisionTime
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


resolveCollision collision@(Collision (PhysicsAtom a, PhysicsWall _) _ _)
    | separatingVelocity > 0 = [a']
    | otherwise              = [a' & atomVelocity .~ v1']
  where
    delta              = collision ^. collisionTime
    a'                 = integrateAtom delta a
    v1                 = a ^. atomVelocity
    n                  = collision ^. collisionNormal
    separatingVelocity = (-1) * dot v1 n
    nn = dot n n
    v1' = v1 - (2 ^* ((-1)*separatingVelocity / nn)) * n


updateWorld :: Float -> World -> World
updateWorld = runPhysics
