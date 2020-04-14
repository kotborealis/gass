stack run


# collideEntities delta (PhysicsAtom atom) pw@(PhysicsWall (Wall (w1, w2)))
#     | denom < 0 = Nothing
#     | otherwise = Just $ Debug.Trace.trace
#         (  show collision
#         ++ "\n"
#         ++ show (p1, p2, a, b, c, d)
#         )
#         collision
#   where
#     move      = atom ^. atomVelocity ^* delta
#     b1        = w1
#     b2        = w2
#     a1 = atom ^. atomPosition
#     a2 = atom ^. atomPosition + move
#     denom = ((b2 ^. _2 - b1 ^. _2) * (a2 ^. _1 - a1 ^. _1)) -
#             ((b2 ^. _1 - b1 ^. _1) * (a2 ^. _2 - a1 ^. _2))
#     collision = Collision (PhysicsAtom atom, pw) delta (normalize move)