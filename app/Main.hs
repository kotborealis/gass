module Main where

import Lib

--import Graphics.Gloss

--window :: Display
--window = FullScreen
--
--renderAtom :: Atom -> Picture
--renderAtom atom = translate x y $ circle 10
--  where
--    (Atom point) = atom
--    (Point x y) = point
--
--render :: Picture
--render = pictures (map renderAtom (atoms $ initialWorld 100))
--
--main :: IO ()
--main = display window white render

main :: IO ()
main = putStrLn $ show $ initialWorld 100