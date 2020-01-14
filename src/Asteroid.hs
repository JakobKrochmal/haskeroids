module Asteroid where

import Graphics.Gloss

data Asteroid = Asteroid {
  coords :: Point
  ,vel :: Vector
  ,size :: Int
  ,rot :: Float
} deriving (Show)

asteroidScale :: Float
asteroidScale = 3.5

asteroidModel :: Asteroid -> Picture
asteroidModel a = line $ map (tupMul asteroidScale) [(0, 20), (14, 14), (9, 8), (11, -4), (6, -14), (0, -13), (-6, -6), (-11, 0), (-14, 6), (-7, 12), (-11, 17), (0,20)] 
  where
    tupMul :: Float -> Point -> Point
    tupMul r (x, y) = (r*x, r*y)


