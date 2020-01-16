module Entity where
import Graphics.Gloss

data Entity = Entity {
  coords :: Point
  ,vel :: Vector
  ,rot :: Float
}

type Ship = Entity
type Asteroid = Entity

shipModel :: Bool -> Ship -> Picture
shipModel accing ship
  | accing = pictures [
    (line [(0, (shL / 2)), (-(shW / 2), -(shL / 2))])
    ,(line [(0, (shL / 2)),((shW / 2), -(shL / 2))])
    ,(line [(-joint,-(shL / 3)),(joint,-(shL / 3))])
    ,(polygon [(-(shW / 5), -(shL / 2)), ((shW / 5), -(shL / 2)), (0, -(shL / 1.3))])]
  | otherwise = pictures [
    (line [(0, (shL / 2)), (-(shW / 2), -(shL / 2))])
    ,(line [(0, (shL / 2)),((shW / 2), -(shL / 2))])
    ,(line [(-joint,-(shL / 3)),(joint,-(shL / 3))])]
  where
    shW :: Float
    shW = 16
    shL :: Float
    shL = 24
    joint :: Float
    joint = shW/3

accShip :: Float -> Ship -> Ship
accShip amount ship = ship { vel = (dx + amount * (sin (fromDeg r)), dy + amount * (cos (fromDeg r)))}
  where
    dx = fst $ vel ship
    dy = snd $ vel ship
    r = rot ship
    fromDeg :: Float -> Float
    fromDeg = (/180) . (*pi)

asteroidScale :: Float
asteroidScale = 3.5

asteroidModel :: Asteroid -> Picture
asteroidModel a = line $ map (tupMul asteroidScale) [(0, 20), (14, 14), (9, 8), (11, -4), (6, -14), (0, -13), (-6, -6), (-11, 0), (-14, 6), (-7, 12), (-11, 17), (0,20)] 
  where
    tupMul :: Float -> Point -> Point
    tupMul r (x, y) = (r*x, r*y)

roidToPic :: Asteroid -> Picture
roidToPic asteroid = uncurry translate (coords asteroid) $ rotate (rot asteroid) $ asteroidModel asteroid

moveAst :: Float -> Asteroid -> Asteroid
moveAst secs a = a {coords = (x', y'), rot = 0.5 + rot a}
  where
    (x, y) = coords a
    (vx, vy) = vel a
    (x', y') = ((x + vx * secs), (y + vy * secs))
