module Game where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss

import Entity

data HaskeroidsGame =  Game { 
  ship :: Ship
  ,asteroids :: [Asteroid]
  ,bullets :: [Bullet]
  ,playerLives :: Int
  ,accInput :: Bool
  ,turnLeftInput :: Bool
  ,turnRightInput :: Bool
  ,timePlayed :: Float
  ,toSpawn :: Int
  ,gameColor :: Color
  ,boardWidth :: Int
  ,boardHeight :: Int
  ,accAmount :: Float
  ,rotAmount :: Float
}

render :: HaskeroidsGame -> Picture
render game =
  pictures $ map (Color $ gameColor game) [
    boat,
    stones,
    pews
  ]
  where
    boat = drawShip (accInput game) (ship game)
    stones = pictures $ map drawAsteroid $ asteroids game
    pews = pictures $ map drawBullet $ bullets game

move :: Int -> Int -> Float -> Entity -> Entity
move width height secs ent = ent {coords = checkPos width height (x', y')}
  where
    (x, y) = coords ent
    (dx, dy) = vel ent
    x' = x + dx * secs
    y' = y + dy * secs

checkPos :: Int -> Int -> Point -> Point
checkPos boardWidth boardHeight (x, y)
      | x > fromIntegral (boardWidth `div` 2) = checkPos boardWidth boardHeight (fromIntegral (-boardWidth `div` 2), y)
      | x < fromIntegral (-(boardWidth `div` 2)) = checkPos boardWidth boardHeight (fromIntegral (boardWidth `div` 2), y)
      | y > fromIntegral (boardHeight `div` 2) = checkPos boardWidth boardHeight (x, fromIntegral (-boardHeight `div` 2))
      | y < fromIntegral (-(boardHeight `div` 2)) = checkPos boardWidth boardHeight (x, fromIntegral (boardHeight `div` 2))
      | otherwise = (x, y)

rotShipLeft :: Float -> Ship -> Ship
rotShipLeft amount ship = ship { rot = (rot ship) - amount}

rotShipRight :: Float -> Ship -> Ship
rotShipRight amount ship = ship {rot = (rot ship) + amount}

moveBullet :: Int -> Int -> Float -> Bullet -> Bullet
moveBullet w h secs b = Bullet {spatial = move w h secs (spatial b), timeLived = (timeLived b) + secs}

spawnBullet :: HaskeroidsGame -> HaskeroidsGame
spawnBullet game = game {bullets = (Bullet (Entity (coords $ ship game) (800.0 * (sin (fromDeg $ rot $ ship game)),800.0 * (cos (fromDeg $ rot $ ship game))) (rot $ ship game)) 0.0) : (bullets game)} 

despawnBullets :: HaskeroidsGame -> HaskeroidsGame
despawnBullets g = g {bullets = destroyBullets $ bullets g}
  where
    destroyBullets :: [Bullet] -> [Bullet]
    destroyBullets [] = []
    destroyBullets (b:bs)
      | timeLived b > 4.0 = destroyBullets bs
      | otherwise = b : (destroyBullets bs)

moveEnts :: Float -> HaskeroidsGame -> HaskeroidsGame
moveEnts secs game = game {ship = move w h secs s, asteroids = map (move w h secs) (asteroids game), bullets = map (moveBullet w h secs) (bullets game)}
  where
    w = boardWidth game
    h = boardHeight game
    s = ship game

modifyGameIf :: (HaskeroidsGame -> Bool) -> (HaskeroidsGame -> HaskeroidsGame) -> HaskeroidsGame -> HaskeroidsGame
modifyGameIf pred mod game = if pred game then mod game else game

update :: Float -> HaskeroidsGame -> HaskeroidsGame
update secs = moveEnts secs . despawnBullets . modifyGameIf accInput doAcc . modifyGameIf turnLeftInput doTurnLeft . modifyGameIf turnRightInput doTurnRight
  where doTurnLeft game       = game { ship = rotShipLeft (rotAmount game) (ship game) }
        doTurnRight game  = game { ship = rotShipRight (rotAmount game) (ship game) }
        doAcc game = game { ship = accShip (accAmount game) (ship game) }

handleKeys :: Event -> HaskeroidsGame -> HaskeroidsGame
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game = game {accInput = True }
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game = game {accInput = False}
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game = game {turnLeftInput = True }
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) game = game {turnLeftInput = False}
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game = game {turnRightInput = True }
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) game = game {turnRightInput = False}
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) game = spawnBullet game
handleKeys _ game = game
