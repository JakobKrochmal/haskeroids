module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Set as S
import Data.Fixed

--TODO: Look into moving logic out into own modules

shipModel :: HaskeroidsGame -> Picture
shipModel game
  | acc game = pictures [
    (line [(0, (shL / 2)), (-(shW / 2), -(shL / 2))])
    ,(line [(0, (shL / 2)),((shW / 2), -(shL / 2))])
    ,(line [(-joint,-(shL / 3)),(joint,-(shL / 3))])
    ,(color gameColor $ polygon [(-(shW / 5), -(shL / 2)), ((shW / 5), -(shL / 2)), (0, -(shL / 1.3))])]
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

boardWidth :: Int
boardWidth = 1920

boardHeight :: Int
boardHeight = 1080

window :: Display
window = InWindow "Haskeroids" (boardWidth, boardHeight) (10, 10)

fps :: Int
fps = 60

background :: Color
background = black

gameColor :: Color
gameColor = green

data Asteroid = Asteroid {
  coords :: Point
  ,vel :: Vector
  ,size :: Int
} deriving (Show)

-- TODO: Make the ship its own type so that it can be used to draw lives and prettify code
data HaskeroidsGame =  Game { 
  shipCoords :: Point
  ,shipVel :: Vector
  ,shipRot :: Float
  ,asteroids :: [Asteroid]
  ,playerLives :: Int
  ,acc :: Bool
  ,turnLeft :: Bool
  ,turnRight :: Bool
  ,timePlayed :: Float
} deriving (Show)

initialState :: HaskeroidsGame
initialState = Game {
  shipCoords = (0, 0)
  ,shipVel = (0, 0)
  ,shipRot = 0
  ,asteroids = [(Asteroid (-250, -250) (30, 30) 3), (Asteroid (150, 150) (4, -45) 2), (Asteroid (410, -200) (-200, 20) 1)]
  ,playerLives = 2
  ,acc = False
  ,turnLeft = False
  ,turnRight = False
  ,timePlayed = 0
}

render :: HaskeroidsGame -> Picture
render game =
  pictures [
    ship,
    stones
--    debugtext
  ]
  where
    ship = uncurry translate (shipCoords game) $ rotate (shipRot game) $ color gameColor $ shipModel game
    stones = pictures $ map roidToPic $ asteroids game
--    debugtext = color white $ text $ (show $ shipCoords game)

roidToPic :: Asteroid -> Picture
roidToPic asteroid = uncurry translate (coords asteroid) $ color gameColor $ circle (10*(fromIntegral (size asteroid)))

moveAst :: Float -> Asteroid -> Asteroid
moveAst secs a = a {coords = checkPos (x', y')}
  where
    (x, y) = coords a
    (vx, vy) = vel a
    (x', y') = ((x + vx * secs), (y + vy * secs))

moveAsts :: Float -> HaskeroidsGame -> HaskeroidsGame
moveAsts secs game = game {asteroids = map (moveAst secs) $ asteroids game }

moveShip :: Float -> HaskeroidsGame -> HaskeroidsGame
moveShip secs game = game {shipCoords = checkPos (x',y')}
  where
    (x, y) = shipCoords game -- Old coordinates
    (vx, vy) = shipVel game -- Speed of ship
    x' = x + vx * secs 
    y' = y + vy * secs

checkPos :: Point -> Point
checkPos (x, y)
      | x > fromIntegral (boardWidth `div` 2) = checkPos (fromIntegral (-boardWidth `div` 2), y)
      | x < fromIntegral (-(boardWidth `div` 2)) = checkPos (fromIntegral (boardWidth `div` 2), y)
      | y > fromIntegral (boardHeight `div` 2) = checkPos (x, fromIntegral (-boardHeight `div` 2))
      | y < fromIntegral (-(boardHeight `div` 2)) = checkPos (x, fromIntegral (boardHeight `div` 2))
      | otherwise = (x, y)

accAmount :: Float
accAmount = 15.0

rotAmount :: Float
rotAmount = 7.0

fromDeg :: Float -> Float
fromDeg = (/180) . (*pi)

accShip :: (Float, Float) -> Float -> (Float, Float)
accShip (dx, dy) r = (dx + accAmount * (sin (fromDeg r)), dy + accAmount * (cos (fromDeg r)))

modifyGameIf :: (HaskeroidsGame -> Bool) -> (HaskeroidsGame -> HaskeroidsGame) -> HaskeroidsGame -> HaskeroidsGame
modifyGameIf pred mod game = if pred game then mod game else game

update :: Float -> HaskeroidsGame -> HaskeroidsGame
update secs = moveAsts secs . moveShip secs . modifyGameIf acc doAcc . modifyGameIf turnLeft doTurnLeft . modifyGameIf turnRight doTurnRight
  where doAcc game       = game { shipVel = accShip (shipVel game) (shipRot game)}
        doTurnLeft game  = game { shipRot = shipRot game - rotAmount }
        doTurnRight game = game { shipRot = shipRot game + rotAmount }

handleKeys :: Event -> HaskeroidsGame -> HaskeroidsGame
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game = game {acc = True }
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game = game {acc = False}
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game = game {turnLeft = True }
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) game = game {turnLeft = False}
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game = game {turnRight = True }
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) game = game {turnRight = False}
handleKeys _ game = game

main :: IO ()
main = play window background fps initialState render handleKeys update
