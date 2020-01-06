module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Set as S
import Data.Fixed

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

shipColor :: Color
shipColor = white

data HaskeroidsGame =  Game { 
  shipCoords :: (Float, Float)
  ,shipVel :: (Float, Float)
  ,shipRot :: Float
  ,playerLives :: Int
  ,acc :: Bool
  ,turnLeft :: Bool
  ,turnRight :: Bool
} deriving (Show)

render :: HaskeroidsGame -> Picture
render game =
  pictures [
    ship,
    debugtext
  ]
  where
    ship = uncurry translate (shipCoords game) $ rotate (shipRot game) $ color shipColor $ rectangleSolid 10 40
    debugtext = color white $ text $ (show (shipCoords game))

moveShip :: Float -> HaskeroidsGame -> HaskeroidsGame
moveShip secs game = game {shipCoords = (x',y')}
  where
    (x, y) = shipCoords game -- Old coordinates
    (vx, vy) = shipVel game -- Speed of ship
    x' = x + vx * secs 
    y' = y + vy * secs
    checkPos :: (Float, Float) -> (Float, Float)
    checkPos (x, y) = ((x `mod'` (fromIntegral boardWidth)),(y `mod'` (fromIntegral boardHeight))) 

initialState :: HaskeroidsGame
initialState = Game {
  shipCoords = (0, 0)
  ,shipVel = (0, 0)
  ,shipRot = 0
  ,playerLives = 2
  ,acc = False
  ,turnLeft = False
  ,turnRight = False
}

accAmount :: Float
accAmount = 15.0

rotAmount :: Float
rotAmount = 7.0

fromDeg :: Float -> Float
fromDeg = (/180) . (*pi)

accShip :: (Float, Float) -> Float -> (Float, Float)
accShip (x, y) r = (x + accAmount * (sin (fromDeg r)), y + accAmount * (cos (fromDeg r)))

update :: Float -> HaskeroidsGame -> HaskeroidsGame
update secs game
  | (acc game) && (turnLeft game) = moveShip secs $ game { shipVel = accShip (shipVel game) (shipRot game), shipRot = (((shipRot game)-rotAmount) `mod'` 360)}
  | (acc game) && (turnRight game) = moveShip secs $ game { shipVel = accShip (shipVel game) (shipRot game), shipRot = (((shipRot game)+rotAmount) `mod'` 360)}
  | acc game = moveShip secs $ game { shipVel = accShip (shipVel game) (shipRot game)}
  | turnLeft game = moveShip secs $ game { shipRot = (((shipRot game)-rotAmount) `mod'` 360) }
  | turnRight game = moveShip secs $ game { shipRot = (((shipRot game)+rotAmount) `mod'` 360) }
  | otherwise = moveShip secs $ game

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
