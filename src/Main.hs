module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Set as S

boardWidth :: Int
boardWidth = 1000

boardHeight :: Int
boardHeight = 1000

window :: Display
window = InWindow "Haskeroids" (boardWidth, boardHeight) (10, 10)

fps :: Int
fps = 60

background :: Color
background = black

shipColor :: Color
shipColor = white

data HaskeroidsGame =  Game { 
  keys :: S.Set Key
  ,shipCoords :: (Float, Float)
  ,shipVel :: (Float, Float)
  ,shipRot :: Float
  ,playerLives :: Int
} deriving (Show)

render :: HaskeroidsGame -> Picture
render game =
  pictures [
    ship
  ]
  where
    ship = uncurry translate (shipCoords game) $ color shipColor $ rectangleSolid 10 40

moveShip :: Float -> HaskeroidsGame -> HaskeroidsGame
moveShip secs game = game {shipCoords = checkPos (x',y')}
  where
    (x, y) = shipCoords game -- Old coordinates
    (vx, vy) = shipVel game -- Speed of ship
    x' = (x + vx) * secs 
    y' = (y + vy) * secs
    checkPos :: (Float, Float) -> (Float, Float)
    checkPos (x, y)
      | x > (fromIntegral boardWidth) = checkPos ((x - (fromIntegral boardWidth)), y)
      | y > (fromIntegral boardHeight) = (x, y - (fromIntegral boardHeight))
      | otherwise = (x, y)

initialState :: HaskeroidsGame
initialState = Game {
  keys = S.empty
  ,shipCoords = (0, 0)
  ,shipVel = (0, 0)
  ,shipRot = 0
  ,playerLives = 2
}

accAmount :: Float
accAmount = 100.0

accShip :: (Float, Float) -> Float -> (Float, Float)
accShip (x, y) r = (x + accAmount * (cos r), y + accAmount * (sin r))

update :: Float -> HaskeroidsGame -> HaskeroidsGame
update secs game
  | S.member (SpecialKey KeyUp) (keys game) = moveShip secs $ game { shipVel = accShip (shipVel game) (shipRot game) }
  | S.member (SpecialKey KeyLeft) (keys game) = moveShip secs $ game { shipRot = (shipRot game) - 2 }
  | S.member (SpecialKey KeyRight) (keys game) = moveShip secs $ game { shipRot = (shipRot game) + 2}
  | otherwise = moveShip secs $ game

handleKeys :: Event -> HaskeroidsGame -> HaskeroidsGame
handleKeys (EventKey k Down _ _) game = game { keys = S.insert k (keys game)}
handleKeys (EventKey k Up _ _) game = game { keys = S.delete k (keys game)}
handleKeys _ game = game


main :: IO ()
main = play window background fps initialState render handleKeys update
