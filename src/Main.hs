module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)

import qualified Data.Set as S
import Data.Fixed

import Entity
import Game



window :: Display
window = InWindow "Haskeroids" (boardWidth initialState, boardHeight initialState) (10, 10)

fps :: Int
fps = 60

background :: Color
background = black

initialState :: HaskeroidsGame
initialState = Game {
  ship = Entity (0.0, 0.0) (0.0, 0.0) 0.0 
  ,asteroids = [(Entity (-250, -250) (30, 30) 3.0), (Entity (150, 150) (4, -45) 2.0), (Entity (410, -200) (-200, 20) 1.0)]
  ,playerLives = 2
  ,accInput = False
  ,turnLeftInput = False
  ,turnRightInput = False
  ,timePlayed = 0
  ,toSpawn = 4
  ,gameColor = white
  ,boardWidth = 1920
  ,boardHeight = 1080
  ,accAmount = 15.0
  ,rotAmount = 7.0
}

main :: IO ()
main = play window background fps initialState render handleKeys update
