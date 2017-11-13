module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game


moveRightPaddle :: Float -> Game -> Game
moveRightPaddle step game = game { player2 = (player2 game) + step }

moveLeftPaddle :: Float -> Game -> Game
moveLeftPaddle step game = game { player1 = (player1 game) + step }

step = 10

handleEvents :: Event -> Game -> Game
handleEvents (EventKey (Char 'r') _ _ _) game = initialState
handleEvents (EventKey (Char 'w') _ _ _) game = moveLeftPaddle step game
handleEvents (EventKey (Char 's') _ _ _) game = moveLeftPaddle (-step) game
handleEvents (EventKey (SpecialKey KeyUp) _ _ _) game = moveRightPaddle step game
handleEvents (EventKey (SpecialKey KeyDown) _ _ _) game = moveRightPaddle (-step) game
handleEvents _ game = game


main :: IO ()
main = play window background fps initialState render handleEvents update
  where
    fps = 30
