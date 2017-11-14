module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game



handleEvents :: Event -> Game -> Game
handleEvents (EventKey (Char 'r') _ _ _) game = initialState
handleEvents (EventKey (Char 'w') _ _ _) game = moveLeftPaddle paddleStep game
handleEvents (EventKey (Char 's') _ _ _) game = moveLeftPaddle (-paddleStep) game
handleEvents (EventKey (SpecialKey KeyUp) _ _ _) game = moveRightPaddle paddleStep game
handleEvents (EventKey (SpecialKey KeyDown) _ _ _) game = moveRightPaddle (-paddleStep) game
handleEvents _ game = game


main :: IO ()
main = play window background fps initialState render handleEvents update
  where
    fps = 30
