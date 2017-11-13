module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game


handleEvents :: Event -> Game -> Game
handleEvents (EventKey (Char 'r') _ _ _) game = game { ballLoc = (0, 0) }


main :: IO ()
main = play window background fps initalState render handleEvents update
  where
    fps = 30
