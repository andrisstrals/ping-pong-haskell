module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game


getPaddleY1 game = fst $ player1 game
getPaddleY2 game = fst $ player2 game


handleEvents :: Event -> Game -> Game
handleEvents (EventKey (Char 'r') Up _ _) game = if suspended game then game { ballLoc = (0, 0), ballVel = (200, 100), suspended = False } else game
handleEvents (EventKey (Char 'w') Down _ _) game = game { player1 = (y, MvUp  ) } where y = fst $ player1 game
handleEvents (EventKey (Char 'w') Up _ _)   game = game { player1 = (y, Stop  ) } where y = fst $ player1 game
handleEvents (EventKey (Char 's') Down _ _) game = game { player1 = (y, MvDown) } where y = fst $ player1 game
handleEvents (EventKey (Char 's') Up _ _)   game = game { player1 = (y, Stop  ) } where y = fst $ player1 game
handleEvents (EventKey (SpecialKey KeyUp) Down _ _)   game = game { player2 = (y, MvUp  ) } where y = fst $ player2 game 
handleEvents (EventKey (SpecialKey KeyUp) Up _ _)     game = game { player2 = (y, Stop  ) } where y = fst $ player2 game
handleEvents (EventKey (SpecialKey KeyDown) Down _ _) game = game { player2 = (y, MvDown) } where y = fst $ player2 game
handleEvents (EventKey (SpecialKey KeyDown) Up _ _)   game = game { player2 = (y, Stop  ) } where y = fst $ player2 game
handleEvents _ game = game


main :: IO ()
main = play window background fps initialState render handleEvents update
  where
    fps = 60
