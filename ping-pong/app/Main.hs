module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort


main :: IO ()
main = simulate window background fps initalState render update
  where
    fps = 30
