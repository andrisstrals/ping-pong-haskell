module Lib
    ( showTheWin
    ) where

import Graphics.Gloss

window :: Display
window = InWindow "Pingy Pong" (200, 300) (10, 10)

background :: Color
background = white

drawing :: Picture
drawing = circle 80

showTheWin :: IO ()
showTheWin = display window background drawing