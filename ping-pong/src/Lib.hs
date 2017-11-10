module Lib
    ( showTheWin
    ) where

import Graphics.Gloss

window :: Display
window = InWindow "Pingy Pong" (200, 300) (10, 10)

background :: Color
background = light black

drawing :: Picture
drawing = pictures
	[ color ballColor $ circleSolid 30
	, color paddleColor $ rectangleSolid 10 50
	]
	where
		ballColor = dark red
		paddleColor = light blue

showTheWin :: IO ()
showTheWin = display window background drawing