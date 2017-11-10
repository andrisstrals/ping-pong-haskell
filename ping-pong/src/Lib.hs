module Lib
    ( showTheWin
    ) where

import Graphics.Gloss

screenW::Float
screenW = 600

screenH::Float
screenH = 480

wallH::Float
wallH = 10

paddleLen::Float
paddleLen = 80

paddleThick::Float
paddleThick = 20

window :: Display
window = InWindow "Pingy Pong" (round screenW, round screenH) (10, 10)

background :: Color
background = light black

drawing :: Picture
drawing = pictures [ ball
				   , walls
                   , mkPaddle rose (screenW /2 - paddleThick * 1.5) (-20)
                   , mkPaddle orange (screenW /(-2) + paddleThick * 1.5) 40
                   ]
  where
    --  The pong ball.
    ball = translate (-10) 40 $ color ballColor $ circleSolid 10
    ballColor = dark red

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid screenW  wallH

    wallColor = greyN 0.5
    walls = pictures [wall (screenH /2 - wallH), wall (screenH /(-2) + wallH)]

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid paddleThick paddleLen
      , translate x y $ color paddleColor $ rectangleSolid (paddleThick - 6) (paddleLen - 6)
      ]

    paddleColor = light (light blue)
showTheWin :: IO ()
showTheWin = display window background drawing