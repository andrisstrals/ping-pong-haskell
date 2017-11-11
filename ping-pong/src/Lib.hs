module Lib
    ( showTheWin
    ) where

import Graphics.Gloss

screenW::Float
screenW = 600

screenH::Float
screenH = 480


background :: Color
background = light black

window :: Display
window = InWindow "Pingy Pong" (round screenW, round screenH) (10, 10)


data Game = Game { ballLoc :: (Float, Float)
                 , ballVel :: (Float, Float)
                 , player1 :: Float
                 , player2 :: Float
                 }

initalState::Game
initalState = Game (0,0) (0,0) 100 (-100)


render :: Game -> Picture
render g = pictures [ ball
                    , walls
                    , paddle1
                    , paddle2
                    ]
    where 
      ball = uncurry translate (ballLoc g) $ color ballColor $ circleSolid ballSize
      ballColor = dark red
      ballSize = 10

      wall :: Float -> Picture
      wall offset =
        translate 0 offset $
          color wallColor $
            rectangleSolid screenW  wallH

      wallColor = greyN 0.5
      walls = pictures [wall (screenH /2 - wallH), wall (screenH /(-2) + wallH)]

      mkPaddle :: Color -> Float -> Float -> Picture
      mkPaddle col x y = pictures
        [ translate x y $ color col $ rectangleSolid paddleThick paddleLen
        , translate x y $ color paddleFrameColor $ rectangleSolid (paddleThick - 6) (paddleLen - 6)
        ]
      paddleFrameColor = light blue

      paddle1 = mkPaddle rose  (screenW / (-2) + paddleThick * 1.5) $ player1 g
      paddle2 = mkPaddle green (screenW / 2 - paddleThick * 1.5) $ player2 g

      wallH::Float
      wallH = 10

      paddleLen::Float
      paddleLen = 80

      paddleThick::Float
      paddleThick = 20



showTheWin :: IO ()
showTheWin = display window background $ render initalState
