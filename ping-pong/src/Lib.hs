module Lib
    ( showTheWin,
      render
    ) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

screenW::Float
screenW = 600

screenH::Float
screenH = 480


background :: Color
background = light black

window :: Display
window = InWindow "Pingy Pong" (round screenW, round screenH) (100, 100)


data Game = Game { ballLoc :: (Float, Float)
                 , ballVel :: (Float, Float)
                 , player1 :: Float
                 , player2 :: Float
                 }

initalState::Game
initalState = Game (0,0) (60, 40) 100 (-100)


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


moveBall :: Float -> Game -> Game
moveBall sec game = game { ballLoc = (x1, y2) }
    where 
      --old ball location and velocity
      (x, y) = ballLoc game
      (vx, vy) = ballVel game

      --new ball location
      x1 = x + sec * vx
      y2 = y + sec * vy

showTheWin :: IO ()
showTheWin = simulate window background fps initalState render update
  where
    fps = 60
    update :: ViewPort -> Float -> Game -> Game
    update _ = moveBall