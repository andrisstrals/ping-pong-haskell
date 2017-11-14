module Lib
    ( initialState
    , render
    , window
    , background
    , moveBall
    , update
    , initialState
    , Game(..)
    ) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

type Position = (Float, Float)

data Game = Game { ballLoc :: (Float, Float)
                 , ballVel :: (Float, Float)
                 , player1 :: Float
                 , player2 :: Float
                 }

-- Various constants used
screenW::Float
screenW = 800

screenH::Float
screenH = 480


background :: Color
background = light black


wallH::Float
wallH = 10

paddleLen::Float
paddleLen = 80

paddleThick::Float
paddleThick = 20

ballRadius :: Float
ballRadius = 10


-- Initial game state definition
initialState::Game
initialState = Game (0, 0) (200, 100) 0 0


-- Main window
window :: Display
window = InWindow "Pingy Pong" (round screenW, round screenH) (100, 100)


-- Render single game frame
render :: Game -> Picture
render g = pictures [ ball
                    , walls
                    , paddle1
                    , paddle2
                    ]
    where 
      ball = uncurry translate (ballLoc g) $ color ballColor $ circleSolid ballRadius
      ballColor = dark red
      
      wall :: Float -> Picture
      wall offset =
        translate 0 offset $
          color wallColor $
            rectangleSolid screenW  wallH

      wallColor = greyN 0.5
      walls = pictures [wall (screenH /2 - wallH /2), wall (screenH /(-2) + wallH /2)]

      mkPaddle :: Color -> Float -> Float -> Picture
      mkPaddle col x y = pictures
        [ translate x y $ color col $ rectangleSolid paddleThick paddleLen
        , translate x y $ color paddleFrameColor $ rectangleSolid (paddleThick - 6) (paddleLen - 6)
        ]
      paddleFrameColor = light blue

      paddle1 = mkPaddle rose  (screenW / (-2) + paddleThick ) $ player1 g
      paddle2 = mkPaddle green (screenW / 2 - paddleThick ) $ player2 g



-- Calculate the ball position after given time (sec)
moveBall :: Float -> Game -> Game
moveBall sec game = game { ballLoc = (x1, y2) }
    where 
      --old ball location and velocity
      (x, y) = ballLoc game
      (vx, vy) = ballVel game

      --new ball location
      x1 = x + sec * vx
      y2 = y + sec * vy



-- Detect collision with wall (top or bottom)
wallCollision :: Position -> Bool
wallCollision (_, y) = topCollision || bottomCollision
    where
      topCollision = y > screenH/2 - wallH - ballRadius
      bottomCollision = y < screenH/(-2) + wallH + ballRadius

wallBounce :: Game -> Game
wallBounce game = game { ballVel = (vx, vy1) }
    where
      (vx, vy) = ballVel game
      (x, y) = ballLoc game
      vy1 = if wallCollision (x, y) then (-vy) else vy


-- Detect collision with paddle
paddleCollision :: Game -> Bool
paddleCollision game = leftCollision || rightCollision
    where
        (ballX, ballY) = ballLoc game
        paddleY1 = player1 game
        paddleY2 = player2 game
        leftCollision =   ballX < screenW / (-2) + 1.5 * paddleThick + ballRadius
                          && ballY < paddleY1 + paddleLen / 2
                          && ballY > paddleY1 - paddleLen / 2
        rightCollision =  ballX > screenW / 2 - 1.5 * paddleThick - ballRadius
                          && ballY < paddleY2 + paddleLen / 2
                          && ballY > paddleY2 - paddleLen / 2

paddleBounce :: Game -> Game
paddleBounce game = game { ballVel = (vx1, vy)}
    where 
        (vx, vy) = ballVel game
        vx1 = if paddleCollision game
            then -vx
            else vx


update :: Float -> Game -> Game
--update tm = paddleBounce . moveBall tm
update tm = paddleBounce . wallBounce . moveBall tm

