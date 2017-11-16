module Lib
    ( initialState
    , render
    , window
    , background
    , moveBall
    , update
    , moveLeftPaddle
    , moveRightPaddle
    , Game(..)
    , paddleStep
    ) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

type Position = (Float, Float)

data Game = Game { ballLoc :: (Float, Float)
                 , ballVel :: (Float, Float)
                 , player1 :: Float
                 , player2 :: Float
                 , score1 :: Int
                 , score2 :: Int
                 , suspended :: Bool
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

paddleStep :: Float
paddleStep = 10

-- Initial game state definition
initialState::Game
initialState = Game (0, 0) (200, 100) 0 0 0 0 True


-- Main window
window :: Display
window = InWindow "Pingy Pong" (round screenW, round screenH) (100, 100)


-- Render single game frame
render :: Game -> Picture
render game = pictures [ ball
                       , walls
                       , paddle1
                       , paddle2
                       , scoreboard
                       ]
    where 
      ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid ballRadius
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

      paddle1 = mkPaddle rose  (screenW / (-2) + paddleThick ) $ player1 game
      paddle2 = mkPaddle green (screenW / 2 - paddleThick ) $ player2 game

      scoreboard = translate 0 (screenH/2 - 60) $ scale 0.3 0.3 $ color scorecolour $ text scoretext
      scoretext = if suspended game then
                     (show $ score1 game) ++ " : " ++ (show $ score2 game)
                  else ""
      scorecolour = red



-- Calculate the ball position after given time (sec)
moveBall :: Float -> Game -> Game
moveBall sec game = if suspended game then game
                    else
                        game { ballLoc = (x1, y2) }
                        where 
                          --old ball location and velocity
                          (x, y) = ballLoc game
                          (vx, vy) = ballVel game

                          --new ball location
                          x1 = x + sec * vx
                          y2 = y + sec * vy


moveRightPaddle :: Float -> Game -> Game
moveRightPaddle step game = game { player2 = calcPaddleMove (player2 game) step }


moveLeftPaddle :: Float -> Game -> Game
moveLeftPaddle step game = game { player1 = calcPaddleMove (player1 game) step }


calcPaddleMove :: Float -> Float -> Float
calcPaddleMove oldY step = 
               if oldY + step < screenH / 2 - paddleLen / 2 && oldY + step > screenH / (-2) + paddleLen / 2
               then oldY + step
               else oldY


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
paddleCollision :: Game -> (Bool, Float)
paddleCollision game = (leftCollision || rightCollision, hitpoint)
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
        hitpoint = if leftCollision then paddleY1 else paddleY2

paddleBounce :: Game -> Game
paddleBounce game = game { ballVel = vel}
    where 
        (vx, vy) = ballVel game
        (hit, paddleY) = paddleCollision game
        vel = if hit
            then (-vx, vy + arch)
            else (vx, vy)
        arch = 0

detectDrop :: Game -> Game
detectDrop game = if x > screenW / 2 - ballRadius && not susp
                  then game { score1 = 1 + score1 game , suspended = True }
                  else if x < screenW / (-2) + ballRadius && not susp
                    then game { score2 = 1 + score2 game , suspended = True }
                  else game
                  where 
                    susp = suspended game
                    (x, _) = ballLoc game

update :: Float -> Game -> Game
update tm = detectDrop . paddleBounce . wallBounce . moveBall tm

