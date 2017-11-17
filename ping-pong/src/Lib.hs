module Lib
    ( initialState
    , render
    , window
    , background
    , moveBall
    , movePaddles
    , moveThings
    , update
    , Game(..)
    , Movement(..)
    ) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

type Position = (Float, Float)

data Movement = MvUp | MvDown | Stop deriving Eq

data Game = Game { ballLoc :: (Float, Float)
                 , ballVel :: (Float, Float)
                 , player1 :: (Float, Movement)
                 , player2 :: (Float, Movement)
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

paddleVelocity::Float
paddleVelocity = 200

ballRadius :: Float
ballRadius = 10


-- Initial game state definition
initialState::Game
initialState = Game (0, 0) (300, 0) (0, Stop) (0, Stop) 0 0 True



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

      paddle1 = mkPaddle rose  (screenW / (-2) + paddleThick ) $ fst $ player1 game
      paddle2 = mkPaddle green (screenW / 2 - paddleThick ) $ fst $ player2 game

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

movePaddles :: Float -> Game -> Game
movePaddles tm game = game { player1 = pl1, player2 = pl2}
      where
        pl1 = movePaddle tm ( player1 game )
        pl2 = movePaddle tm ( player2 game )

        movePaddle :: Float -> (Float, Movement) -> (Float, Movement)
        movePaddle tm (y, dir)  
          | dir == MvUp = (y + tm * paddleVelocity, dir)
          | dir == MvDown = (y - tm * paddleVelocity, dir)
          | otherwise = (y, dir)




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
        paddleY1 = fst $ player1 game
        paddleY2 = fst $ player2 game
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
        (x, y)   = ballLoc game
        (hit, paddleY) = paddleCollision game
        vel = if hit
            then (-vx, archvy )
            else (vx, vy)
        archvy = abs vx * tan (normalAngle + archedAngle)
        normalAngle = atan (vy / abs vx)
        archedAngle = (y - paddleY) / (paddleLen * 0.7)

moveThings :: Float -> Game -> Game
moveThings tm = moveBall tm . movePaddles tm

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
update tm =  detectDrop . paddleBounce . wallBounce . moveThings tm

