{-# LANGUAGE MultiParamTypeClasses, DeriveGeneric #-}
module Main where

import Flow.Net
import Flow.Game
import Flow.Common
import Flow.Linear
import Data.Serialize
import Data.Proxy
import GHC.Generics
import System.Environment
import System.Console.ANSI (clearScreen)
import System.Process (system)


data State = State
           { playerPaddle :: Int
           , aiPaddle :: Int
           , ball :: Vec2 Int
           , ballV :: Vec2 Int } deriving Show

data Update = BallPos (Vec2 Int)
            | BallVel (Vec2 Int)
            | PlayerPos Int
            | AIPos Int
            deriving (Generic, Show)

instance Serialize Update

instance Diff State Update where
    commit s (BallPos p) = s { ball = p }
    commit s (BallVel p) = s { ballV = p }
    commit s (PlayerPos p) = s { playerPaddle = p }
    commit s (AIPos p) = s { aiPaddle = p }

update :: Game State Update ()
update = do
    withCgs_ $ BallPos . advance
    withCgs_ $ BallVel . bounceWall
    withCgs_ $ BallVel . bouncePaddle
    return ()

advance :: State -> Vec2 Int
advance s = ball s + ballV s

bounceWall :: State -> Vec2 Int
bounceWall s
    | y <= 0 || y >= 9 = Vec2 vx (-vy)
    | otherwise        = Vec2 vx vy
    where Vec2 _ y = ball s
          Vec2 vx vy = ballV s

bouncePaddle :: State -> Vec2 Int
bouncePaddle s
    | x <= 1 && abs (y - playerPaddle s) < 2 = bounced
    | x >= 18 && abs (y - aiPaddle s) < 2    = bounced
    | otherwise                             = ballV s
    where Vec2 x y = ball s
          Vec2 vx vy = ballV s
          bounced = Vec2 (-vx) vy

initial :: State
initial = State 5 5 (Vec2 10 5) (Vec2 1 1)

config :: GameConfiguration State Update
config = GameConfiguration
       { networkStrategy      = fullSharingStrategy
       , initialState         = initial
       , gameLogic            = update
       , newConnectionHandler = NewConnectionHandler $ const $ const [] }

draw :: State -> IO ()
draw st = do
    system "cls"
    putStrLn $ unlines $ map (\y -> map (\x -> drawPx x y) [0..19]) [0..9]
    where drawPx x y = if full x y then '#' else ' '
          full x y = ball st == Vec2 x y
                  || x == 0 && abs (y - playerPaddle st) < 2
                  || x == 19 && abs (y - aiPaddle st) < 2

startServer, startClient :: IO ()
startServer = serveGame config
startClient = connectGame initial (Proxy :: Proxy Update) draw

main :: IO ()
main = do
    [arg] <- getArgs
    case arg of
        "server" -> startServer
        "client" -> startClient
        _        -> error "Wrong param"
