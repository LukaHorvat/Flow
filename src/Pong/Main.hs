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
import System.Process (system)

type NumType = Int
type Vec = Vec2 NumType
width, height :: Int
width = 40
height = 20

data State = State
           { playerPaddle :: NumType
           , aiPaddle :: NumType
           , ball :: Vec
           , ballV :: Vec } deriving Show

data Update = BallPos (Vec)
            | BallVel (Vec)
            | PlayerPos NumType
            | AIPos NumType
            deriving (Generic, Show)

data Command = Up | Down deriving (Generic, Show)
instance Serialize Update
instance Serialize Command

instance Diff State Update where
    commit s (BallPos p) = s { ball = p }
    commit s (BallVel p) = s { ballV = p }
    commit s (PlayerPos p) = s { playerPaddle = p }
    commit s (AIPos p) = s { aiPaddle = p }

update :: Game State Update ()
update = do
    withCgs_ $ BallPos . Main.advance
    withCgs_ $ BallVel . bounceWall
    withCgs_ $ BallVel . bouncePaddle
    return ()

advance :: State -> Vec
advance s = ball s + ballV s

bounceWall :: State -> Vec
bounceWall s
    | y <= 0 || y >= height - 1 = Vec2 vx (-vy)
    | otherwise        = Vec2 vx vy
    where Vec2 _ y = ball s
          Vec2 vx vy = ballV s

bouncePaddle :: State -> Vec
bouncePaddle s
    | x <= 1 && abs (y - playerPaddle s) < 2 = bounced
    | x >= width - 2 && abs (y - aiPaddle s) < 2    = bounced
    | otherwise                             = ballV s
    where Vec2 x y = ball s
          Vec2 vx vy = ballV s
          bounced = Vec2 (-vx) vy

initial :: State
initial = State 5 5 (Vec2 10 5) (Vec2 1 1)

config :: LogicConfiguration State Update Command
config = LogicConfiguration
       { networkStrategy      = fullSharingStrategy
       , initialState         = initial
       , gameLogic            = update
       , newConnectionHandler = NewConnectionHandler $ const $ const [] }

draw :: State -> IO ()
draw st = do
    system "cls"
    putStrLn $ unlines $ map (\y -> map (\x -> drawPx x y) [0..width - 1]) [0..height - 1]
    where drawPx x y | full x y  = '#'
                     | otherwise = ' '
          full x y = ball st == Vec2 x y
                  || x == 0 && abs (y - playerPaddle st) < 2
                  || x == width - 1 && abs (y - aiPaddle st) < 2

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
