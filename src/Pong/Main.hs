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
import Control.Monad ((=<<))

data State = State
           { playerPaddle :: Double
           , aiPaddle :: Double
           , ball :: Vec2 Double
           , ballV :: Vec2 Double } deriving Show

data Update = BallPos (Vec2 Double)
            | BallVel (Vec2 Double)
            | PlayerPos Double
            | AIPos Double
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
    logMsg . show . ball =<< cgs
    return ()

advance :: State -> Vec2 Double
advance s = ball s + ballV s

bounceWall :: State -> Vec2 Double
bounceWall s
    | y < 0 || y > 100 = Vec2 vx (-vy)
    | otherwise        = Vec2 vx vy
    where Vec2 _ y = ball s
          Vec2 vx vy = ballV s

bouncePaddle :: State -> Vec2 Double
bouncePaddle s
    | x < 10 && abs (y - playerPaddle s) < 20 = bounced
    | x > 90 && abs (y - aiPaddle s) < 20     = bounced
    | otherwise                               = ballV s
    where Vec2 x y = ball s
          Vec2 vx vy = ballV s
          bounced = Vec2 (-vx) vy

initial :: State
initial = State 50 50 (Vec2 50 50) (Vec2 10 10)

config :: GameConfiguration State Update
config = GameConfiguration
       { networkStrategy      = fullSharingStrategy
       , initialState         = initial
       , gameLogic            = update
       , newConnectionHandler = NewConnectionHandler $ const $ const [] }

draw :: State -> IO ()
draw _ = return ()

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
