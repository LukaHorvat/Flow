{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}
module Flow.Game where

import Data.Serialize.Put
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity
import Flow.Common

newtype ClientID = ClientID Int deriving (Show, Num)

-- | Determines which update packets get sent to which client.
data NetworkStrategy s u = NetworkStrategy (s -> u -> ClientID -> Bool)

data NewConnectionHandler s u = NewConnectionHandler (ClientID -> s -> [u])


-- | Game configuration.
data GameConfiguration s u = GameConfiguration
                           { networkStrategy :: NetworkStrategy s u
                           , initialState :: s
                           , gameLogic :: Game s u ()
                           , newConnectionHandler :: NewConnectionHandler s u }

-- | Sends all updates to every client
fullSharingStrategy :: Diff s u => NetworkStrategy s u
fullSharingStrategy = NetworkStrategy $ \_ _ _ -> True

-- | Summary of what happened in one logic step
data Report u = Report [u] [String]

instance Monoid (Report u) where
    mempty = Report [] []
    Report us ss `mappend` Report us' ss' = Report (us ++ us') (ss ++ ss')

-- | The Game monad lets you inspect the current game state and post changes to it which get recorded.
newtype Game s u a = Game (WriterT (Report u) (StateT s Identity) a)
                     deriving (Monad, MonadState s, MonadWriter (Report u), Functor, Applicative)

runGame :: Diff s u => Game s u a -> s -> (s, Report u, a)
runGame (Game g) s = let ((a, us), s') = runIdentity (runStateT (runWriterT g) s) in (s', us, a)

doUpdate :: Diff s u => u -> Game s u s
doUpdate u = do
    tell $ Report [u] []
    modify (`commit` u)
    get

doUpdate_ :: Diff s u => u -> Game s u ()
doUpdate_ = void . doUpdate

logMsg :: String -> Game s u ()
logMsg msg = tell $ Report [] [msg]

currentGameState, cgs :: Game s u s
currentGameState = get
cgs = currentGameState

withCgs :: Diff s u => (s -> u) -> Game s u s
withCgs f = cgs >>= doUpdate . f

withCgs_ :: Diff s u => (s -> u) -> Game s u ()
withCgs_ = void . withCgs
