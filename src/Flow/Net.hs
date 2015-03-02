{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Flow.Net where

import Flow.Common
import Flow.Game
import Flow.Lifted
import Network.Simple.TCP (connect)
import Network.Socket (withSocketsDo, iNADDR_ANY)
import Data.Serialize
import Data.Proxy
import qualified Data.ByteString as ByteString
import Control.Monad.Reader.Class
import Control.Monad.Reader
import Prelude hiding (putStrLn)

data InternalState s = InternalState
                     { currentState :: s
                     , connections  :: [(Socket, ClientID)]
                     , lastClientID :: ClientID }

fps, ft :: Integer
fps = 5
ft = 1000000 `div` fps

newtype Net s u e a = Net (ReaderT (LogicConfiguration s u e, IORef (InternalState s)) IO a)
                      deriving
                    ( Monad
                    , MonadReader (LogicConfiguration s u e, IORef (InternalState s))
                    , MonadIO, Functor, Applicative )

instance Forkable (Net s u e) where
    forkIO n = do
        (gc, stRef) <- ask
        liftIO $ forkIO $ runNet gc stRef n

runNet :: LogicConfiguration s u e -> IORef (InternalState s) -> Net s u e a -> IO a
runNet gc stRef (Net r) = runReaderT r (gc, stRef)

serveGame :: (Diff s u, Serialize e, Serialize u) => LogicConfiguration s u e -> IO ()
serveGame gc = withSocketsDo $ do
    stRef <- newIORef $ InternalState (initialState gc) [] 0
    runNet gc stRef logicLoop

logicLoop :: (Diff s u, Serialize e, Serialize u) => Net s u e a
logicLoop = do
    (gc, _) <- ask
    initServerSocket
    let logic = gameLogic gc
    forever $ do --Game loop
        time <- getCPUTime
        advance logic
        time' <- getCPUTime
        let diffUs = (time' - time) `div` (10^(3 :: Integer))
        threadDelay (max 0 $ fromIntegral $ ft - diffUs)

initServerSocket :: (Diff s u, Serialize e, Serialize u) => Net s u e ()
initServerSocket = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 8442 iNADDR_ANY)
    listen sock 100
    void $ forkIO $ handleConnection sock

handleConnection :: (Diff s u, Serialize e, Serialize u) => Socket -> Net s u e ()
handleConnection serverSock = do
    (gc, stRef) <- ask
    (sock, addr) <- accept serverSock
    putStrLn $ "Got connection on " ++ show sock ++ " " ++ show addr
    st <- readIORef stRef
    initiate (newConnectionHandler gc) (currentState st) (lastClientID st) sock
    writeIORef stRef $ st { connections = (sock, lastClientID st) : connections st
                          , lastClientID = lastClientID st + 1 }
    processEvents (lastClientID st) sock
    void $ forkIO $ handleConnection serverSock

processEvents :: forall s u e. (Diff s u, Serialize e, Serialize u) => ClientID -> Socket -> Net s u e ()
processEvents cID sock = do
    (gc, _) <- ask
    let handler = eventHandler gc
    forever $ do
        threadDelay $ fromIntegral ft
        bs <- recv sock 4096
        unless (ByteString.null bs) $ case runGet (getAll get :: Get [e]) bs of
            Left err -> putStrLn err
            Right es ->
                 let game = foldl1 (>>) $ map (handler cID) es
                 in  advance game

advance :: (Diff s u, Serialize u) => Game s u () -> Net s u e ()
advance game = do
    (gc, stRef) <- ask
    st <- readIORef stRef
    let strat = networkStrategy gc
        gameState = currentState st
        (gameState', Report updates logs, _) = runGame game gameState
        clients = connections st
    writeIORef stRef $ st { currentState = gameState' }
    mapM_ putStrLn logs
    mapM_ (tellClient strat gameState' updates) clients

initiate :: Serialize u => NewConnectionHandler s u -> s -> ClientID -> Socket -> Net s u e ()
initiate (NewConnectionHandler fn) st cID sock = mapM_ (sendUpdate sock) updates
    where updates = fn cID st

tellClient :: Serialize u => NetworkStrategy s u -> s -> [u] -> (Socket, ClientID) -> Net s u e ()
tellClient (NetworkStrategy p) state us (sock, cID) =
    mapM_ (sendUpdate sock) $ filter (\u -> p state u cID) us

sendUpdate :: Serialize u => Socket -> u -> Net s u e ()
sendUpdate sock u = sendAll sock $ encode u

connectGame :: (Diff s u, Serialize u) => s -> Proxy u -> (s -> IO ()) -> IO ()
connectGame initState p draw =
    withSocketsDo $ connect "127.0.0.1" "8442" (runClient initState p draw)

runClient :: forall s u. (Diff s u, Serialize u) => s -> Proxy u -> (s -> IO ()) -> (Socket, SockAddr) -> IO ()
runClient state p draw (sock, addr) = do
    threadDelay (10^(3 :: Integer))
    bs <- recv sock 4096
    when (ByteString.null bs) $ runClient state p draw (sock, addr)
    case runGet (getAll get :: Get [u]) bs of
        Left err -> putStrLn err
        Right us -> do
            let state' = apply state us
            draw state'
            runClient state' p draw (sock, addr)

getAll :: Get a -> Get [a]
getAll g = do
    a <- g
    r <- remaining
    if r == 0 then return [a]
    else (a :) <$> getAll g
