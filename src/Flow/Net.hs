{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, TemplateHaskell #-}
module Flow.Net where

import Flow.Common
import Flow.Game
import Network.Simple.TCP (connect)
import Network.Socket hiding (recv, connect)
import Network.Socket.ByteString
import Data.Serialize
import Data.IORef
import Data.Proxy
import Control.Concurrent
import System.IO (IOMode)
import System.CPUTime
import Lens.Family2
import Lens.Family2.TH
import qualified Data.ByteString as ByteString

data InternalState s = InternalState
                     { _currentState :: s
                     , _connections  :: [(Socket, ClientID)]
                     , _lastClientID :: ClientID }
$(makeLenses ''InternalState)

fps, ft :: Integer
fps = 1
ft = 1000000 `div` fps

serveGame :: (Diff s u, Serialize u) => GameConfiguration s u -> IO ()
serveGame gc = withSocketsDo $ do
    stRef <- newIORef $ InternalState (initialState gc) [] 0
    -- create socket
    sock <- socket AF_INET Stream 0
    -- make socket immediately reusable - eases debugging.
    setSocketOption sock ReuseAddr 1
    -- listen on TCP port 4242
    bindSocket sock (SockAddrInet 8442 iNADDR_ANY)
    -- allow a maximum of 100 outstanding connections
    listen sock 100
    forkIO (handleConnection gc stRef sock)
    let logic = gameLogic gc
        strat = networkStrategy gc
    forever $ do
        time <- getCPUTime
        st <- readIORef stRef
        let gameState                            = _currentState st
            (gameState', Report updates logs, _) = runGame logic gameState
            clients                              = _connections st
        writeIORef stRef $ st { _currentState = gameState' }
        mapM_ putStrLn logs
        mapM_ (tellClient strat gameState' updates) clients
        time' <- getCPUTime
        let diffUs = (time' - time) `div` (10^(3 :: Integer))
        threadDelay (max 0 $ fromIntegral $ ft - diffUs)

tellClient :: Serialize u => NetworkStrategy s u -> s -> [u] -> (Socket, ClientID) -> IO ()
tellClient (NetworkStrategy p) state us (sock, cID) =
    mapM_ (sendUpdate sock) $ filter (\u -> p state u cID) us

handleConnection :: (Diff s u, Serialize u) => GameConfiguration s u -> IORef (InternalState s) -> Socket -> IO ()
handleConnection gc stRef serverSock = do
    (sock, addr) <- accept serverSock
    putStrLn $ "Got connection on " ++ show sock ++ " " ++ show addr
    st <- readIORef stRef
    initiate gc (_currentState st) (_lastClientID st) sock
    writeIORef stRef $ st { _connections = (sock, _lastClientID st) : _connections st
                          , _lastClientID = _lastClientID st + 1 }
    handleConnection gc stRef serverSock


initiate :: Serialize u => GameConfiguration s u -> s -> ClientID -> Socket -> IO ()
initiate gc st cID sock = mapM_ (sendUpdate sock) updates
    where updates = fn cID st
          NewConnectionHandler fn = newConnectionHandler gc

sendUpdate :: Serialize u => Socket -> u -> IO ()
sendUpdate socket u = do
    let d = encode u
    sendAll socket d

connectGame :: (Diff s u, Serialize u) => s -> Proxy u -> (s -> IO ()) -> IO ()
connectGame initialState p draw =
    withSocketsDo $ connect "127.0.0.1" "8442" (runClient initialState p draw)

runClient :: forall s u. (Diff s u, Serialize u) => s -> Proxy u -> (s -> IO ()) -> (Socket, SockAddr) -> IO ()
runClient state p draw (sock, addr) = forever $ do
    threadDelay (10^(3 :: Integer))
    bs <- recv sock 4096
    when (ByteString.null bs) $ runClient state p draw (sock, addr)
    case runGet (get :: Get u) bs of
        Left err -> putStrLn err
        Right (us :: u) -> do
            let state' = commit state us
            draw state'
            runClient state' p draw (sock, addr)
