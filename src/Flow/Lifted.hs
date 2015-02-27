module Flow.Lifted
     ( Sock.Socket(..)
     , IORef.IORef
     , Sock.Family(..)
     , Sock.SocketType(..)
     , Sock.ProtocolNumber
     , Sock.SocketOption(..)
     , Sock.SockAddr(..)
     , BS.ByteString
     , Forkable
     , forkIO
     , threadDelay
     , getCPUTime
     , socket
     , setSocketOption
     , bindSocket
     , listen
     , accept
     , Flow.Lifted.putStrLn
     , newIORef
     , readIORef
     , writeIORef
     , sendAll
     , recv ) where

import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as SockBS
import qualified Data.IORef as IORef
import qualified Control.Concurrent as Conc
import qualified System.CPUTime as CPU
import qualified Data.ByteString as BS
import Control.Monad.Trans

threadDelay :: MonadIO m => Int -> m ()
threadDelay n = liftIO $ Conc.threadDelay n

getCPUTime :: MonadIO m => m Integer
getCPUTime = liftIO CPU.getCPUTime

class Monad m => Forkable m where
    forkIO :: m () -> m Conc.ThreadId

instance Forkable IO where
    forkIO = Conc.forkIO

socket :: MonadIO m => Sock.Family -> Sock.SocketType -> Sock.ProtocolNumber -> m Sock.Socket
socket f t n = liftIO $ Sock.socket f t n

setSocketOption :: MonadIO m => Sock.Socket -> Sock.SocketOption -> Int -> m ()
setSocketOption s o n = liftIO $ Sock.setSocketOption s o n

bindSocket :: MonadIO m => Sock.Socket -> Sock.SockAddr -> m ()
bindSocket s a = liftIO $ Sock.bindSocket s a

listen :: MonadIO m => Sock.Socket -> Int -> m ()
listen s n = liftIO $ Sock.listen s n

accept :: MonadIO m => Sock.Socket -> m (Sock.Socket, Sock.SockAddr)
accept s = liftIO $ Sock.accept s

putStrLn :: MonadIO m => String -> m ()
putStrLn s = liftIO $ Prelude.putStrLn s

newIORef :: MonadIO m => a -> m (IORef.IORef a)
newIORef a = liftIO $ IORef.newIORef a

readIORef :: MonadIO m => IORef.IORef a -> m a
readIORef r = liftIO $ IORef.readIORef r

writeIORef :: MonadIO m => IORef.IORef a -> a -> m ()
writeIORef r a = liftIO $ IORef.writeIORef r a

sendAll :: MonadIO m => Sock.Socket -> BS.ByteString -> m ()
sendAll s bs = liftIO $ SockBS.sendAll s bs

recv :: MonadIO m => Sock.Socket -> Int -> m BS.ByteString
recv s n = liftIO $ SockBS.recv s n
