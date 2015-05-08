module Socket where

import System.IO.Error
import System.Directory
import Network.Socket

unixSocket :: FilePath -> IO Socket
unixSocket path = do
    catchIOError (removeFile path) (const $ return ())
    s <- socket AF_UNIX Stream defaultProtocol
    bind s (SockAddrUnix path)
    listen s maxListenQueue
    return s

awaitTouch :: Socket -> IO a -> IO b
awaitTouch s action = do
    (_, _) <- accept s
    _ <- action
    awaitTouch s action
