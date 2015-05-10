module Socket where

import System.IO.Error
import System.Directory
import Network.Socket

import Infinite

unixSocket :: FilePath -> IO Socket
unixSocket path = do
    catchIOError (removeFile path) (const $ return ())
    s <- socket AF_UNIX Stream defaultProtocol
    bind s (SockAddrUnix path)
    listen s maxListenQueue
    return s

awaitTouch :: Socket -> IO Infinite -> IO b
awaitTouch s action = do
    (_, _) <- accept s
    (Infinite newAction) <- action
    awaitTouch s newAction
