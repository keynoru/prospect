module Main where

import Network.Socket

main :: IO ()
main = do
    s <- socket AF_UNIX Stream defaultProtocol
    connect s (SockAddrUnix "/tmp/prospect.socket")
