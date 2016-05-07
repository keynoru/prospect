module Lib where

import Data.Int
import Data.Monoid
import Control.Exception

import System.IO (Handle, hSetBuffering, BufferMode(..))
import Control.Concurrent
import System.Posix.ByteString
import Foreign.C.Types (CTime(..))

import RawFilePath
import ByteString (ByteString, Builder)
import qualified ByteString as B

hms :: Int64 -> Builder
hms x = mconcat [d ((h + 9) `mod` 24), ":", d m, ":", d s]
  where
    d num = B.byteString (if B.length b == 1 then "0" <> b else b)
      where
        b = B.toByteString (B.int64Dec num)
    (h, remainder) = (x `mod` 86400) `divMod` 3600
    (m, s) = remainder `divMod` 60

line :: ByteString -> ByteString
line = B.takeWhile (/= 10)

app :: IO ()
app = do
    args <- getArgs
    CTime now <- epochTime
    mdate <- readProcess "date" ["--iso-8601"]
    mcal <- readProcess "sh"
        [ "-c"
        , "cal | grep \" $(date +%e) \" | "
        <> "sed -e \"s/\\($(date +%e)\\)/[\\1]/\""
        ]
    bracket (dzen args) (\(p, _, _) -> eraseProcess p) $ \(_, hi, _) -> do
        hSetBuffering hi LineBuffering
        putter hi (either line line mdate) (either line line mcal) now
  where
    dzen args = rwProcess "dzen2" $
        [ "-p", "-y", "-1"
        , "-fg", "#ffffff", "-bg", "#004999"
        ] ++ args

-- putter puts one line per second, with fixed prefixes and suffixes.
putter :: Handle -> ByteString -> ByteString -> Int64 -> IO a
putter h prefix suffix now = do
    B.hPutBuilder h $ mconcat
        [ " "
        , B.byteString prefix
        , " "
        , hms now
        , " | "
        , B.byteString suffix
        , "\n"
        ]
    threadDelay $ 1000 * 1000
    putter h prefix suffix (now + 1)
