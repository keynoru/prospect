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
    CTime now <- epochTime
    mdate <- readProcess "date" ["--iso-8601"]
    bracket execDzen (\(p, _, _) -> eraseProcess p) $ \(_, hi, _) -> do
        hSetBuffering hi LineBuffering
        loop hi (either line line mdate) now
  where
    execDzen = rwProcess "dzen2" ["-p", "-fn", "Noto Mono-10", "-ta", "l"]

loop :: Handle -> ByteString -> Int64 -> IO a
loop h prefix now = do
    B.hPutBuilder h $ B.byteString prefix <> " " <> hms now <> "\n"
    threadDelay $ 1000 * 1000
    loop h prefix (now + 1)
