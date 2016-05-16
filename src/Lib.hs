module Lib (app) where

import Data.Int
import Data.Monoid
import Control.Monad
import Control.Exception

import System.IO (Handle, hClose, hSetBuffering, BufferMode(..))
import Control.Concurrent
import Control.Concurrent.STM

import System.Posix.ByteString
import Foreign.C.Types (CTime(..))
import System.Exit

import RawFilePath
import ByteString (ByteString, Builder)
import qualified ByteString as B
import qualified Data.Attoparsec.ByteString.Char8 as A

data Message
    = MsgClockSet Int64
    | MsgDate ByteString
    | MsgCal ByteString
    | MsgBattery ByteString
    deriving Show

timeOfDay :: Int64 -> Builder
timeOfDay x = mconcat [d ((h + 9) `mod` 24), ":", d m]
  where
    d num = B.byteString (if B.length b == 1 then "0" <> b else b)
      where
        b = B.toByteString (B.int64Dec num)
    (h, remainder) = (x `mod` 86400) `divMod` 3600
    m = remainder `div` 60

line :: ByteString -> ByteString
line = B.takeWhile (/= 10)

-- producers

dayProducer :: TChan Message -> IO a
dayProducer chan = forever $ do
    CTime now <- epochTime
    mdate <- readProcess "date" ["+%Y-%m-%d (%a)"]
    mcal <- readProcess "sh"
        [ "-c"
        , "cal | awk 'NR > 1' | grep \"$(date +%e) \" | "
        <> "sed -e \"s/\\($(date +%e)\\)/^bg(white)^fg(#2AA198) \\1 "
        <> "^bg(#2AA198)^fg(white)/\""
        ]
    atomically $ do
        writeTChan chan (MsgDate $ either line line mdate)
        writeTChan chan (MsgCal $ either line line mcal)
    threadDelay $ (1000 * 1000 *) $
        86400 - ((fromIntegral now + 9 * 3600) `mod` 86400)

minuteProducer :: TChan Message -> IO a
minuteProducer chan = forever $ do
    mbat <- readProcess "acpi" []
    CTime now <- epochTime
    atomically $ do
        writeTChan chan (MsgBattery $ either line line mbat)
        writeTChan chan (MsgClockSet now)
    threadDelay $ (1000 * 1000 *) $
        60 - (fromIntegral now `mod` 60)

-- consumers

consumer
    :: Handle -> TChan Message
    -> Int64 -> ByteString -> Int64 -> ByteString -> ByteString
    -> IO a
consumer h chan up date now cal battery = do
    msg <- atomically $ readTChan chan
    let
        (ndate, nnow, ncal, nbattery) = case msg of
            MsgClockSet x -> (date, x, cal, battery)
            MsgDate b -> (b, now, cal, battery)
            MsgCal b -> (date, now, b, battery)
            MsgBattery b -> (date, now, cal, b)
    B.hPutBuilder h $ mconcat
        [ "^bg(#268BD2)^fg(#FDF6E3) system ^bg(#FFCC00)^fg(#268BD2)\57520^fg(black) "
        , B.byteString ndate
        , " "
        , timeOfDay nnow
        , " ^bg(#073642)^fg(#FFCC00)\57520^fg(white) uptime "
        , timeOfDay (nnow - up)
        , " ^bg(#2AA198)^fg(#073642)\57520^fg(white) "
        , B.byteString ncal
        , "^bg(#FFCC00)^fg(#2AA198)\57520^fg(black) "
        , B.byteString nbattery
        , "\n"
        ]
    consumer h chan up ndate nnow ncal nbattery

-- application

app :: IO ()
app = do
    args <- getArgs
    chan <- atomically newTChan
    up <- getUpSince >>= \case
        Left err -> B.putStr (err <> "Failed getting uptime") *> exitFailure
        Right x -> case A.parseOnly A.decimal (line x) of
            Left err -> do
                putStrLn err
                B.putBuilder $ "Failed parsing " <> B.byteString x
                exitFailure
            Right n -> return (n + 32400)
    bracket (dzen args) cleanup $ \(_, hi, _) -> do
        hSetBuffering hi LineBuffering
        void $ forkIO $ dayProducer chan
        void $ forkIO $ minuteProducer chan
        consumer hi chan up "" 0 "" ""
  where
    dzen args = rwProcess "dzen2" $
        [ "-p", "-y", "-1", "-ta", "l"
        , "-fg", "#ffffff", "-bg", "#004999"
        ] ++ args
    getUpSince = readProcess "sh"
        ["-c", "uptime -s | xargs -0 date +%s --date"]
    cleanup (p, hi, ho) = do
        hClose hi
        hClose ho
        eraseProcess p
