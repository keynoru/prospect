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
    = MsgClockTick
    | MsgClockSet Int64
    | MsgDate ByteString
    | MsgCal ByteString
    | MsgBattery ByteString

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

-- producers

dayChangeProducer :: TChan Message -> IO a
dayChangeProducer chan = forever $ do
    CTime now <- epochTime
    mdate <- readProcess "date" ["--iso-8601"]
    mcal <- readProcess "sh"
        [ "-c"
        , "cal | grep \"$(date +%e) \" | "
        <> "sed -e \"s/\\($(date +%e)\\)/[\\1]/\""
        ]
    atomically $ do
        writeTChan chan (MsgClockSet now)
        writeTChan chan (MsgDate $ either line line mdate)
        writeTChan chan (MsgCal $ either line line mcal)
    threadDelay $ (1000 * 1000 *) $
        86400 - ((fromIntegral now + 9 * 3600) `mod` 86400)

tickProducer :: TChan Message -> IO a
tickProducer chan = forever $ do
    threadDelay 999100
    atomically $ writeTChan chan MsgClockTick

batteryProducer :: TChan Message -> IO a
batteryProducer chan = forever $ do
    mbat <- readProcess "acpi" []
    atomically $ writeTChan chan (MsgBattery $ either line line mbat)
    threadDelay $ 60 * 1000 * 1000

adjustmentProducer :: TChan Message -> IO a
adjustmentProducer chan = forever $ do
    threadDelay $ 60 * 1000 * 1000
    CTime now <- epochTime
    atomically $ writeTChan chan (MsgClockSet now)

-- consumers

consumer
    :: Handle -> TChan Message
    -> Int64 -> ByteString -> Int64 -> ByteString -> ByteString
    -> IO a
consumer h chan up date now cal battery = do
    msg <- atomically $ readTChan chan
    let
        (ndate, nnow, ncal, nbattery) = case msg of
            MsgClockTick ->  (date, now + 1, cal, battery)
            MsgClockSet x -> (date, x, cal, battery)
            MsgDate b -> (b, now, cal, battery)
            MsgCal b -> (date, now, b, battery)
            MsgBattery b -> (date, now, cal, b)
    B.hPutBuilder h $ mconcat
        [ " "
        , B.byteString ndate
        , " "
        , hms now
        , " | uptime "
        , hms (now - up)
        , " | "
        , B.byteString ncal
        , " | "
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
        void $ forkIO $ dayChangeProducer chan
        void $ forkIO $ batteryProducer chan
        void $ forkIO $ tickProducer chan
        void $ forkIO $ adjustmentProducer chan
        consumer hi chan up "" 0 "" ""
  where
    dzen args = rwProcess "dzen2" $
        [ "-p", "-y", "-1"
        , "-fg", "#ffffff", "-bg", "#004999"
        ] ++ args
    getUpSince = readProcess "sh"
        ["-c", "uptime -s | xargs -0 date +%s --date"]
    cleanup (p, hi, ho) = do
        hClose hi
        hClose ho
        eraseProcess p
