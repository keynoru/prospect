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

import RawFilePath
import ByteString (ByteString, Builder)
import qualified ByteString as B

data Message
    = MsgClockTick
    | MsgClockSet Int64
    | MsgDate ByteString
    | MsgCal ByteString
    | MsgBattery ByteString
    | MsgDebugClockDiff Int64 Int64

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
    threadDelay $ 1000 * 1000
    atomically $ writeTChan chan MsgClockTick

batteryProducer :: TChan Message -> IO a
batteryProducer chan = forever $ do
    mbat <- readProcess "acpi" []
    atomically $ writeTChan chan (MsgBattery $ either line line mbat)
    threadDelay $ 60 * 1000 * 1000

debugClockProducer :: TChan Message -> Int64 -> IO a
debugClockProducer chan origin = forever $ do
    threadDelay $ 3 * 1000 * 1000
    CTime now <- epochTime
    atomically $ writeTChan chan (MsgDebugClockDiff origin now)
    threadDelay $ 57 * 1000 * 1000

-- consumers

consumer
    :: Handle -> TChan Message
    -> ByteString -> Int64 -> ByteString -> ByteString
    -> IO a
consumer h chan date now cal battery = do
    msg <- atomically $ readTChan chan
    let
        (ndate, nnow, ncal, nbattery) = case msg of
            MsgClockTick ->  (date, now + 1, cal, battery)
            MsgClockSet x -> (date, x, cal, battery)
            MsgDate b -> (b, now, cal, battery)
            MsgCal b -> (date, now, b, battery)
            MsgBattery b -> (date, now, cal, b)
            MsgDebugClockDiff _ _ -> (date, now, cal, battery)
    case msg of
        MsgDebugClockDiff origin x -> B.putBuilder $ mconcat
            [ B.int64Dec (nnow - origin)
            , " seconds since the beginning. Internal clock is "
            , B.int64Dec $ x - nnow
            , " seconds behind.\n"
            ]
        _ -> return ()
    B.hPutBuilder h $ mconcat
        [ " "
        , B.byteString ndate
        , " "
        , hms now
        , " | "
        , B.byteString ncal
        , " | "
        , B.byteString nbattery
        , "\n"
        ]
    consumer h chan ndate nnow ncal nbattery

-- application

app :: IO ()
app = do
    args <- getArgs
    chan <- atomically newTChan
    CTime now <- epochTime
    bracket (dzen args) cleanup $ \(_, hi, _) -> do
        hSetBuffering hi LineBuffering
        void $ forkIO $ dayChangeProducer chan
        void $ forkIO $ batteryProducer chan
        void $ forkIO $ tickProducer chan
        void $ forkIO $ debugClockProducer chan now
        consumer hi chan "" 0 "" ""
  where
    dzen args = rwProcess "dzen2" $
        [ "-p", "-y", "-1"
        , "-fg", "#ffffff", "-bg", "#004999"
        ] ++ args
    cleanup (p, hi, ho) = do
        hClose hi
        hClose ho
        eraseProcess p
