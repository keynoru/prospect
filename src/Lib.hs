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

timeGap :: Int64 -> Builder
timeGap x
    | d /= 0 = mconcat [day, hour, minute]
    | h /= 0 = mconcat [hour, minute]
    | otherwise = minute
  where
    (d, daySeconds) = x `divMod` 86400
    (h, hourSeconds) = daySeconds `divMod` 3600
    m = hourSeconds `div` 60
    day = B.int64Dec d <> "d "
    hour = B.int64Dec h <> "h "
    minute = B.int64Dec m <> "m"

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
        <> "sed -e \"s/\\($(date +%e)\\)/^bg(black)^fg(#FFCC00) \\1 "
        <> "^bg(#FFCC00)^fg(black)/\""
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
        [ " 💻 system \57521 📅 "
        , B.byteString ndate
        , " ⌚ "
        , timeOfDay nnow
        , " \57521 ⌛ up "
        , timeGap (nnow - up)
        , " \57521 "
        , B.byteString ncal
        , "\57521 ⚡️"
        , B.byteString (bat nbattery)
        , "\n"
        ]
    consumer h chan up ndate nnow ncal nbattery
  where
    bat b = if "Battery 0: " `B.isPrefixOf` b then B.drop 11 b else b

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
            Right n -> return n
    bracket (dzen args) cleanup $ \(_, hi, _) -> do
        hSetBuffering hi LineBuffering
        void $ forkIO $ dayProducer chan
        void $ forkIO $ minuteProducer chan
        consumer hi chan up "" 0 "" ""
  where
    dzen args = rwProcess "dzen2" $
        [ "-p", "-y", "-1", "-ta", "l"
        , "-fg", "black", "-bg", "#ffcc00"
        ] ++ args
    getUpSince = readProcess "sh"
        ["-c", "uptime -s | xargs -0 date +%s --date"]
    cleanup (p, hi, ho) = do
        hClose hi
        hClose ho
        eraseProcess p
