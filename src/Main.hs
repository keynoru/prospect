{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import Data.Word

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Control.Concurrent
import System.Process
import System.IO

import Data.Hourglass
import System.Hourglass

import Battery
import qualified Parser as I
import Infinite
import Socket

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    sock <- unixSocket "/tmp/prospect.socket"
    _ <- forkIO $ awaitTouch sock toggleReport

    taskQueue <- newChan
    writeChan taskQueue ()
    worker taskQueue

toggleReport :: IO Infinite
toggleReport = do
    report <- statusReport
    (Just dzenIn, _, _, _) <- createProcess $
        (shell "dzen2 -y -1 -h 24")
            { std_in = CreatePipe }
    B.hPutStrLn dzenIn report
    hFlush dzenIn
    return $ Infinite $ hClose dzenIn >> return (Infinite toggleReport)

statusReport :: IO ByteString
statusReport = do
    -- general ones
    bs <- mapM (\(f, command) -> fmap f (execShellGet command))
        [ (volume, "amixer -c 1 get Master")
        ]
    -- special ones
    bat <- fmap (maybe "(battery info error)" battery .
        I.parseMaybe I.batteryInfos) $ execShellGet
            "upower -i /org/freedesktop/UPower/devices/battery_BAT0"
    stamp <- formattedTime
    return $ B.intercalate " | " $ bat : stamp : bs

worker :: Chan () -> IO a
worker taskQueue = do
    readChan taskQueue
    bat <- fmap (I.parseMaybe I.batteryInfos) $
        execShellGet "upower -i /org/freedesktop/UPower/devices/battery_BAT0"
    fromMaybe (return ()) $
        bat >>= lookup Percentage >>= \ b ->
            if readWord8 (rdrop 1 b) < 50
                then return (notify b 2)
                else return (return ())

    _ <- forkIO $ timer 60 taskQueue
    worker taskQueue
  where
    readWord8 :: ByteString -> Word8
    readWord8 = read . B.unpack

timer :: Int -> Chan () -> IO ()
timer n taskQueue = do
    threadDelay $ 1000 * 1000 * n
    writeChan taskQueue ()

battery :: [(BatteryInfo, ByteString)] -> ByteString
battery = (B.intercalate ", ") . map snd

volume :: ByteString -> ByteString
volume b = fromMaybe "(volume info error)" $
    fmap ("Master " `mappend`) (I.parseMaybe I.volumeInfo b)

formattedTime :: IO ByteString
formattedTime = fmap (B.pack . localTimePrint fmt) localDateCurrent
  where
    fmt =
        [ Format_Year4, Format_Text '-'
        , Format_Month2, Format_Text '-'
        , Format_Day2, Format_Spaces
        , Format_Hour, Format_Text ':'
        , Format_Minute
        ]

-- utility functions

rdrop :: Int -> ByteString -> ByteString
rdrop n b = B.take (B.length b - n) b

execShellGet :: String -> IO ByteString
execShellGet command = do
    (_, Just dateHdl, _, _) <- createProcess $
        (shell command) { std_out = CreatePipe }
    B.hGetContents dateHdl

execShell :: String -> IO ()
execShell command = createProcess (shell command) >> return ()

notify :: ByteString -> Int -> IO ()
notify text delay = (return () <<) $ forkIO $ do
    (Just dzenIn, _, _, _) <- createProcess $
        (shell "dzen2 -x -600 -y 100 -tw 500 -h 24") { std_in = CreatePipe }
    B.hPutStrLn dzenIn text
    hFlush dzenIn
    threadDelay $ 1000 * 1000 * delay
    hClose dzenIn
  where
    (<<) = flip (>>)
