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

import Data.Serialize (decode)

import Battery
import qualified Parser as I
import Socket

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    taskQueue <- newChan
    writeChan taskQueue ()
    sock <- unixSocket "/tmp/prospect.socket"
    _ <- forkIO $ awaitTouch sock $ writeChan taskQueue ()
    worker taskQueue

worker :: Chan () -> IO a
worker taskQueue = do
    readChan taskQueue

    -- general ones
    bs <- mapM (\(f, command) -> fmap f (execShellGet command))
        [ (volume, "amixer -c 1 get Master")
        ]

    -- special ones
    bat <- fmap (I.parseMaybe I.batteryInfos) $
        execShellGet "upower -i /org/freedesktop/UPower/devices/battery_BAT0"
    batStr <- case bat of
        Just infos -> case lookup Percentage infos of
            Just b -> case decode (rdrop 1 b) of
                Left _ -> return "(battery info parsing error)"
                Right n -> if n < (100 :: Word8)
                    then do
                        _ <- execShell "notify-send Low battery"
                        return $ battery infos
                    else return $ battery infos
            Nothing -> return "(battery info error)"
        Nothing -> return "(battery info error)"
    (stamp, sec) <- timeS

    -- print them out
    B.putStrLn $ B.intercalate " | " $ batStr : stamp : bs

    _ <- forkIO $ timer (60 - sec) taskQueue
    worker taskQueue

timer :: Int -> Chan () -> IO ()
timer n taskQueue = do
    threadDelay (1000 * 1000 * n)
    writeChan taskQueue ()

battery :: [(BatteryInfo, ByteString)] -> ByteString
battery = (B.intercalate ", ") . map snd

volume :: ByteString -> ByteString
volume b = fromMaybe "(volume info error)" $
    fmap ("Master " `mappend`) (I.parseMaybe I.volumeInfo b)

-- returns formatted time in ByteString and seconds in Int.
timeS :: IO (ByteString, Int)
timeS = do
    dt <- localDateCurrent
    return (B.pack $ localTimePrint fmt dt, extractSecond dt)
  where
    fmt =
        [ Format_Year4, Format_Text '-'
        , Format_Month2, Format_Text '-'
        , Format_Day2, Format_Spaces
        , Format_Hour, Format_Text ':'
        , Format_Minute
        ]
    extractSecond = fromIntegral . todSec . dtTime . localTimeUnwrap

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
