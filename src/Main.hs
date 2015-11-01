{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import Control.Monad

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Builder

import Control.Concurrent
import System.Process
import System.IO hiding (char8)

import Data.Hourglass
import System.Hourglass

import Battery
import qualified Parser as P
import Infinite
import Socket

data BarSignal = DeleteBar | CreateBar | UpdateBar (Maybe Battery)
data DZenProcess = NoDZen | DZen Handle ProcessHandle

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    barQueue <- newChan
    writeChan barQueue CreateBar
    _ <- forkIO $ barWorker barQueue NoDZen

    sock <- unixSocket "/tmp/prospect.socket"
    _ <- forkIO $ awaitTouch sock $ toggleBar barQueue

    tick barQueue

tick :: Chan BarSignal -> IO ()
tick barQueue = do
    bat <- getBattery
    writeChan barQueue (UpdateBar bat)
    threadDelay $ 1000 * 1000 * 60
    tick barQueue

getBattery :: IO (Maybe Battery)
getBattery = fmap (P.parseMaybe P.battery) $
    execShellGet "upower -i /org/freedesktop/UPower/devices/battery_BAT0"

barWorker :: Chan BarSignal -> DZenProcess -> IO ()
barWorker queue mhdl = do
    sig <- readChan queue
    case sig of
        DeleteBar -> case mhdl of
            NoDZen -> barWorker queue mhdl
            DZen hdl phdl ->
                hClose hdl >> waitForProcess phdl >> barWorker queue NoDZen
        CreateBar -> do
            mbat <- getBattery
            upsertBar mbat mhdl
        UpdateBar mbat -> warnBattery mbat >> case mhdl of
            NoDZen -> barWorker queue mhdl
            DZen hdl phdl -> updateBar mbat hdl phdl
  where
    upsertBar mbat NoDZen = do
        (Just dzenIn, _, _, ph) <- createProcess $
            (shell "dzen2 -y -1 -h 20")
                { std_in = CreatePipe }
        updateBar mbat dzenIn ph
    upsertBar mbat (DZen dzenIn ph) = updateBar mbat dzenIn ph
    updateBar mbat dzenIn ph = do
        report <- statusReport mbat
        hPutBuilder dzenIn report
        hFlush dzenIn
        barWorker queue (DZen dzenIn ph)
    warnBattery mbat = maybeAct mbat $ \bat -> case bat of
        Discharging p -> if p < 50
            then notify (word8Dec p)
            else return ()
        _ -> return ()

toggleBar :: Chan BarSignal -> IO Infinite
toggleBar queue = do
    writeChan queue DeleteBar
    return $ Infinite $ do
        writeChan queue CreateBar
        return (Infinite $ toggleBar queue)

statusReport :: Maybe Battery -> IO Builder
statusReport mbat = do
    vol <- fmap volume $ execShellGet "amixer -c 1 get Master"
    stamp <- formattedTime
    return $ mconcat
        [bat, stamp, byteString " | ", byteString vol, char8 '\n']
  where
    bat = maybe (byteString "(battery info error)") batteryFormatted mbat

volume :: ByteString -> ByteString
volume b = fromMaybe "(volume info error)" $
    fmap ("Master " `mappend`) (P.parseMaybe P.volumeInfo b)

formattedTime :: IO Builder
formattedTime = fmap (string8 . localTimePrint fmt) localDateCurrent
  where
    fmt =
        [ Format_Year4, Format_Text '-'
        , Format_Month2, Format_Text '-'
        , Format_Day2, Format_Spaces
        , Format_Hour, Format_Text ':'
        , Format_Minute
        ]

-- utility functions

execShellGet :: String -> IO ByteString
execShellGet command = do
    (_, Just dateHdl, _, pHandle) <- createProcess $
        (shell command) { std_out = CreatePipe }
    B.hGetContents dateHdl <* waitForProcess pHandle

notify :: Builder -> IO ()
notify text = do
    (Just dzenIn, _, _, pHandle) <- createProcess $
        (shell "dzen2 -x -600 -y 100 -tw 200 -h 100 -p 3")
            { std_in = CreatePipe }
    hPutBuilder dzenIn $ text `mappend` char8 '\n'
    hClose dzenIn
    void $ waitForProcess pHandle

maybeAct :: Maybe a -> (a -> IO ()) -> IO ()
maybeAct Nothing _ = return ()
maybeAct (Just x) action = action x
