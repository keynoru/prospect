{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parseMaybe
    , battery
    , volumeInfo
    ) where

import Control.Applicative

import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString.Char8

import Battery

parseMaybe :: Parser a -> ByteString -> Maybe a
parseMaybe parser b = result (parse parser b)
  where
    result :: Result r -> Maybe r
    result r = case r of
        Done _ raw -> Just raw
        ir@(Partial _) -> result (feed ir "")
        _ -> Nothing

battery :: Parser Battery
battery = skipLinesUntil $ do
    string "    state:" >> skipSpaces
    rakeTill '\n' >>= \ s -> case s of
        "fully-charged" -> return FullyCharged
        "discharging"   -> discharging
        "charging"      -> charging
        _               -> return $ UnknownBatteryState s
  where
    discharging = fmap Discharging percentage
    percentage = skipLinesUntil $
        string "    percentage:" >> skipSpaces >> decimal
    charging = skipLinesUntil $ do
        string "    time to full:" >> skipSpaces
        time <- rakeTill '\n'
        perc <- percentage
        return $ Charging perc time

volumeInfo :: Parser ByteString
volumeInfo = skipLinesUntil $
    string "  Mono:" >> rakeTill '[' >> takeTill (== ']')

-- utilities

skipLinesUntil :: Parser a -> Parser a
skipLinesUntil p = p <|> (rakeTill '\n' >> skipLinesUntil p)

rakeTill :: Char -> Parser ByteString
rakeTill x = do
    s <- takeTill (== x)
    _ <- anyChar
    return s

skipSpaces :: Parser ()
skipSpaces = takeTill (not . isSpace) >> return ()
