{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parseMaybe
    , batteryInfos
    , volumeInfo
    ) where

import Data.Maybe
import Control.Applicative hiding (empty)

import Data.ByteString (ByteString, empty)
import Data.Attoparsec.ByteString.Char8

import Battery

parseMaybe :: Parser a -> ByteString -> Maybe a
parseMaybe parser b = result (parse parser b)
  where
    result :: Result r -> Maybe r
    result r = case r of
        Done _ raw -> Just raw
        ir@(Partial _) -> result (feed ir empty)
        _ -> Nothing

batteryInfos :: Parser [(BatteryInfo, ByteString)]
batteryInfos = fmap catMaybes $ many batteryLine
  where
    batteryLine = picky <|> generous
    picky = do
        _ <- string "    "
        choice
            [ string "state:" >> complete BatteryState
            , string "time to full:" >> complete TimeToFull
            , string "percentage:" >> complete Percentage
            , generous
            ]
    complete key = do
        skipSpaces
        value <- rakeTill '\n'
        return $ Just (key, value)
    generous = do
        _ <- rakeTill '\n'
        return Nothing

volumeInfo :: Parser ByteString
volumeInfo = keep <|> ditch
  where
    keep = string "  Mono:" >> rakeTill '[' >> takeTill (== ']')
    ditch = rakeTill '\n' >> volumeInfo

-- utilities

rakeTill :: Char -> Parser ByteString
rakeTill x = do
    s <- takeTill (== x)
    _ <- anyChar
    return s

skipSpaces :: Parser ()
skipSpaces = takeTill (not . isSpace) >> return ()
