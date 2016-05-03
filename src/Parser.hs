module Parser
    ( parseMaybe
    , timestamp
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

timestamp :: Parser Integer
timestamp = decimal

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
