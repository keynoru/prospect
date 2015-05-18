{-# LANGUAGE OverloadedStrings #-}

module Battery
    ( Battery(..)
    , batteryFormatted
    ) where

import Data.Word
import Data.ByteString (ByteString)
import Data.ByteString.Builder

data Battery
    = FullyCharged
    | Discharging Word8
    | Charging Word8 ByteString
    | UnknownBatteryState ByteString
    deriving Show -- debug

batteryFormatted :: Battery -> Builder
batteryFormatted FullyCharged = byteString ""
batteryFormatted (Discharging p) = mconcat
    [ byteString "Discharging ", word8Dec p, byteString "% | " ]
batteryFormatted (Charging p r) = mconcat
    [ byteString "Charging ", word8Dec p, byteString "% / "
    , byteString r , byteString " | "]
batteryFormatted (UnknownBatteryState x) = mconcat
    [ byteString x, byteString " | " ]
