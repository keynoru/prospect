module Battery where

data BatteryInfo
    = BatteryState
    | TimeToFull
    | Percentage
    deriving Eq
