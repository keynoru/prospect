module Infinite where

data Infinite = Infinite (IO Infinite)
