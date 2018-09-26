module Day
( Day(..)
) where

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
      deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Show
-- show Friday

-- Read
-- read "Friday" :: Day

-- Eq
-- Friday == Saturday
-- Friday /= Saturday

-- Ord
-- Saturday > Friday
-- Monday `compare` Wednesday

-- Bounded
-- minBound :: Day
-- maxBound :: Day

-- Enum
-- succ Monday
-- pred Saturday
-- [ThursDay .. Sunday]
-- [minBound .. maxBound] :: [Day]
