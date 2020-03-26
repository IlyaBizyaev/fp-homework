module Block1.Task1
  ( DayOfWeek(..)
  , dayFromInt
  , dayToInt
  , nextDay
  , afterDays
  , isWeekend
  , daysToParty
  )
where

-- | ADT for days of week (starting with Monday).
data DayOfWeek = Monday
               | Tuesday
               | Wednesday
               | Thursday
               | Friday
               | Saturday
               | Sunday deriving (Show)

instance Eq DayOfWeek where
  x == y = dayToInt x == dayToInt y

-- | Utility method to get the day in week by its index (mod 7 is applied).
dayFromInt :: Int -> DayOfWeek
dayFromInt 0 = Monday
dayFromInt 1 = Tuesday
dayFromInt 2 = Wednesday
dayFromInt 3 = Thursday
dayFromInt 4 = Friday
dayFromInt 5 = Saturday
dayFromInt 6 = Sunday
dayFromInt x = dayFromInt $ ((x `mod` 7) + 7) `mod` 7

-- | Utility method to get an index of the day in week.
dayToInt :: DayOfWeek -> Int
dayToInt Monday    = 0
dayToInt Tuesday   = 1
dayToInt Wednesday = 2
dayToInt Thursday  = 3
dayToInt Friday    = 4
dayToInt Saturday  = 5
dayToInt Sunday    = 6

-- | Get day of week in 'inc' days from the specified day.
afterDays :: Int -> DayOfWeek -> DayOfWeek
afterDays inc day = dayFromInt $ dayToInt day + inc

-- | Get next day of week; wraps, equivalent to 'afterDays' 1.
nextDay :: DayOfWeek -> DayOfWeek
nextDay = afterDays 1

-- | Check if the specified day of week is Saturday or Sunday.
isWeekend :: DayOfWeek -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

-- | Calculate days remaining until Friday; wraps.
daysToParty :: DayOfWeek -> Int
daysToParty day = (dayToInt Friday - dayToInt day + 7) `mod` 7
