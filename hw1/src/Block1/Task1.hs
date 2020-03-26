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


data DayOfWeek = Monday
               | Tuesday
               | Wednesday
               | Thursday
               | Friday
               | Saturday
               | Sunday deriving (Show)

instance Eq DayOfWeek where
  x == y = dayToInt x == dayToInt y

dayFromInt :: Int -> DayOfWeek
dayFromInt 0 = Monday
dayFromInt 1 = Tuesday
dayFromInt 2 = Wednesday
dayFromInt 3 = Thursday
dayFromInt 4 = Friday
dayFromInt 5 = Saturday
dayFromInt 6 = Sunday
dayFromInt x = dayFromInt $ ((x `mod` 7) + 7) `mod` 7

dayToInt :: DayOfWeek -> Int
dayToInt Monday    = 0
dayToInt Tuesday   = 1
dayToInt Wednesday = 2
dayToInt Thursday  = 3
dayToInt Friday    = 4
dayToInt Saturday  = 5
dayToInt Sunday    = 6

afterDays :: Int -> DayOfWeek -> DayOfWeek
afterDays inc day = dayFromInt $ dayToInt day + inc

nextDay :: DayOfWeek -> DayOfWeek
nextDay = afterDays 1

isWeekend :: DayOfWeek -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

daysToParty :: DayOfWeek -> Int
daysToParty day = (dayToInt Friday - dayToInt day + 7) `mod` 7
