module WeekDay
  ( WeekDay(..)
  , nextDay
  , afterDays
  , isWeekend
  , daysToParty
  ) where

data WeekDay
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving(Eq, Show, Enum)

nextDay :: WeekDay -> WeekDay
nextDay Sunday = Monday
nextDay day = succ day

afterDays :: WeekDay -> Int -> WeekDay
afterDays day n = last $ take (n + 1) $ iterate nextDay day

isWeekend :: WeekDay -> Bool
isWeekend day = day == Sunday || day == Saturday

daysToParty :: WeekDay -> Int
daysToParty = count 0
  where
    count :: Int -> WeekDay -> Int
    count acc Friday  = acc
    count acc day     = count (acc + 1) (nextDay day)