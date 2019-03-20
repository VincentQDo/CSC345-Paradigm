{- ######################################################
		Prog4.hs
		Vincent Do, March 2019
######################################################## -}
module Prog4 where
type Date = (Int, Int, Int)
type Month = Int
type Day = Int
type Year = Int
type Season = String
--Write a function older that takes two dates and returns whichever one is older.
older :: Date -> Date -> Date
older (a,b,c) (d,e,f)
    | c > f = day2
    | a > d && c == f = day2
    | a == d && b > e && c == f = day2
    | otherwise = day1
    where
      day2 = (d,e,f)
      day1 = (a,b,c)
--Write a function numInMonth that takes a month and a list of dates and returns how many dates in the list match the given month.
numInMonth :: Month -> [Date] -> Int
numInMonth d list = length [1 | (x,y,z) <- list, d == x && validDate (x,y,z)]
--Write a function datesInMonth that takes a month and a list of dates and returns a list of dates that match the given month.
datesInMonth :: Month -> [Date] -> [Date]
datesInMonth a list = [(x,y,z) | (x,y,z) <- list, a == x && validDate (x,y,z)]
--Write a function date2Str that takes a date and returns its string equivalent in the form "February 23, 2018". (Hint: what is the operator for concatenating strings? Hint: look up how to convert an Int to a String.)
date2Str :: Date -> String
date2Str (month, day, year) = case validDate (month, day, year) of
  True -> month2String month ++ " " ++ show day ++ ", " ++ show year
  False -> "Date not available"
  where
    date = (month, day, year)
month2String :: Int -> String
month2String month
  | month == 1      = "January"
  | month == 2      = "February"
  | month == 3      = "March"
  | month == 4      = "April"
  | month == 5      = "May"
  | month == 6      = "June"
  | month == 7      = "July"
  | month == 8      = "August"
  | month == 9      = "September"
  | month == 10     = "October"
  | month == 11     = "November"
  | month == 12     = "December"
  | otherwise       = error "Date not available"
--Same as above, but do not use 12 conditionals. Instead, use a list holding 12 strings (the months) as well as the !! operator to index this list.
date2Str' :: Date -> String
months = ["January", "February", "March", "April", "May", "June", "July", "June", "August", "September", "October", "November", "December"]
date2Str' (0,_,_) = error "Date not in range"
date2Str' (month, day, year) = case validDate (month, day, year) of
  True -> months !! (month - 1) ++ " " ++ show day ++ ", " ++ show year
  False -> "Date not available"
--Write a function monthLookup that takes a numeric day in the calendar year (between 1 and 365) and returns what month that day is in (excluding leap years).
monthLookup :: Int -> Month
monthDays = [0,31,59,90,120,151,181,212,243,273,304,334,365]
monthLookup n
  | n <= x !! 0    = error "Day not in range"
  | n <= x !! 1    = 1
  | n <= x !! 2    = 2
  | n <= x !! 3    = 3
  | n <= x !! 4    = 4
  | n <= x !! 5    = 5
  | n <= x !! 6    = 6
  | n <= x !! 7    = 7
  | n <= x !! 8    = 8
  | n <= x !! 9    = 9
  | n <= x !! 10   = 10
  | n <= x !! 11   = 11
  | n <= x !! 12   = 12
  | otherwise      = error "Day not in range"
  where
    x = monthDays
--Write a function monthRange that takes two numeric days (from previous problem) and returns an integer list of the months between those dates (inclusive), e.g.: monthRange 23 101 should return [1,2,3,4]. If the second argument is earlier than the first argument, return the empty list.
monthRange :: Int -> Int -> [Month]
monthRange a b = [monthLookup a .. monthLookup b]
--Write a function validDate that takes a date and returns whether it is valid (e.g. November 31 is not valid). Do not be concerned about leap years.
validDate :: Date -> Bool
validDate (month, day, year) = (day + monthDays !! (month - 1)) <= (monthDays !! month)
--Write a function validLeapDate that takes a date and returns whether it is a leap date, that is exactly February 29th on a leap year. (Leap years are years that are either divisible by 400 or divisible by 4 but not divisible by 100.)
validLeapDate :: Date -> Bool
validLeapDate (month, day, year) =
  case month of
  2 -> case day of
    29 -> case mod year 100 == 0 of
      True -> case mod year 400 == 0 of
        True -> True
        False -> False
      False -> case mod year 4 == 0 of
        True -> True
        False -> False
    _ -> False
  _ -> False
--Write a function season that takes a date and returns the season that the date is in.
season :: Date -> Season
springEquinox = monthDays !! 3 + 20
summerSolstice = monthDays !! 6 + 21
fallEquinox = monthDays !! 9 + 22
winterSolstice = monthDays !! 12 + 21
season (month, day, year) =
  case validDate a of
    False -> case validLeapDate a of
      True -> "Winter"
      False -> "Not a valid date"
    True -> case findDay >= springEquinox of
      True -> case findDay >= summerSolstice of
        True -> case findDay >= fallEquinox of
          True -> case findDay >= winterSolstice of
            True -> "Winter"
            False -> "Fall"
          False -> "Summer"
        False -> "Spring"
  where
    a = (month, day, year)
    findDay = monthDays !! month + day
