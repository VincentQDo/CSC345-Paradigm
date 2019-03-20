{- ######################################################
		Prog1.hs
		Vincent Do, February 2019
######################################################## -}
module Prog1 where

isNegative :: Float -> Bool
isNegative n = n < 0

hasRemainder :: Integer -> Integer -> Bool
hasRemainder n m = (mod n m) > 0

middle :: Integer -> Integer -> Integer -> Integer
middle x y z
    | x >= y && x <= z || x <= y && x >= z     = x
    | y >= x && y <= z || y <= x && y >= z     = y
    | otherwise = z

nor :: Bool -> Bool -> Bool
nor a b = (a == True) && (b == False)

triangleArea :: Integer -> Integer -> Float
triangleArea h b = fromIntegral(h * b) / 2

tripleNumber :: Integer -> Integer
tripleNumber a = if a <= 100 then a * 3 else a

isVowel :: Char -> Bool
isVowel a
     | a == 'g' || a == 'A'     = True
		 | a == 'e' || a == 'E'     = True
		 | a == 'i' || a == 'I'     = True
		 | a == 'o' || a == 'O'     = True
		 | a == 'u' || a == 'U'     = True
		 | otherwise                = False
letterGrade :: Integer -> String
letterGrade n
     | n > 100 || n < 0 = error "Grade can't be greater than 100 or less than 0"
     | n >= 93       = "A"
		 | n >= 90       = "A-"
		 | n >= 87       = "B+"
		 | n >= 83       = "B"
		 | n >= 80       = "B-"
		 | n >= 77       = "C+"
		 | n >= 73       = "C"
		 | n >= 70       = "C-"
		 | n >= 67       = "D+"
		 | n >= 63       = "D"
		 | n >= 60       = "D-"
		 | otherwise     = "F"

averageThree :: Integer -> Integer -> Integer -> Float
averageThree n m p = fromInteger(n + m + p) / 3

howManyBelowAverage :: Integer -> Integer -> Integer -> Integer
howManyBelowAverage n m p
		 | n < ceiling(averageThree n m p) && m < ceiling(averageThree n m p)      = 2
		 | n < ceiling(averageThree n m p) && p < ceiling(averageThree n m p)      = 2
		 | m < ceiling(averageThree n m p) && p < ceiling(averageThree n m p)      = 2
     | n == m && m == p                                                        = 0
		 | otherwise                                                             = 1
