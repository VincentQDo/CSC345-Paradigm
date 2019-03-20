{- ######################################################
		Prog3.hs
		Vincent Do, February 2019
######################################################## -}
module Prog3 where
import Data.Char (toLower)

--1. Write a function sumLastPart which, only using library functions, returns the sum of the last n numbers in the list, where n is the first argument to the function.
sumLastPart :: Int -> [Int] -> Int
sumLastPart x xs = sum $ drop (length xs - x) xs
--2. Write a function init' that has identical behavior to the init function.
--In your definition, you may only use the standard Haskell functions that operate on lists (except for init).
init' :: [Int] -> [Int]
init' [] = error "empty list"
init' xs = reverse $ tail $ reverse xs
--3. Write a recursive function init'' that has the same behavior as init'.
--No standard Haskell functions may be used.
init'' :: [Int] -> [Int]
init'' [] = error "empty list"
init'' [a] = []
init'' (x:xs) = x : init'' xs
--4. Write a recursive function elemAt that returns the ith item of the list, where the first item is index 1.
--You may not use any of the standard Haskell functions that operate on lists.
elemAt :: Int -> [Int] -> Int
elemAt _ [] = error "Index out of bound"
elemAt 1 (x:xs) = x
elemAt a (x:xs) = elemAt (a - 1) xs
--5. Write a function numTimes that returns the number of times that an element occurs in the list. Use recursion, not a list comprehension.
numTimes :: Int -> [Int] -> Int
numTimes x [] = 0
numTimes x (head:tail)
  | x == head     = 1 + numTimes x tail
  | otherwise     = numTimes x tail
--6. Write a function lowerFirstLetter that lowercases the first and only first letter of a string.
lowerFirstLetter :: String -> String
lowerFirstLetter [] = []
lowerFirstLetter (x:xs) = toLower x : xs
--7. Write a function and' that uses recursion to return the conjunction of a list of boolean values.
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs)
  | x == True       = and' xs
  | otherwise       = False
--8. Write a function or' that uses recursion to return the disjunction of a list of boolean values.
or' :: [Bool] -> Bool
or' [] = False
or' (x:xs)
  | x == True       = True
  | otherwise       = or' xs
--9. Write a function iSort' that uses insertion sort to sort a list of triples (Float, Int, String) where only the second element (the Int part of the triple) is to be considered during the sorting process.
-- Helper function used to access the second function of the triple Credit to the snd function
secondEle :: (Float,Int,String) -> Int
secondEle (_,b,_) = b
--helper function used to insert the triple back into the list
insert :: (Float, Int, String) -> [(Float, Int, String)] -> [(Float, Int, String)]
insert (a,b,c) [] = [(a,b,c)]
insert (a,b,c) ((d,e,f):tail)
  | b <= e        = (a,b,c) : y : tail
  | otherwise    = y : insert x tail
  where
    x = (a,b,c)
    y = (d,e,f)
iSort' :: [(Float, Int, String)] -> [(Float, Int, String)]
iSort' [] = []
iSort' (x:xs) = insert x $ iSort' xs
--10. Write a function merge that takes two sorted lists (decreasing order) and merges them into a single sorted list (decreasing order).
insert' :: Int -> [Int] -> [Int]
insert' x [] = [x]
insert' x (y:ys)
  | x >= y        = x : y : ys
  | otherwise    = y : insert' x ys
iSort'' :: [Int] -> [Int]
iSort'' [] = []
iSort'' (x:xs) = insert' x $ iSort'' xs
merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge [] list = iSort'' list
merge list [] = iSort'' list
merge list1 list2 = iSort'' $ iSort'' list1 ++ iSort'' list2
