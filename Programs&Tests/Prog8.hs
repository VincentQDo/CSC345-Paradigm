{- ######################################################
		Prog8.hs
		Vincent Do, May 2019
######################################################## -}
module Prog8 where

--1. Write a function sumSqNeg that computes the "sum of squares of negatives". You must use one or more higher-order functions: map, filter, foldr
sumSqNeg :: [Int] -> Int
sumSqNeg xs = sum (map doubleNegNum (filter isNeg xs))

doubleNegNum :: Int -> Int
doubleNegNum n = -(n * n)

isNeg :: Int -> Bool
isNeg n = n < 0
--2. Write a function containing (without any higher order functions) that returns whether each element in the first list is also in the second list.
containing :: Eq a => [a] -> [a] -> Bool
containing [] [] = True
containing [] ys = True
containing xs [] = False
containing (x:xs) ys = case elem x ys of
    True -> containing xs ys
    False -> False
--3. Write a function total that applies the function (first argument) to every element in the list (second argument) and sums the result. You must use one or more higher-order functions: map, filter, foldr
total :: (Int -> Int) -> [Int] -> Int
total fn xs = sum (map fn xs)
--4. Write a function containing' (with higher order functions) that returns whether each element in the first list is also in the second list. You must use one or more higher-order functions: map, filter, foldr
-- containing' :: Eq a => [a] -> [a] -> Bool
-- containing' xs ys = length (filter ) == length xs
--5. Write a function lengths that returns a list of lengths of the given strings. You must use one or more higher-order functions: map, filter, foldr.
lengths :: [String] -> [Int]
lengths xs = map length xs
--6. Write a function product' that returns the product of a nonempty list of numbers. You must use one or more higher-order functions: map, filter, foldr.
product' :: Num a => [a] -> a
product' xs = foldr (*) 1 xs
{-
--7. Write a function max' that returns the largest element of a nonempty list. You must use one or more higher-order functions: map, filter, foldr.
max' :: Ord a => [a] -> a
--8. Write a function append' that appends two lists. You must use one or more higher-order functions: map, filter, foldr.
append' :: [a] -> [a] -> [a]
--9. Write a function filterFirst that removes the first element from the list (second argument) that does not satisfy a given predicate function (first argument). You must use one or more higher-order functions: map, filter, foldr.
filterFirst :: (a -> Bool) -> [a] -> [a]
--10. Write a function filterLast that removes the last element from the list (second argument) that does not satisfy a given predicate function (first argument). You must use one or more higher-order functions: map, filter, foldr.
filterLast :: (a -> Bool) -> [a] -> [a]
-}