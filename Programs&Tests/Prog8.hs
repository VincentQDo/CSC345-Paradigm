{- ######################################################
		Prog8.hs
		Vincent Do, May 2019
######################################################## -}
module Prog8 where

--1. Write a function sumSqNeg that computes the "sum of squares of negatives". You must use one or more higher-order functions: map, filter, foldr
sumSqNeg :: [Int] -> Int
sumSqNeg xs = foldr (+) 0 (map doubleNegNum (filter isNeg xs))
  where 
    doubleNegNum x = x * x
    isNeg x = x < 0
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
containing' :: Eq a => [a] -> [a] -> Bool
containing' xs ys = and (map (containingHelp ys) xs) --this function will execute containingHelp on every item in xs and check if it belongs in ys

--this function helper is to check if an item is an element of a list. This function will take in one single list then return a function
containingHelp :: Eq a => [a] -> (a -> Bool)
containingHelp xs = fn --the retunred function then take in an item then check if that one item is in the list that was taken in by helper function
  where
    fn x = elem x xs
--TODO:: need to finish this
--5. Write a function lengths that returns a list of lengths of the given strings. You must use one or more higher-order functions: map, filter, foldr.
lengths :: [String] -> [Int]
lengths xs = map length xs
--6. Write a function product' that returns the product of a nonempty list of numbers. You must use one or more higher-order functions: map, filter, foldr.
product' :: Num a => [a] -> a
product' xs = foldr (*) 1 xs
-- --7. Write a function max' that returns the largest element of a nonempty list. You must use one or more higher-order functions: map, filter, foldr.
max' :: (Ord a, Num a) => [a] -> a
max' xs = foldr max 0 xs
--8. Write a function append' that appends two lists. You must use one or more higher-order functions: map, filter, foldr.
append' :: [a] -> [a] -> [a]
append' xs ys = foldr (:) [] (xs++ys)
--9. Write a function filterFirst that removes the first element from the list (second argument) that does not satisfy a given predicate function (first argument). You must use one or more higher-order functions: map, filter, foldr.
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst fn xs = removeFirst (head (filter fn xs)) xs

removeFirst :: Eq a => a -> [a] -> [a]
removeFirst x (y:ys) = case x == y of
    True -> ys
    False -> y : removeFirst x ys
{-

--10. Write a function filterLast that removes the last element from the list (second argument) that does not satisfy a given predicate function (first argument). You must use one or more higher-order functions: map, filter, foldr.
filterLast :: (a -> Bool) -> [a] -> [a]
-}