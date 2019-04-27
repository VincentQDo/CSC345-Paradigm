{- ######################################################
		Prog8.hs
		Vincent Do, May 2019
######################################################## -}
module Prog8 where

--1. Write a function sumSqNeg that computes the "sum of squares of negatives". You must use one or more higher-order functions: map, filter, foldr
sumSqNeg :: [Int] -> Int
sumSqNeg xs = foldr (+) 0 (map (^2) (filter (<0) xs))

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
containingHelp xs = fn --This function will take in a list then return a function, that function will take in a single literal then check if that literal is an element of the list from the containingHelp function
  where
    fn x = elem x xs

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
append' xs ys = foldr (:) ys xs 

--9. Write a function filterFirst that removes the first element from the list (second argument) that does not satisfy a given predicate function (first argument). You must use one or more higher-order functions: map, filter, foldr.
filterFirst :: Eq a => (a -> Bool) -> [a] -> [a]
filterFirst fn xs = case reverseFilter fn xs of 
    [] -> xs --if the filter return empty list then just return the whole list because there will be nothing to remove
    _  -> removeFirst (head (reverseFilter fn xs)) xs --if filter return anything then take the head of that list and remove it from the input list then return the rest of the list
    where
        removeFirst :: Eq a => a -> [a] -> [a] --this function will remove the first elemtn in a list that is the same as the input element and return the previous list without that removed element
        removeFirst x (y:ys) = case x == y of
            True -> ys
            False -> y : removeFirst x ys
        reverseFilter fn xs = reverseIntersect (filter fn xs) xs
        reverseIntersect xs ys = [x | x <- ys, elem x xs == False]
--TODO: redo this this is oposite of the requirement, remove first item that does not satisfy not the first item that satisfy
--10. Write a function filterLast that removes the last element from the list (second argument) that does not satisfy a given predicate function (first argument). You must use one or more higher-order functions: map, filter, foldr.
filterLast :: Eq a => (a -> Bool) -> [a] -> [a]
filterLast fn xs = reverse (filterFirst fn (reverse xs))