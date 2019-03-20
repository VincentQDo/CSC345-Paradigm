{- ######################################################
		Prog5.hs
		Vincent Do, February 2019
######################################################## -}
module Prog5 where

--1. Write a function reverse' that reverses a list.
--You must use a case expression inside of your function definition.
--You may not use any built-in Haskell functions.
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = case xs == [] of
  False -> reverse' xs ++ [x]
  True -> []

  --[1,2,3,4] -> 2,3,4 not empty -> reverse' [2,3,4] ++ [1]
--2. Write a function isPalindrome that returns if some list can be read the same way forward and backward.
--(e.g. "12345" and "madam")
isPalindrome :: String -> Bool
isPalindrome s = reverse' s == s
--3. Write a function safeFindAfter that takes a string and a list of strings,
--and returns the remainder of list after the given string is found.
--The function should be "safe", that is returning the Maybe type.
--The item is guaranteed to be in the list a maximum of one time.
--(Hint: what are the two relevant data constructors for this problem?)
safeFindAfter :: String -> [String] -> Maybe [String]
safeFindAfter _ [] = Nothing
safeFindAfter str (x:xs) = case x == str of
  True -> Just xs
  False -> safeFindAfter str xs
--In the following functions, we are going to implement a Set algebraic data type
--by taking advantage of built-in Haskell lists. But, we'll have to manually check
--that we never violate most important property of lists: that there are no duplicate items.
--Use the following type constructor and data constructors:

data Set = Set [Int]
         | EmptySet
    deriving Show
--4. Write a function member that checks whether the given item is present in the given set.
member :: Int -> Set -> Bool
member n [] = False
--5. Write a function size that returns the number of elements in a given set.
size :: Set -> Int
size [] = 0
--6. Write a function add that inserts the given item into a set.
--(If the item is already in the set, simply return the set unmodified.)
--(Hint: you may want to program a helper function that takes two Sets and merges them into one.)
add :: Int -> Set -> Set
add n [] = [n]
--7. Write a function safeRemoveMax that removes the largest element from a set of integers.
safeRemoveMax :: Set -> Maybe Int
safeRemoveMax [] = Nothing
--8. Write a function equal that returns whether two sets are equal.
equal :: Set -> Set -> Bool
equal [] [] = True
--9. Write a function union that takes two sets and returns the union of both sets.
union :: Set -> Set -> Set
union [] [] = []
--10. Write a function intersection that takes two sets and returns the intersection of them.
intersection :: Set -> Set -> Set
intersection [] [] = []
