{- ######################################################
		Prog5.hs
		Vincent Do, February 2019
######################################################## -}
module Prog5 where

--Write a function reverse' that reverses a list. You must use a case expression inside of your function definition.
--You may not use any built-in Haskell functions.
reverse' :: [a] -> [a]
--Write a function isPalindrome that returns if some list can be read the same way forward and backward.
--(e.g. "12345" and "madam")
isPalindrome :: String -> Bool
--Write a function safeFindAfter that takes a string and a list of strings,
--and returns the remainder of list after the given string is found.
--The function should be "safe", that is returning the Maybe type.
--The item is guaranteed to be in the list a maximum of one time.
--(Hint: what are the two relevant data constructors for this problem?)
safeFindAfter :: String -> [String] -> Maybe [String]
--In the following functions, we are going to implement a Set algebraic data type
--by taking advantage of built-in Haskell lists. But, we'll have to manually check
--that we never violate most important property of lists: that there are no duplicate items.
--Use the following type constructor and data constructors:

data Set = Set [Int]
         | EmptySet
    deriving Show
--Write a function member that checks whether the given item is present in the given set.
member :: Int -> Set -> Bool
--Write a function size that returns the number of elements in a given set.
size :: Set -> Int
--Write a function add that inserts the given item into a set.
--(If the item is already in the set, simply return the set unmodified.)
--(Hint: you may want to program a helper function that takes two Sets and merges them into one.)
add :: Int -> Set -> Set
--Write a function safeRemoveMax that removes the largest element from a set of integers.
safeRemoveMax :: Set -> Maybe Int
--Write a function equal that returns whether two sets are equal.
equal :: Set -> Set -> Bool
--Write a function union that takes two sets and returns the union of both sets.
union :: Set -> Set -> Set
--Write a function intersection that takes two sets and returns the intersection of them.
intersection :: Set -> Set -> Set
