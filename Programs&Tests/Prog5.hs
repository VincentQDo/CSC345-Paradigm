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
reverse' (x:xs) = reverse' xs ++ [x]
--2. Write a function isPalindrome that returns if some list can be read the same way forward and backward.
--(e.g. "12345" and "madam")
--SUPER EASY
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
    deriving (Eq, Show)
--4. Write a function member that checks whether the given item is present in the given set.
member :: Int -> Set -> Bool
member n EmptySet = False
member n (Set []) = False
member n (Set (x:xs)) = case n == x of
  True -> True
  False -> member n (Set xs)
--5. Write a function size that returns the number of elements in a given set.
--SUPER EASY
size :: Set -> Int
size EmptySet = 0
size (Set a) = length a
--6. Write a function add that inserts the given item into a set.
--(If the item is already in the set, simply return the set unmodified.)
--(Hint: you may want to program a helper function that takes two Sets and merges them into one.)
--NOT SURE IF NEED A HELPER
add :: Int -> Set -> Set
add n EmptySet = Set [n]
add n (Set a) = case length [x | x <- a, x == n] == 1 of
  True -> Set a
  False -> Set (a ++ [n])
--7. Write a function safeRemoveMax that removes the largest element from a set of integers.
safeRemoveMax :: Set -> Maybe Int
safeRemoveMax (Set []) = Nothing
safeRemoveMax EmptySet = Nothing
safeRemoveMax (Set [x,y]) = case x > y of
  True -> Just x
  False -> Just y
safeRemoveMax (Set (x:y:xs)) = case x > y of
  True -> safeRemoveMax $ Set (x:xs)
  False -> safeRemoveMax $ Set (y:xs)
--8. Write a function equal that returns whether two sets are equal.
--SUPER EASY
equal :: Set -> Set -> Bool
equal EmptySet EmptySet = True
equal (Set [a]) EmptySet = False
equal EmptySet (Set [a]) = False
equal (Set []) (Set []) = True
equal (Set [a]) (Set []) = False
equal (Set []) (Set [a]) = False
equal (Set [x]) (Set [y]) = x == y
equal (Set a) (Set b) = length [1 | z <- a, member z (Set b)] == length b
  -- where a = xs
  --       b = ys
--9. Write a function union that takes two sets and returns the union of both sets.
-- DIFFICULT
union :: Set -> Set -> Set
union EmptySet EmptySet = EmptySet
union EmptySet b = b
union a EmptySet = a
union (Set []) (Set []) = EmptySet
union (Set []) (Set b) = Set b
union (Set a) (Set []) = Set a
--all the patterns above are for special cases where there maybe an emptyset or a set of nothing
union (Set (x:xs)) (Set (y:ys)) = case x == y of
  True -> add x $ union (Set xs) (Set ys)
  False -> Set $ setToList $ add x $ add y $ union (Set xs) (Set ys)
setToList :: Set -> [Int]
setToList (Set x) = x
--10. Write a function intersection that takes two sets and returns the intersection of them.
-- SUPER EASY
intersection :: Set -> Set -> Set
intersection EmptySet EmptySet = EmptySet
intersection EmptySet _ = EmptySet
intersection _ EmptySet = EmptySet
intersection (Set []) (Set []) = EmptySet
intersection (Set []) (Set _) = EmptySet
intersection (Set _) (Set []) = EmptySet
intersection (Set a) (Set b)  = case Set [s | s <- a , member s (Set b)] of
  Set [] -> EmptySet
  Set (x:xs) -> Set [s | s <- a , member s (Set b)]
