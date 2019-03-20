{- ######################################################
		Prog2.hs
		Vincent Do, February 2019
######################################################## -}
module Prog2 where

  --1 use recursion to compute the sum of all numbers from 1 to n.
  sum' :: Integer -> Integer
  sum' 0 = 0
  sum' n = if n > 0 then n + sum' (n-1) else error "Number needs to be positive"
  --2 Write a function integerSqrt that returns the integer square root of a positive integer n.
  --(The integer square root is defined to be the largest integer whose square is
  --less than or equal to n, i.e. the result of integerSqrt 15 is 3.)
  --3 Incorporate into the above function definitions an error message that will be output
  --if the function is called with a value that is less than or equal to 0.
  integerSqrt :: Integer -> Integer
  integerSqrt n = if n >= 0 then floor (sqrt (fromInteger n)) else error "Please enter a number greater than 0"
  --4 Write a function exponent' that recursively computes the result of raising
  --some base number, b, to some exponent, e. (e.g. 2^8 = 64).
  --You may not use the ^ or ** operators -- you must use recursion.
  --This function will only be called on an exponent value that is a whole number
  --(an integer that is 0 or greater).
  exponent' :: Integer -> Integer -> Integer
  exponent' n 0 = 1
  exponent' n m = n * (exponent' n (m - 1))
  --5 Redefine the built-in infix || operator.
  --Name your new function or'. Do not use guards in your definition,
  --but rather utilize pattern matching. Use the wildcard _ pattern so that you only have two definitions,
  --rather than the four definitions we would usually write given the truth table of ||.
  or' :: Bool -> Bool -> Bool
  or' False False = False
  or' _ _ = True
  --6 Write a function orderTriple that takes a triple,
  --and returns a version in decreasing order.
  --(Hint: you may want to define other helper functions such as maxOfThree, middleOfThree, and minOfThree.)
  maxOfThree :: (Integer, Integer, Integer) -> Integer
  maxOfThree (a, b, c)
    | a >= b && a >= c     = a
    | b >= a && b >= c     = b
    | otherwise            = c
  middleOfThree :: (Integer, Integer, Integer) -> Integer
  middleOfThree (a, b, c)
    | a >= b && a <= c || a <= b && a >= c     = a
    | b >= a && b <= c || b <= a && b >= c     = b
    | otherwise                                = c
  minOfThree :: (Integer, Integer, Integer) -> Integer
  minOfThree (a, b, c)
    | a <= b && a <= c     = a
    | b <= a && b <= c     = b
    | otherwise            = c
  orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
  orderTriple (a, b, c) = (maxOfThree (a, b, c), middleOfThree (a, b, c), minOfThree (a, b, c))
  --7 Write a function swap that swaps the characters in a quadruple (4-tuple) in the following way:
  --the first elements swaps with the last, and the middle two flip.
  --Only use pattern matching. You may not call any other functions.
  swap :: (Char, Char, Char, Char) -> (Char, Char, Char, Char)
  swap (a, b, c, d) = (d, c, b, a)
  --8 Write a function asciiNums that takes a String and returns a list of the ascii values of characters
  -- in that string.
  asciiNums :: String -> [Int]
  asciiNums s = [fromEnum c | c <- s]
  --9 Write a function matches that picks out all instances of an integer n from a list.
  --(Example: matches 3 [3,4,5,3] should return [3,3] and matches 3 [4,5,7] should return [].)
  matches :: Integer -> [Integer] -> [Integer]
  matches k t = [k' | k' <- t, k' == k]
  --10 Use the matches function in the above problem to write a function element
  --that returns True if an element is a member of a list, False otherwise.
  --(You may not use the elem function that is already defined in Haskell.)
  element :: Integer -> [Integer] -> Bool
  element k t = not(matches k t == [])
