{- ######################################################
		Prog7.hs
		Vincent Do, April 2019
######################################################## -}
module Prog7 where

--Write a function unique that returns the list of elements that occur exactly once in the argument list. You must use recursion and not list comprehension. A helper function, or functions, may be useful.
unique :: Eq a => [a] -> [a]
unique (x:xs) = 
--Consider the following type:
data Expr1 = Val1 Int
           | Add1 Expr1 Expr1
           | Sub1 Expr1 Expr1
--Write a function value1 that evaluates an expression.
value1 :: Expr1 -> Int
--Create a Expr2 type constructor that also supports multiplication and division, in addition to the int literal, addition, and subtraction.
--Write a function value2 that evaluates an expression, but returns Nothing if there is a division by zero scenario.
value2 :: Expr2 -> Maybe Int
--Make the Expr2 type an instance of the Show class. Appropriate define the function show so that (Add2 (Val2 3) (Val2 4)) returns the string "3 + 4".
show :: Expr2 -> String
--Write a function piglatinize that returns a word into its piglatin form: if it begins with a vowel, add to the end "yay", else move non-vowels to the end of the string until a vowel is at the front and then add to the end "ay". The word arguments are guaranteed to have a vowel (a, e, i, o, or u) and not begin with the letter y.
piglatinize :: String -> String
--Consider the following type of binary trees:
data Tree a = Leaf a | Node (Tree a) (Tree a)
--A tree is balanced if the number of leaves in the left and right subtree of every node differ by at most one. Write a function balanced that returns whether a tree is balanced or not.
balanced :: Tree a -> Bool
--Now we will extend the Expr example above to contain conditional expressions. Take everything from Expr2, and create an Expr3, like so:
data Expr3 = Val3 Int
           | ...
           | If BExpr3 Expr3 Expr3
--The If data constructor (If BExpr3 Expr3 Expr3) will evaluate the boolean expression (first argument) and will return the value of the second argument if it is true, else it will return the third argument. Define the BExpr3 type as the following:
data BExpr3 = BoolLit Bool
            | Or BExpr3 BExpr3
            | EqualTo Expr3 Expr3
            | LessThan Expr3 Expr3
--Write a function bEval :: BExpr3 -> Bool that evaluates instances of the above boolean expression.
bEval :: BExpr3 -> Bool
--Write a function value3 that evaluates an expression.
value3 :: Expr3 -> Maybe Int