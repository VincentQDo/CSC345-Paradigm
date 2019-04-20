{- ######################################################
		Prog7.hs
		Vincent Do, April 2019
######################################################## -}
module Prog7 where

--1. Write a function unique that returns the list of elements that occur exactly once in the argument list. You must use recursion and not list comprehension. A helper function, or functions, may be useful.
unique :: Eq a => [a] -> [a]
unique [] = []
unique xs = unique' xs xs

occurs :: Eq a => a -> [a] -> Int
occurs n [] = 0
occurs n (x:xs) = case n == x of
    True -> 1 + occurs n xs
    False -> occurs n xs

unique' :: Eq a => [a] -> [a] -> [a]
unique' [] _ = []
unique' (x:xs) ys = case occurs x ys of
    1 -> x : unique' xs ys
    _ -> unique' xs ys

--Consider the following type:
data Expr1 = Val1 Int
           | Add1 Expr1 Expr1
           | Sub1 Expr1 Expr1
--2. Write a function value1 that evaluates an expression.
value1 :: Expr1 -> Int
value1 expr = case expr of
    Val1 n                      -> n
    Add1 (Val1 n) (Val1 m)      -> n+m
    Sub1 (Val1 n) (Val1 m)      -> n-m
    Add1 expr (Val1 n)           -> (value1 expr)+n
    Add1 (Val1 n) expr           -> n+(value1 expr)
    Sub1 expr (Val1 n)           -> (value1 expr)-n
    Sub1 (Val1 n) expr           -> n-(value1 expr)
    Add1 exp1 exp2              -> (value1 exp1)+(value1 exp2)
    Sub1 exp1 exp2              -> (value1 exp1)-(value1 exp2)

-- 3. Create a Expr2 type constructor that also supports 
-- multiplication and division, in addition to the int literal, addition, 
-- and subtraction.
-- data Expr2 = Expr1 --why does this not work, turn out this does not work because expr1 does not allow for Multi and Div so something like 2+(2*1) will not work with Expr1
data Expr2 = Val2 Int
           | Add2 Expr2 Expr2
           | Sub2 Expr2 Expr2
           | Multi2 Expr2 Expr2
           | Div2 Expr2 Expr2

--4. Write a function value2 that evaluates an expression, but returns Nothing if there is a division by zero scenario.
value2 :: Expr2 -> Maybe Int
value2 expr = case expr of
    Div2 _ (Val2 0) -> Nothing
    Val2 n -> Just n
    Add2 n m -> maybeAdd (value2 n) (value2 m)
    Sub2 n m -> maybeSub (value2 n) (value2 m)
    Multi2 n m -> maybeMulti (value2 n) (value2 m)
    Div2 n m -> maybeDiv (value2 n) (value2 m)


--all the methods below are basically helper functions for normal mathematical operations for Maybe type.
maybeAdd :: Maybe Int -> Maybe Int -> Maybe Int
maybeAdd _ Nothing = Nothing
maybeAdd Nothing _ = Nothing
maybeAdd (Just n) (Just m) = Just (n+m)
maybeSub :: Maybe Int -> Maybe Int -> Maybe Int
maybeSub _ Nothing = Nothing
maybeSub Nothing _ = Nothing
maybeSub (Just n) (Just m) = Just (n-m)
maybeDiv :: Maybe Int -> Maybe Int -> Maybe Int
maybeDiv _        Nothing   = Nothing
maybeDiv Nothing  _         = Nothing
maybeDiv _        (Just 0)  = Nothing
maybeDiv (Just n) (Just m)  = Just (div n m)
maybeMulti :: Maybe Int -> Maybe Int -> Maybe Int
maybeMulti _ Nothing = Nothing
maybeMulti Nothing _ = Nothing
maybeMulti (Just n) (Just m) = Just (n*m)
--TODO: use foldable for this, not sure how it will work may need some help later so come back to this.


--5. Make the Expr2 type an instance of the Show class. Appropriate define the function show so that (Add2 (Val2 3) (Val2 4)) returns the string "3 + 4".
instance Show Expr2 where
    show (Val2 n) = show n
    show n = init (tail (show' n))

show' :: Expr2 -> String
show' (Val2 n) = show n
show' (Add2 n m) = "(" ++ show' n ++ ['+'] ++ show' m ++ ")"
show' (Sub2 n m) = "(" ++ show' n ++ ['-'] ++ show' m ++ ")"
show' (Multi2 n m) = "(" ++ show' n ++ ['*'] ++ show' m ++ ")"
show' (Div2 n m) = "(" ++ show' n ++ ['/'] ++ show' m ++ ")"
--TODO: get rid of the first and last "()" in number 5
--TODO: foldable here too maybe, althought i dont think foldable is useful here

--6. Write a function piglatinize that returns a word into its piglatin form: 
--if it begins with a vowel, add to the end "yay", 
--else move non-vowels to the end of the string until a vowel is at the front and then add to the end "ay". 
--The word arguments are guaranteed to have a vowel (a, e, i, o, or u) and not begin with the letter y.
piglatinize :: String -> String
piglatinize (x:xs) = case x of
    'a' -> (x:xs) ++ "yay"
    'e' -> (x:xs) ++ "yay"
    'i' -> (x:xs) ++ "yay"
    'o' -> (x:xs) ++ "yay"
    'u' -> (x:xs) ++ "yay"
    _   -> piglatinizeHelper (x:xs) ++ "ay" --this called a helper function in order to preserve the string as is and then add ay at the end of the recursive calls
piglatinizeHelper :: String -> String --this method is responsible for moving the non vowel char to the end and stop if it finds a vowel
piglatinizeHelper (x:xs) = case x of
    'a' -> (x:xs)
    'e' -> (x:xs)
    'i' -> (x:xs)
    'o' -> (x:xs)
    'u' -> (x:xs)
    _   -> piglatinizeHelper (xs ++ [x])

-- Consider the following type of binary trees:
data Tree a = Leaf a | Node (Tree a) (Tree a)
--7. A tree is balanced if the number of leaves in the left and right subtree of every node differ by at most one. Write a function balanced that returns whether a tree is balanced or not.
balanced :: Tree a -> Bool
balanced (Leaf a) = True
balanced (Node left right) = or [countLeaf left == (countLeaf right - 1), countLeaf right == (countLeaf left - 1), countLeaf left == countLeaf right]
countLeaf :: Tree a -> Int
countLeaf (Leaf a) = 1
countLeaf (Node n1 n2) = countLeaf n1 + countLeaf n2

--8. Now we will extend the Expr example above to contain conditional expressions. Take everything from Expr2, and create an Expr3, like so:
data Expr3 = Val3 Int
            | Add3 Expr3 Expr3
            | Sub3 Expr3 Expr3
            | Multi3 Expr3 Expr3
            | Div3 Expr3 Expr3
            | If BExpr3 Expr3 Expr3
--The If data constructor (If BExpr3 Expr3 Expr3) will evaluate the boolean expression 
--(first argument) and will return the value of the second argument if it is true, 
--else it will return the third argument. Define the BExpr3 type as the following:
data BExpr3 = BoolLit Bool
            | Or BExpr3 BExpr3
            | EqualTo Expr3 Expr3
            | LessThan Expr3 Expr3
--9. Write a function bEval :: BExpr3 -> Bool that evaluates instances of the above boolean expression.
bEval :: BExpr3 -> Bool
bEval (BoolLit n) = n
bEval (Or n m) = or [bEval n, bEval m]
bEval (EqualTo n m) = value3 n == value3 m
bEval (LessThan n m) = value3 n < value3 m
--10. Write a function value3 that evaluates an expression.
value3 :: Expr3 -> Maybe Int
value3 expr = case expr of
    Div3 _ (Val3 0) -> Nothing
    Val3 n -> Just n
    Add3 n m -> maybeAdd (value3 n) (value3 m)
    Sub3 n m -> maybeSub (value3 n) (value3 m)
    Multi3 n m -> maybeMulti (value3 n) (value3 m)
    Div3 n m -> maybeDiv (value3 n) (value3 m)
    If b n m -> case bEval b of
        True -> value3 n
        False -> value3 m


--If Or (EqualTo (Val3 5) (Val3 2)) (LessThan (Val3 5) (Val3 2))
--         False                      True
-- True