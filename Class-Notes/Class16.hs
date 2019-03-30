-- recursive algebraic data types
-- list is a recursive data types
-- a list can either be an empty list of a single element followed by the remaining list
-- 4:5:[] in this case 4 is followed by 5:[] and 5 is followed by [] which is theempty list
-- we can make our own list type constructor or type and object
-- data List a = Empty -- the a will allow tihs data type to return any thing
--             | Cons a (List a) -- type variable a can be anh type like double or int
-- data IntList = Empty -- this list can only ever be int
--              | Cons Int IntList
-- --exmamle favorite example
-- integer
--   expression
--   -int literal
--   -arith. op. + -
--
--   the integer expression can be 123 which is int literal or 2+2 which is arith op.
--
--     1 + 2
--       +
--     /   \
--    1     2
--
--
--   (3-1) + 3
--         +
--       /   \
--      -     3
--    /   \
--   3     1
--

data Expr = Lit Int
          | Add Expr Expr
          | Sub Expr Expr
-- List 123 = 123
--
-- Add (Lit 1) (Lit 2) which is 1 + 2
-- Add (Sub (Lit 3) (Lit 1)) (Lit 2) which is (3 - 1) + 3
--
-- given an expression
-- Problem 1: write a haskell function that will evaluate it
-- Problem 2: output the tree as a string
-- Problem 3: counts the number of operators
-- Problem 4: find the largest leaf
--
-- Problem 1
eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
exprToStr :: Expr -> String
exprToStr (Lit n) = show n
exprToStr (Add e1 e2) = "(" ++ exprToStr e1 ++ "+" ++ exprToStr e2 ++ ")"
exprToStr (Sub e1 e2) = "(" ++ exprToStr e1 ++ "-" ++ exprToStr e2 ++ ")"
countOp :: Expr -> Int
countOp (Lit n) = 0
countOp (Add e1 e2) = 1 + countOp e1 + countOp e2
countOp (Sub e1 e2) = 1 + countOp e1 + countOp e2
largestLeaf :: Expr -> Int
largestLeaf (Lit n) = n
largestLeaf (Add e1 e2) = max largestLeaf e1 largestLeaf e2
largestLeaf (Sub e1 e2) = max largestLeaf e1 largestLeaf e2
