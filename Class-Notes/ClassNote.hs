--Class note today

-- occurs :: Eq a => a -> Tree a -> Bool --a need to be a member of the Eq class
-- occurs x (Leaf y) = x == y
-- occurs x (Node left y right) = x == y || occurs x left || occurs x right --what does || do again?

-- flatten :: Tree a -> [a] --will produce a inorder traversal
-- flatten (Leaf a) = [a]
-- flatten (Node left r right) = flatten left ++ [r] ++ flatten right

-- data Tree1 a = Leaf1 a | Node1 (Tree1 a)(Tree1 a) --data only on leaf in this tree
-- data Tree2 a = Leaf2 | Node2 (Tree1 a) a (Tree1 a) --data only in interior node in this tree
-- data Tree3 a b = Leaf3 b | Node3 (Tree1 a b) a (Tree1 a b) --multiple parameterized data type on Tree3
-- data Tree4 a = Leaf4 a | Node4 [Tree4 a]
{-# LANGUAGE FlexibleInstances #-}
data Nat = Zero | Succ Nat
-- this data type would produce
-- 0 = Zero
-- 1 = Succ Zero
-- 2 = Succ Succ Nat
-- 3 = Succ Succ Nat

nat2Int :: Nat -> Int --maybe usefull for the final
nat2Int Zero = 0
nat2Int (Succ n) = 1 + nat2Int (n)

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = add m (Succ n)


-- Class note April 11
-- Classes

-- type Classes is used to define a set of operations (similar to an interface in java)
-- class in haskell is a set or collection of types or multiple types
-- members of a class, called instances
-- for example Eq a, for a to belong in the Eq class 'a' haveto satisfy the definition for (==) and (/=)
-- to belong to a class an instanc must provide the definition for every operation that the class specifying that it need a definition for
-- polymorphic fn's vs overloaded fn's
-- length :: [a] -> Int --this is a polymorphic function becuase it works on any data type, it works the same for any data type
-- (+) --function is overloaded (haskell refer to this as ad-hoc polymorphism) the underline code for addition may change depends on the data type that you are passing in, it does not work with all data type

--elem :: Eq a => a -> [a] -> Bool -- a has to be an instanced of Eq and 'a' in [a] need ot be the same data type as 'a'

data Shape = Circle Int | Rect Int Int | Square Int --Creating our own data type as an instance of Eq
    deriving (Show, Eq) --This line is magicall because it will automatically make the data type we just created, become instance of Eq and we dont have to do the part right after for defining instance of Eq
-- instance Eq Shape where
--     (Circle r1) == (Circle r2)    = r1 == r2
--     (Rect r1 r2) == (Rect r3 r4)  = r1 == r3 && r2 == r4
--     (Square r1) == (Square r2)    = r1 == r2
--     (Rect r1 r2) == (Square r3)   = r1 == r2 && r1 == r3 
--     (Square r3) == (Rect r1 r2)   = r1 == r2 && r1 == r3
--     _ == _                        = False
    --shape1 /= shape2              = not (shape1 == shape2) --This line is not needed because Eq have a default def for /= because they ar emutually recursive we will only have to define the definition for either == or /=, not both

-- class Eq a where
--     (==), (/=) :: a -> a -> Bool
--     x == y    = not (x /= y)
--     x /= y    = not (x == y)

-- class Show a where
--     show :: a -> String
-- data Roman = Roman Int
-- instance Show Roman where
--     show (Roman n)
--         | n >= 1 && n <= 3     = replicate n "I"
--         | n == 4               = "IV"
--         | n == 5               = "V"
--         | n > 5 && n < 9       = "V" ++ (replicate (n - 5) "I")
--         | n == 9               = "IX"
--         | n == 10              = "X"


--Create my own class
class Listable a where
   toList :: Listable a => a -> [Int]

-- class of things
--     that can be converted to a list of ints


data Tree a = Empty | Node a (Tree a) (Tree a)

instance Listable Int where
    toList x = [x]
instance Listable Bool where
    toList True = [1]
    toList False = [0]
instance Listable [Int] where
    toList xs = xs
--April 16th, 2019
instance Listable (Tree Int) where
    toList Empty = []
    toList (Node x l r) = (toList l) ++ [x] ++ (toList r)


sumL xs = sum (toList xs)
foo :: (Listable a, Ord a) => a -> a -> Bool --a have to be an instance of 2 classes which is Ord (govern >< and Listable)
foo xs1 xs2 = sum(toList xs1) == sum (toList xs2) && xs1 < xs2
{-
First-Class functions
-functions can be stored in variables
-fn's can be passed as arguments and returned by functions
x = function
-}
doubleAll :: [Int] -> [Int]
doubleAll xs = [2*x | x <- xs]
tripleAll :: [Int] -> [Int]
tripleAll [] = []
tripleAll (x:xs) = (3 * x) : (tripleAll xs)
double :: Int -> Int
double x = 2 * x
triple :: Int -> Int
triple x = 3 * x
mapInteger :: (Int -> Int) -> [Int] -> [Int] -- (Int->Int) is a function
mapInteger _ [] = []
mapInteger f (x:xs) = f x :: mapInteger f xs
{-
mapping
1) name a function

-}