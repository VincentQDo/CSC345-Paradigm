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
mapInteger f xs = [f x| x<-xs]
sixteen :: [Int]
sixteen = mapInteger double [3,5]

{-mapping property is already defined in haskell, it is simply called map
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs
filter :: (a -> b) -> [a] -> [b]
filter _ [] = []
filter p (x:xs)
      |p x       = x : filter p xs
      |otherwise = filter p xs
filter p xs = [x | x <- xs, p x] --either 132 or 129 not both
-}

[[Int]] => [[1,2,3],[4,5]]
map (map double) [[1,2,3],[4,5]]
--work outside in, apply outter map
[map double [1,2,3], map double [4,5]]
--apply the innter map
[[2,4,6],[8,10]]
--higher order function
-- >map :: apply a given function to each item in a list
-- >filter :: select all element from a list that satisfy a given predicate
-- digits xs = [x | x<-xs, isDigit x]
-- digits xs = filter isDigit xs --either 145 or 144, 145 uses higher order function which is better tha nlist comp because it already is implemented in list comp.


-- April 18 2019
--map and filter are higher level function
sumDoubleEven :: [Int] -> Int
sumDoubleEven xs = sum (map double (filter isEven xs)) --filter will return all the nubmer that is even, map will apply double to every number in the list and return that list, then we sum up all the elements.

--zipWith is also a higher order function. this will take two lists and pattern patch them the same way with zip and it will also take a function as a first argument.
zipWith f (x:xs) (y:xs) = f x y : zipWith f xs ys
zipWith f _ _ = []



foo :: Int -> Char -> Bool
foo n _ = n == 1
zipWith foo [1,2,3,4] ['a','B','c']
[True, False, False]

--zipWith :: (a -> b -> c) -> [a] -> [b] -> [c] like zip but apply a function to each of the element first then return the result of that function basically zip with a function
--map :: (a -> b) -> [a] -> [b]
--filter :: (a -> Bool) -> [a] -> [a]
--all :: Foldable t => (a -> Bool) -> t a -> Bool similar with any but like an and
--any :: Foldable t => (a -> Bool) -> t a -> Bool just like an or function as long as anything in the function is false return false but apply the predicate to each element first
--takeWhile :: (a -> Bool) -> [a] -> [a] take any item that satisfy the predicate until u meet an item that does not.
-- >takeWhile even [2,4,6,7,8] -> [2,4,6]
--dropWhile :: (a -> Bool) -> [a] -> [a] like take while but the opposite
-- >dropWhile even [2,4,6,7,8] -> [7,8]

{-
Folding chapter 10.3 in the thompson textbook

there is a function built in called foldr
foldr stand for fold right
in functional languages there is a concept call folding
operations of folding

some example leading up to folding:
sum [] = 0
sum (x:xs) = x + sum xs

product [] = 1
produc (x:xs) = x * product xs

or [] = False
or (x:xs) = x || or xs

and [] = True
and (x:xs) = x && and xs

the 4 functions above are very similar in patterns and structures, the only thing that is different is the base case and the functions inside
foldr will allow us to mash these 4 functions

leading up to folding now
this is folding.
f [] = v
f (x:xs) = x # f xs  --read this as: grab the head and then apply some operator # then call the function again on the tail of the list

3 things are necessary here:
1. we need to define the operator #
2. need ot define the base case value v
3. need the list of value to apply the function to. the 3rd idea we will relax next week

some example are starting now:

before we look at the definition of foldr we want to use it now:

sum :: Num a => [a] -> a --Num class include Double Float Integer
sum xs = foldr (+) 0 xs  --this read we will take foldr  and pass to it the operator + (plus is in fix so that is why it have parenthesis around it) and the base case 0 and the list xs
sum xs = foldr add 0 xs

or :: [Bool] -> Bool
or xs = foldr (||) False xs

and :: [Bool] -> Bool
and xs = foldr (&&) True xs

product :: Num a => [a] -> a
product xs = foldr (*) 1 xs


definition of foldr:

foldr f v [] = v
foldr f v (x:xs) = f x (foldr f v xs) --not a cons operator like we are used to with recursion


sum xs = foldr (+) 0 xs
sum [1,2,3] tracing
foldr (+) 0 x:xs

1 + (foldr (+) 0 [2,3])
1 + (2 + (foldr + 0 [3]))
1 + (2 + (3 + (foldr + 0 [])))
1 + (2 + (3 + (0)))


-}

--want ot write a function that will return the sum of all of the squares of positive numbers.

--currying
f :: Int -> Int -> Int --represented in curry form which is named after one of the mathematician Haskell Curry which derive lambda calculus
--big idea, every function in haskell takes one and only one argument.
>f 3 5

add :: Int -> Int -> Int
add x y = x + y
--under the hood it is the type
add :: Int -> (Int -> Int)
add x = g
  where
    g y = x + y
--(add 3) would return the "3+" function then 3+ would take in argument 4 and return 7
-- so add would take in an Int then return (Int -> Int)

-- -> is right asscoiated
-- function application a is left asscoiated

-- add 3 theoratically make sense by itself
so theoratically we can make
belong :: a -> b
belong (x:xs) = belong'
  where
    belong' ys = x elem ys

curried form
add :: Int -> Int -> Int
add x y = x + y
1 args fn
return fn if there are multi args

uncarried form
add :: (Int, Int) -> Int
add (x,y) = x + y

both forms are possible but function would elect to go with the curry form.

aprtial application of add
add 3 will have the type Int -> Int
we can combine this with map
map (add 3) xs
this would take the list xs and then add 3 to each element in xs
very important down below
****************************
addThree :: [Int] -> [Int]
addThree = map (add 3) --this will add 3 to each element of the incoming list
****************************


sum xs = foldr (+) 0 xs
sum = foldr (+) 0
--296 and 295 are the same thing

--lambda expression
f :: [Int] -> Int
f xs = foldr (+) 0 (map sqr (filter pos xs))
  where
    sqr x = x * x
    pos x = x > 0
--now convert it to lambda expression
--we will be coming up with nameless function
addOne x = x + 1 --normal
\x = x + 1 --lambda
308 and 307 are the same thing

--translating 300-304 to below:
f xs = foldr (+) 0 (map (\x -> x*x) (filter (\x -> x>0) xs))
--even shorter
f xs = foldr (+) 0 (map (^2) (filter (>0) xs))

evaluation of lambda expression
(\x -> x > 0) 3
let x = 3 in x > 0
3 > 0
True

(\x -> \y -> x + y) 3 4
(let x = 3 in (\y -> x + y)) 4
(\y -> 3 + y) 4
let y = 4 in (\y -> 3 + y)
3 + 4

\x ->  x > 0 can also be written as (>0)
\x ->  x+1 can also be written as (+1)

add x y = x + y
add = \x -> (\y -> x + y)
--331-332 are the same
const x _ = x
const x = (\_ -> x)
const = \x -> (\_ -> x)
--334-336 are all the same

potential final question
**********************************
odds :: Int -> [Int]
--will return first n odd ints
>odds 3 should return 1,3,5
--answer would be
odds x = map (\n -> 2*n+1) [0..n-1]
or
odds x = map (+1).(*2) [0..x-1]


function composition
the '.' operator will pass the result of one function to another
pos (sqr 3) can be rewrite as (pos.sqr) 3
pos 9
True

(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x) 
(.) f g = \x -> f (g x)

possqr :: Int -> Bool
possqr x = pos (sqr x)
possqr = pos.sqr

f.g.h
h is applied first then g then f

f xs = foldr (+) 0 (map (^2) (filter (>0) xs))
f = foldr (+) 0 . map(^2) . filter (>0)

there is a second composition operator 
(>.>) :: (a->b) -> (b->c) -> (a->c)
f>.>g = g.f --syntactic sugar to write it in a different order

Strict evaluation is java
while haskell use lazy evaluation

strict evaluation: any argument you want to pass to a function will be evaluate completely before being pass to a fcuntion
strict eval will eval any parameter even if we dont use it
for example

f x y= x + 5
>f 4 (5^27928374982) this will waste a lot of time trying to eval the second argument

strict:
>inc (2*3)
{apply multi}
inc 6
7

lazy:
>inc (2*3)
{apply inc}
(2*3) + 1
6 + 1
7


it is much easier to predict order of strict eval but you may be doing some unneeded works.

n = 0


n + (n = 1) --imperitive expression, set n to 1 then return value of n
{applying n} --which then return 0 for first n
0 + (n = 1)
{apply the assignment imperitive expression which set n to 1 then return value of n}
0 + 1
1
java will prob choose this implementation instead
n + (n = 1) --n is still value of 0
{apply imperitive expression which change value of n to 1 then return 1}
n + 1
{apply n, look up the value of n in memory which is 1 now}
1 + 1
2


in a language with side effect, strict is what you want
haskell have no side effect so that is why we can do lazy eval

lazy eval: evaluation of fn argss, delay as long as possible args won't be eval until neccessary, fn args, internally represented as "unevaluate expr"

f1 n = (n,n)
f1 n = (n,n)
>f1 (2+2) --lazy eval wont have to do 2+2 but strict does
>f1 9

f m n p
  | m >= n = m --p can be untouch
  | otherwise = p --p can be unevaluated you dont have to evaluate it in lazy eval

f m n
  |notNil xs = front xs
  |otherwise = n
  where
    xs = [m..n]
front (x:y:zs) = x + y
front [x] = x
notNil [] = False
notNil (_:_) = True

f 3 5
1. notNil xs
  1a. xs = [3..5] --partially unevaluated
2. notNil [3..5]
  2a. pattern match [3..5] did not match anything so we eval it to (3:[4..5])
  2b. True
3. fn calls to front xs which is front (3:[4..5])
  3a. (3:[4..5]) it does not match so we have to eval it more which is (3:4:[5]) we dont have to eval [5]
  3b. 3+4 not 7 because we dont need to eval it yet


evaluation order
both f and g will take 2 integer arguments g will return an int

>f 3 (g 1 2)
java going to do inside out so figure out g then apply f

haskell apply outside in which is aplly f first then apply g

left to right
(f 1 2) + (f 1 3)
(1*2) + (f 1 3)
2 + (f 1 3)
2 + (1*3)
2 + 3
5

haskell is outside in and left to right

no duplicated expressions in haskell

h (9-3) (h 34 3)
(9-3) + (9-3)
6 + 6 --did the first one and that be come 6 the seond one is automatically become 6 as well so we dont have to redo the work