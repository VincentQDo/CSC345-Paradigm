data Shape = Circle Float | Rect Float Float
isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rect _ _) = False
c1 :: Shape
c1 = Circle 8.3
--Second
data Person = Person String Int | People String
 deriving Show
student1 :: Person
student1 = Person "Ted" 6
getAge :: Person -> Int
getAge (Person _ n) = n
--Third Exmple
--data Bool = True | False (same as this)
--data Maybe a = Just a| Nothing (this is already defined in prelude)
{-Just will get a to tag along. This Maybe data type will create a type safe function to avoid exceptions or errors
for example head [] will throw an exception, or div 5 0 will also throw exception divide by 0. now we want to make a new functions
called safehead which will avoid the empty list error-}
safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:xs) = Just x
safehead xs = Just (head xs)

getIntSafe :: Maybe Int -> Int
getIntSafe (Just x) = x
getIntSafe (Nothing) = 0

safehead' :: [Int] -> Int
safehead' [] = getIntSafe (Nothing)
safehead' (x:xs) = getIntSafe (Just x)

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv a b = Just (div a b)
