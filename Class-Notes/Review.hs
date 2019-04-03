append' :: [a] -> [a] -> [a]
append' [] [] = []
append' [] ys = ys
append' xs [] = xs
append' (x:xs) (y:ys) = x : append' (xs) (y:ys)

selectEven :: [Int] -> [Int]
selectEven xs = [x | x <- xs, x `mod` 2 == 0 ]

selectEven' :: [Int] -> [Int]
selectEven' b = case b of
  []     -> []
  (x:xs) -> case x `mod` 2 of
             0 -> x : selectEven' xs
             _ -> selectEven' xs

elemNum :: Int -> [Int] -> Int
elemNum n xs = length [1 | x <- xs, x == n]

elemNum' :: Int -> [Int] -> Int
elemNum' n [] = 0
elemNum' n (x:xs) = case x == n of
  True -> length (elemNum' n xs : [1])
  False -> length (elemNum' n xs : [])

data Shape = Triangle (Int,Int,Int)| Rectangle (Int,Int) | Circle Int
perimeter :: Shape -> Maybe Int
perimeter (Triangle (x,y,z)) = Just (x+y+z)
perimeter (Rectangle (x,y)) = Just (x+y)
perimeter (Circle _) = Nothing
