rev' :: [Int] -> [Int]
rev' [] = []
rev' (head:tail) = rev' tail ++ [head]

(++) :: [Int] -> [Int] -> [Int]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)
