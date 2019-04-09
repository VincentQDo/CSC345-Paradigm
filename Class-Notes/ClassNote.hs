--Class note today

data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Eq a => a -> Tree a -> Bool --a need to be a member of the Eq class
occurs x (Leaf y) = x == y
occurs x (Node left y right) = x == y || occurs x left || occurs x right --what does || do again?

flatten :: Tree a -> [a] --will produce a inorder traversal
flatten (Leaf a) = [a]
flatten (Node left r right) = flatten left ++ [r] ++ flatten right

-- data Tree1 a = Leaf1 a | Node1 (Tree1 a)(Tree1 a) --data only on leaf in this tree
-- data Tree2 a = Leaf2 | Node2 (Tree1 a) a (Tree1 a) --data only in interior node in this tree
-- data Tree3 a b = Leaf3 b | Node3 (Tree1 a b) a (Tree1 a b) --multiple parameterized data type on Tree3
-- data Tree4 a = Leaf4 a | Node4 [Tree4 a]

data Nat = Zero | Succ Nat
-- this data type would produce
-- 0 = Zero
-- 1 = Succ Zero
-- 2 = Succ Succ Nat
-- 3 = Succ Succ Nat

nat2Int :: Nat -> Int --maybe usefull for the final
nat2Int Zero = 0
nat2Int (Succ n) = 1 + nat2Int (n)
