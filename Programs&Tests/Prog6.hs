{- ######################################################
		Prog6.hs
		Vincent Do, April 2019
######################################################## -}
module Prog6 where

data Tree1 = Leaf1 Int
        | Node1 Tree1 Int Tree1

--1. Write a function preorder that takes a tree argument and returns as a list an inorder traversal of the tree.
preorder :: Tree1 -> [Int]
preorder (Leaf1 n) = [n]
preorder (Node1 left r right) = [r] ++ preorder left ++ preorder right

--2. Write a function postorder that takes a tree argument and returns as a list an inorder traversal of the tree.
postorder :: Tree1 -> [Int]
postorder (Leaf1 n) = [n]
postorder (Node1 n2 root n3) =  postorder n2 ++ postorder n3 ++ [root]

--3. Write a function sumPositives that takes a tree argument and returns the sum of positive integers in the tree.
sumpositives :: Tree1 -> Int
sumpositives tree = sum [x | x <- postorder tree, x > 0]

-- 4. Write a function countInteriorNodes that returns the number of interior nodes in the given tree.
countInteriorNodes :: Tree1 -> Int
countInteriorNodes (Leaf1 n) =  0
countInteriorNodes (Node1 left r right) = 1 + countInteriorNodes left + countInteriorNodes right

-- 5. Write a function depth that returns the depth of a tree. (A tree with only a root node is defined to have depth=1.)
depth :: Tree1 -> Int
depth (Leaf1 n) = 1
depth (Node1 n2 r n3) = 1 + maximum [depth n2, depth n3]


data Tree2 a = Leaf2 a
             | Node2 [Tree2 a]

--6. Write a function occurs that returns whether a given argument is present in a given tree.
occurs :: Eq a => a -> Tree2 a -> Bool
occurs n (Leaf2 x) = n == x
occurs n (Node2 []) = False
occurs n (Node2 (x:xs)) = or [occurs n x, occurs n (Node2 xs)]

--7. Write a function countLeaves that takes a tree argument and returns the number of leaves in the tree.
countLeaves :: Tree2 a -> Int
countLeaves (Leaf2 n) = 1
countLeaves (Node2 []) = 0
countLeaves (Node2 (x:xs)) = countLeaves x + countLeaves (Node2 xs)
--8. Write a function sumTree that takes a tree of integers and returns the sum of all integers in the tree.
sumTree :: Tree2 Int -> Int
sumTree (Leaf2 n) = n
sumTree (Node2 []) = 0
sumTree (Node2 (x:xs)) = sumTree x + sumTree (Node2 (xs))
--9. Write a function post2 that returns a postorder traversal of the nodes in the tree.
post2 :: Tree2 a -> [a]
post2 (Leaf2 n) = [n]
post2 (Node2 []) = []
post2 (Node2 (x:xs)) =  post2 x ++ post2 (Node2 xs)
--10. Write a function depthK that returns all nodes that are at depth k in the tree. (A tree with only a root node is defined to have depth=1.) The order that the nodes are returned does not matter.
depthK :: Int -> Tree2 a -> [a]
depthK 1 (Leaf2 m) = [m]
depthK 1 (Node2 xs) = []
depthK n (Leaf2 m) = []
depthK n (Node2 []) = []
depthK n (Node2 (x:xs)) = depthK (n-1) x ++ depthK (n) (Node2 xs)

--testTreeDepth = Node2 [Leaf2 1, Node2 [Leaf2 2, Node2 [Leaf2 3, Node2 [Leaf2 4, Node2 [Leaf2 5]]]]]
--depthk 3 testTreeDepth
--depthk 2 (Leaf2 1) ++ depthk 2 (Node2 [Node2 [Leaf2 2, Node2 [Leaf2 3, Node2 [Leaf2 4, Node2 [Leaf2 5]]]]])
-- [] ++ depthK 1 (Node2 [Leaf2 2, Node2 [Leaf2 3, Node2 [Leaf2 4, Node2 [Leaf2 5]]]] ++ depthK 1 (Node2 []))
-- [] ++ [] ++ []