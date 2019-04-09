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
sumpositives tree = sum [x | x <- postorder tree]

-- 4. Write a function countInteriorNodes that returns the number of interior nodes in the given tree.
countInteriorNodes :: Tree1 -> Int
countInteriorNodes (Leaf1 n) =  0
countInteriorNodes (Node1 left r right) = 1 + countInteriorNodes left + countInteriorNodes right

-- 5.Write a function depth that returns the depth of a tree. (A tree with only a root node is defined to have depth=1.)
depth :: Tree1 -> Int
depth (Leaf1 n) = 1
depth tree = case tree of
             Node1 (Leaf1 l1) n1 (Leaf1 l2) -> 2
             Node1 n2 root (Leaf1 l2)       -> 1 + depth n2
             Node1 (Leaf1 l2) root n2       -> 1 + depth n2
             Node1 n2 root n3               -> 1 + maximum [depth n2, depth n3]

data Tree2 a = Leaf2 a
             | Node2 [Tree2 a]

occurs :: Eq a => a -> Tree2 a -> Bool
occurs n (Leaf2 x) = n == x
occurs n (Node2 []) = False
occurs n (Node2 (x:xs)) = case n == x of
  True -> True
  False -> occurs n (Node2 xs)
