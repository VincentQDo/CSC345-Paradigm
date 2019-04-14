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
preorder (Node1 left r right) = [r] ++ preorder left ++ preorder right --this will return the value of the node we are on and then get the value of the left and right child of that node

--2. Write a function postorder that takes a tree argument and returns as a list an inorder traversal of the tree.
postorder :: Tree1 -> [Int]
postorder (Leaf1 n) = [n]
postorder (Node1 n2 root n3) =  postorder n2 ++ postorder n3 ++ [root]--same as before but just different order

--3. Write a function sumPositives that takes a tree argument and returns the sum of positive integers in the tree.
sumPositives :: Tree1 -> Int
sumPositives tree = sum [x | x <- postorder tree, x > 0]--reuse one of the previous function that return the tree as a list of Int and just sum all the value up if its a positive number

-- 4. Write a function countInteriorNodes that returns the number of interior nodes in the given tree.
countInteriorNodes :: Tree1 -> Int
countInteriorNodes (Leaf1 n) =  0
countInteriorNodes (Node1 left r right) = 1 + countInteriorNodes left + countInteriorNodes right --add 1 everytime we call countInteriorNodes, this method will only add 1 if it is not on a leaf node, if it is on a leaf node which is the base case for this function then it will simply return 0 and stop the recursion

-- 5. Write a function depth that returns the depth of a tree. (A tree with only a root node is defined to have depth=1.)
depth :: Tree1 -> Int
depth (Leaf1 n) = 1
depth (Node1 n2 r n3) = 1 + maximum [depth n2, depth n3]--this method will keep ultilize maximum to decide if the left or right node have a bigger depth then it will return that plus 1 for the root node
{- #####################---EXAMPLE END HERE---#####################
TREE CREATION:
tree1 = (Leaf1 2) 1 (tree1Node2)
tree1Node2 = tree1Node2Node1 3 (Leaf1 5)
tree1Node2Node1 = (Leaf1 6) 4 (Leaf1 7)
FUNCTION START:
>depth tree1
1 + maximum [depth Leaf1 2, depth tree1Node2]
1 + maximum [1, (1 + maximum [depth tree1Node2Node1, depth Leaf1 5])]
1 + maximum [1, (1 + maximum [(1 + maximum [depth Leaf1 6, depth Leaf1 7]), 1])] 
1 + maximum [1, (1 + maximum [(1 + maximum [1, 1), 1])] 
1 + maximum [1, (1 + maximum [1 + 1, 1])] 
1 + maximum [1, (1 + maximum [2, 1])] 
1 + maximum [1, (1 + 2)] 
1 + maximum [1, 3] 
1 + 3 
4 
   #####################---EXAMPLE END HERE---##################### -}
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
sumTree (Node2 (x:xs)) = sumTree x + sumTree (Node2 (xs))--get the value of each nodes in the tree in postorder fashion
--9. Write a function post2 that returns a postorder traversal of the nodes in the tree.
post2 :: Tree2 a -> [a]
post2 (Leaf2 n) = [n]
post2 (Node2 []) = []
post2 (Node2 (x:xs)) =  post2 x ++ post2 (Node2 xs)--return every children nodes of the tree from left to right
--10. Write a function depthK that returns all nodes that are at depth k in the tree. (A tree with only a root node is defined to have depth=1.) The order that the nodes are returned does not matter.
depthK :: Int -> Tree2 a -> [a]
depthK 1 (Leaf2 m) = [m] --this base case will return the value of the leaf node if we are at the depth we want
depthK 1 (Node2 xs) = [] --this case will return an empty list if we are at the correct depth and we are on an interior node
depthK n (Leaf2 m) = [] --this case is for when we are not yet at the depth we want so we will return empty list
depthK n (Node2 []) = [] --this case is here to stop the recursion from going if we are not at the depth we want yet and there are nothing left to check
depthK n (Node2 (x:xs)) = depthK (n-1) x ++ depthK (n) (Node2 xs) -- depthK (n-1) x will check on the first child node of the root, we will call this n1, then it will keep checking the children of n1 until it reach a base case
--depthK (n) (Node2 xs) will be responsible for checking on the rest of the children nodes of the root