{- ##################################
Vincent Do
Units Tests for Homework 6.

Usage: ghci Test6; main
First time user type in the command below
      sudo cabal install tasty
      sudo cabal install tasty-hunit

   ################################## -}


import Prog6
import Test.Tasty
import Test.Tasty.HUnit
import System.Environment


main = do
    setEnv "TASTY_TIMEOUT" "2s"
    defaultMain tests

{-
main = defaultMain tests
-}

tests :: TestTree
tests = testGroup "Tests" [unitTests]

ts :: [Tree1]
ts = [Leaf1 0, Node1 (Leaf1 3) 4 (Leaf1 6), Node1 (Node1 (Leaf1 4) 2 (Leaf1 5)) 1 (Leaf1 (-3)), Node1  (Leaf1 3) 1 (Node1 (Leaf1 4) 2 (Leaf1 5)), Node1  (Node1 (Leaf1 4) 2 (Leaf1 5)) 1 (Node1 (Leaf1 6) 3 (Leaf1 7)), Node1  (Node1 (Leaf1 4) 2 (Node1 (Leaf1 (-6)) 5 (Leaf1 7))) 1 (Node1 (Leaf1 0) 3 (Leaf1 5)), Node1 (Leaf1 1) 2 (Node1 (Leaf1 7) 5 (Leaf1 8))]
ts' :: [Tree2 Int]
ts' = [Leaf2 0, Node2 [], Node2 [Leaf2 2, Leaf2 3, Leaf2 4, Node2 [Leaf2 5, Leaf2 6]], Node2 [Node2 [Leaf2 5, Leaf2 6], Leaf2 2, Leaf2 3, Leaf2 4]]

testTree :: Tree1
testTree = Node1 (Leaf1 7) 5 (Leaf1 8)
testTree3 :: Tree1
testTree3 = Node1 (Leaf1 1) 2 testTree

testTree2 :: (Num a) => Tree2 a
testTree2 = Node2 [testTree2Node1, testTree2Node2, testTree2Node3]


testTree2Node1 :: (Num a) =>  Tree2 a
testTree2Node1 = Leaf2 1


testTree2Node2 :: (Num a) =>  Tree2 a
testTree2Node2 = Node2 [Node2 [(Leaf2 2), (Leaf2 3)], Leaf2 4]


testTree2Node3 :: (Num a) =>  Tree2 a
testTree2Node3 = Node2 [ Node2 [ Node2 [ (Leaf2 5), Leaf2 6, Leaf2 7], ( Leaf2 8 ), Leaf2 9, Node2 [Leaf2 10, Leaf2 11, Leaf2 12]]]
unitTests = testGroup "Unit tests"
  [
      testCase "test1a" $ assertEqual [] [0] (preorder (ts !! 0)),
      testCase "test1b" $ assertEqual [] [4,3,6] (preorder (ts !! 1)),
      testCase "test1c" $ assertEqual [] [1,2,4,5,-3] (preorder (ts !! 2)),
      testCase "test1d" $ assertEqual [] [1,3,2,4,5] (preorder (ts !! 3)),
      testCase "test1e" $ assertEqual [] [1,2,4,5,3,6,7] (preorder (ts !! 4)),
      testCase "test1f" $ assertEqual [] [1,2,4,5,-6,7,3,0,5] (preorder (ts !! 5)),

      testCase "test2a" $ assertEqual [] [0] (postorder (ts !! 0)),
      testCase "test2b" $ assertEqual [] [3,6,4] (postorder (ts !! 1)),
      testCase "test2c" $ assertEqual [] [4,5,2,-3,1] (postorder (ts !! 2)),
      testCase "test2d" $ assertEqual [] [3,4,5,2,1] (postorder (ts !! 3)),
      testCase "test2e" $ assertEqual [] [4,5,2,6,7,3,1] (postorder (ts !! 4)),
      testCase "test2f" $ assertEqual [] [4,-6,7,5,2,0,5,3,1] (postorder (ts !! 5)),

      testCase "test3a" $ assertEqual [] 0 (sumpositives (ts !! 0)),
      testCase "test3b" $ assertEqual [] 13 (sumpositives (ts !! 1)),
      testCase "test3c" $ assertEqual [] 9 (sumpositives (ts !! 2)),
      testCase "test3d" $ assertEqual [] 15 (sumpositives (ts !! 3)),
      testCase "test3e" $ assertEqual [] 28 (sumpositives (ts !! 4)),
      testCase "test3f" $ assertEqual [] 21 (sumpositives (ts !! 5)),

      testCase "test4a" $ assertEqual [] 0 (countInteriorNodes (ts !! 0)),
      testCase "test4b" $ assertEqual [] 1 (countInteriorNodes (ts !! 1)),
      testCase "test4c" $ assertEqual [] 2 (countInteriorNodes (ts !! 2)),
      testCase "test4d" $ assertEqual [] 2 (countInteriorNodes (ts !! 3)),
      testCase "test4e" $ assertEqual [] 3 (countInteriorNodes (ts !! 4)),
      testCase "test4f" $ assertEqual [] 4 (countInteriorNodes (ts !! 5)),

      testCase "test5a" $ assertEqual [] 1 (depth (ts !! 0)),
      testCase "test5b" $ assertEqual [] 2 (depth (ts !! 1)),
      testCase "test5c" $ assertEqual [] 3 (depth (ts !! 2)),
      testCase "test5d" $ assertEqual [] 3 (depth (ts !! 3)),
      testCase "test5e" $ assertEqual [] 3 (depth (ts !! 4)),
      testCase "test5f" $ assertEqual [] 4 (depth (ts !! 5)),

      testCase "test6a" $ assertEqual [] True (occurs 0 (ts' !! 0)),
      testCase "test6b" $ assertEqual [] False (occurs 3 (ts' !! 1)),
      testCase "test6c" $ assertEqual [] True (occurs 3 (ts' !! 2)),

      testCase "test7a" $ assertEqual [] 1 (countLeaves (ts' !! 0)),
      testCase "test7b" $ assertEqual [] 0 (countLeaves (ts' !! 1)),
      testCase "test7c" $ assertEqual [] 5 (countLeaves (ts' !! 2)),

      testCase "test8a" $ assertEqual [] 0 (sumTree (ts' !! 0)),
      testCase "test8b" $ assertEqual [] 0 (sumTree (ts' !! 1)),
      testCase "test8c" $ assertEqual [] 20 (sumTree (ts' !! 2)),

      testCase "test9a" $ assertEqual [] [0] (post2 (ts' !! 0)),
      testCase "test9b" $ assertEqual [] [] (post2 (ts' !! 1)),
      testCase "test9c" $ assertEqual [] [2,3,4,5,6] (post2 (ts' !! 2)),
      testCase "test9d" $ assertEqual [] [5,6,2,3,4] (post2 (ts' !! 3)),
      testCase "test9e" $ assertEqual [] [1,2,3,4,5,6,7,8,9,10,11,12] (post2 testTree2)



  ]
