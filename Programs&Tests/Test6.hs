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

--Creditted to DJ for this test tree
testTree2 = Node2 [Leaf2 1, testTree2Node2, testTree2Node3]
testTree2Node2 = Node2 [testTree2Node2Node1, Leaf2 4]
testTree2Node2Node1 = Node2 [(Leaf2 2), (Leaf2 3)]
testTree2Node3 = Node2 [testTree2Node3Sub]
testTree2Node3Sub = Node2 [testTree2Node3SubNode1, Leaf2 8, Leaf2 9, testTree2Node3SubNode4]
testTree2Node3SubNode1 = Node2 [ (Leaf2 5), Leaf2 6, Leaf2 7]
testTree2Node3SubNode4 =  Node2 [Leaf2 10, Leaf2 11, Leaf2 12]

testTree5 :: (Num a) => Tree2 a
testTree5 = Node2 [Leaf2 1, Node2 [Leaf2 2, Leaf2 3, Leaf2 4], Node2 [Leaf2 5, Node2 [Leaf2 6, Leaf2 7]]]

testTreeDepth :: (Num a) => Tree2 a
testTreeDepth = Node2 [Leaf2 1, Node2 [Leaf2 2, Node2 [Leaf2 3, Node2 [Leaf2 4, Node2 [Leaf2 5]]]]]

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

      testCase "test3a" $ assertEqual [] 0 (sumPositives (ts !! 0)),
      testCase "test3b" $ assertEqual [] 13 (sumPositives (ts !! 1)),
      testCase "test3c" $ assertEqual [] 12 (sumPositives (ts !! 2)),
      testCase "test3d" $ assertEqual [] 15 (sumPositives (ts !! 3)),
      testCase "test3e" $ assertEqual [] 28 (sumPositives (ts !! 4)),
      testCase "test3f" $ assertEqual [] 27 (sumPositives (ts !! 5)),

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
      testCase "test6d" $ assertEqual [] True (occurs 12 testTree2),
      testCase "test6e" $ assertEqual [] False (occurs 13 testTree2),


      testCase "test7a" $ assertEqual [] 1 (countLeaves (ts' !! 0)),
      testCase "test7b" $ assertEqual [] 0 (countLeaves (ts' !! 1)),
      testCase "test7c" $ assertEqual [] 5 (countLeaves (ts' !! 2)),
      testCase "test7d" $ assertEqual [] 7 (countLeaves testTree5),
      testCase "test7e" $ assertEqual [] 12 (countLeaves testTree2),


      testCase "test8a" $ assertEqual [] 0 (sumTree (ts' !! 0)),
      testCase "test8b" $ assertEqual [] 0 (sumTree (ts' !! 1)),
      testCase "test8c" $ assertEqual [] 20 (sumTree (ts' !! 2)),
      testCase "test8d" $ assertEqual [] 78 (sumTree testTree2),


      testCase "test9a" $ assertEqual [] [0] (post2 (ts' !! 0)),
      testCase "test9b" $ assertEqual [] [] (post2 (ts' !! 1)),
      testCase "test9c" $ assertEqual [] [2,3,4,5,6] (post2 (ts' !! 2)),
      testCase "test9d" $ assertEqual [] [5,6,2,3,4] (post2 (ts' !! 3)),
      testCase "test9e" $ assertEqual [] [1,2,3,4,5,6,7,8,9,10,11,12] (post2 testTree2),


      testCase "test10a" $ assertEqual [] [] (depthK 1 testTree2),
      testCase "test10b" $ assertEqual [] [1] (depthK 2 testTree2),
      testCase "test10c" $ assertEqual [] [4] (depthK 3 testTree2),
      testCase "test10d" $ assertEqual [] [2,3,8,9] (depthK 4 testTree2),
      testCase "test10e" $ assertEqual [] [5,6,7,10,11,12] (depthK 5 testTree2),
      testCase "test10f" $ assertEqual [] [] (depthK 5 (ts' !! 0)),
      testCase "test10g" $ assertEqual [] [] (depthK 5 (ts' !! 1)),
      testCase "test10h" $ assertEqual [] [] (depthK 1 (ts' !! 1)),
      testCase "test10i" $ assertEqual [] [0] (depthK 1 (ts' !! 0))

  ]
