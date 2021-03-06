{- ##################################
Vincent Do
Units Tests for Homework 8.

Usage: ghci Test8; main
First time user type in the command below
      sudo cabal install tasty
      sudo cabal install tasty-hunit

   ################################## -}


   import Prog8
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
   
   intList = [(-1),(-2),(-3),4]
   intList2 :: [Int]
   intList2 = []
   intList3 = [0,2,3,1,4]
   intList4 = [0,(-1)]
   intList5 = [(-1),(-2),(-3),4,5,6]
   intList6 = [(-1),(-3)]
   intList7 = [1,2,2]
   
   stringList :: [String]
   stringList = ["hello", "hi", "long", ""]

   unitTests = testGroup "Unit tests"
     [
   
         testCase "test1a" $ assertEqual [] 14 (sumSqNeg intList),
         testCase "test1b" $ assertEqual [] 0 (sumSqNeg intList2),
         testCase "test1c" $ assertEqual [] 0 (sumSqNeg intList3),
         testCase "test1d" $ assertEqual [] 1 (sumSqNeg intList4),

         testCase "test2a" $ assertEqual [] True (containing intList intList5),
         testCase "test2b" $ assertEqual [] False (containing intList5 intList),
         testCase "test2c" $ assertEqual [] True (containing intList2 intList5),
         testCase "test2d" $ assertEqual [] True (containing intList2 intList2),
         testCase "test2e" $ assertEqual [] False (containing intList5 intList2),

         testCase "test3a" $ assertEqual [] (-4) (total (*2) intList),
         testCase "test3b" $ assertEqual [] 0 (total (*2) intList2),
         testCase "test3c" $ assertEqual [] 20 (total (*2) intList3),
         testCase "test3d" $ assertEqual [] (-2) (total (*2) intList4),
         testCase "test3e" $ assertEqual [] 18 (total (*2) intList5),

         testCase "test4a" $ assertEqual [] True (containing' intList intList5),
         testCase "test4b" $ assertEqual [] False (containing' intList5 intList),
         testCase "test4c" $ assertEqual [] True (containing' intList2 intList5),
         testCase "test4d" $ assertEqual [] True (containing' intList2 intList2),
         testCase "test4e" $ assertEqual [] False (containing' intList5 intList2),

         testCase "test5a" $ assertEqual [] [5,2,4,0] (lengths stringList),
         testCase "test5b" $ assertEqual [] [] (lengths []),

         testCase "test6a" $ assertEqual [] (-24) (product' intList),
         testCase "test6c" $ assertEqual [] 0 (product' intList3),
         testCase "test6d" $ assertEqual [] 0 (product' intList4),
         testCase "test6e" $ assertEqual [] (-720) (product' intList5),

         testCase "test7a" $ assertEqual [] 4 (max' intList),
         testCase "test7c" $ assertEqual [] 4 (max' intList3),
         testCase "test7d" $ assertEqual [] 0 (max' intList4),
         testCase "test7e" $ assertEqual [] 6 (max' intList5),
         testCase "test7f" $ assertEqual [] (-1) (max' intList6),

         testCase "test8a" $ assertEqual [] [(-1),(-2),(-3),4] (append' intList intList2),
         testCase "test8b" $ assertEqual [] [(-1),(-2),(-3),4,0,2,3,1,4] (append' intList intList3),

         testCase "test9a" $ assertEqual [] [(-2),(-3),4] (filterFirst even intList),
         testCase "test9b" $ assertEqual [] [] (filterFirst even intList2),
         testCase "test9c" $ assertEqual [] [(-3)] (filterFirst even intList6),
         testCase "test9d" $ assertEqual [] [2,2] (filterFirst even intList7),
         testCase "test9e" $ assertEqual [] [(-2),(-3),4,5,6] (filterFirst even intList5),
         testCase "test9d" $ assertEqual [] [1,2] (filterFirst (not.even) intList7),

         testCase "test10a" $ assertEqual [] [(-1),(-2),4] (filterLast even intList),
         testCase "test10b" $ assertEqual [] [] (filterLast even intList2),
         testCase "test10c" $ assertEqual [] [2,2] (filterLast even intList7),
         testCase "test10d" $ assertEqual [] [(-1)] (filterLast even intList6),
         testCase "test10e" $ assertEqual [] [(-1),(-2),(-3),4,6] (filterLast even intList5),
         testCase "test10d" $ assertEqual [] [(-1),(-3)] (filterLast (not.even) intList6)


     ]