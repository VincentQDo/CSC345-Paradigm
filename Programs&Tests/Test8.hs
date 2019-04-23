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
   
   intList :: [Int]
   intList = [(-1),(-2),(-3),4]
   intList2 :: [Int]
   intList2 = []
   intList3 :: [Int]
   intList3 = [0,2,3,1,4]
   intList4 :: [Int]
   intList4 = [0,(-1)]
   intList5 :: [Int]
   intList5 = [(-1),(-2),(-3),4,5,6]
   
   stringList :: [String]
   stringList = ["hello", "hi", "long", ""]
   unitTests = testGroup "Unit tests"
     [
   
         testCase "test1a" $ assertEqual [] (-14) (sumSqNeg intList),
         testCase "test1b" $ assertEqual [] 0 (sumSqNeg intList2),
         testCase "test1c" $ assertEqual [] 0 (sumSqNeg intList3),
         testCase "test1d" $ assertEqual [] (-1) (sumSqNeg intList4),

         testCase "test2a" $ assertEqual [] True (containing intList intList5),
         testCase "test2b" $ assertEqual [] False (containing intList5 intList),
         testCase "test2c" $ assertEqual [] True (containing intList2 intList5),
         testCase "test2d" $ assertEqual [] True (containing intList2 intList2),
         testCase "test2e" $ assertEqual [] False (containing intList5 intList2),

         testCase "test3a" $ assertEqual [] (-30) (total doubleNegNum intList),
         testCase "test3b" $ assertEqual [] 0 (total doubleNegNum intList2)

         testCase "test4a" $ assertEqual [] True (containing' intList intList5),
         testCase "test4b" $ assertEqual [] False (containing' intList5 intList),
         testCase "test4c" $ assertEqual [] True (containing' intList2 intList5),
         testCase "test4d" $ assertEqual [] True (containing' intList2 intList2),
         testCase "test4e" $ assertEqual [] False (containing' intList5 intList2),

         testCase "test5a" $ assertEqual [] [5,2,4,0] (lengths stringList),
         testCase "test5b" $ assertEqual [] [] (lengths [])










     ]
   