{- ##################################
Vincent Do
Units Tests for Homework 3.

Usage: ghci Test3; main
First time user type in the command below
      sudo cabal install tasty
      sudo cabal install tasty-hunit

   ################################## -}


import Prog3
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure
import System.Environment


main = do
    setEnv "TASTY_TIMEOUT" "2s"
    defaultMain tests

{-
main = defaultMain tests
-}

tests :: TestTree
tests = testGroup "Tests" [unitTests]


unitTests = testGroup "Unit tests"
  [


      testCase "test1a" $ assertEqual [] 9 (sumLastPart 3 [1,2,3,4]),
      testCase "test1b" $ assertEqual [] 0 (sumLastPart 0 [3,3,3]),
      testCase "test1c" $ assertEqual [] 10 (sumLastPart 4 [1,2,3,4]),
      testCase "test1d" $ assertEqual [] 0 (sumLastPart 1 []),

      testCase "test2a" $ assertEqual [] [1,2,3] (init' [1,2,3,4]),
      testCase "test2b" $ assertEqual [] [] (init' [1]),
      testCase "test2c fail" $ assertEqual [] [] (init' []),

      testCase "test3a" $ assertEqual [] [1,2,3] (init'' [1,2,3,4]),
      testCase "test3b" $ assertEqual [] [] (init'' [1]),
      testCase "test3c fail" $ assertEqual [] [] (init'' []),

      testCase "test4a" $ assertEqual [] 3 (elemAt 3 [1,2,3]),
      testCase "test4b fail" $ assertEqual [] 3 (elemAt 0 [1,2,3]),
      testCase "test4c fail" $ assertEqual [] 0 (elemAt 3 []),

      testCase "test5a" $ assertEqual [] 0 (numTimes 1 [2,3,4,5]),
      testCase "test5b" $ assertEqual [] 4 (numTimes 1 [1,1,1,1]),
      testCase "test5c" $ assertEqual [] 1 (numTimes 1 [1,2,3,4]),
      testCase "test5d" $ assertEqual [] 0 (numTimes 1 []),

      testCase "test6a" $ assertEqual [] "hELLO" (lowerFirstLetter "HELLO"),
      testCase "test6b" $ assertEqual [] "hello" (lowerFirstLetter "Hello"),
      testCase "test6c" $ assertEqual [] "" (lowerFirstLetter ""),

      testCase "test7a" $ assertEqual [] True (and' [True, True]),
      testCase "test7b" $ assertEqual [] False (and' [True, False]),
      testCase "test7c" $ assertEqual [] False (and' [False, True]),
      testCase "test7d" $ assertEqual [] False (and' [False, False]),
      testCase "test7e" $ assertEqual [] True (and' []),

      testCase "test8a" $ assertEqual [] True (or' [True, True]),
      testCase "test8b" $ assertEqual [] True (or' [True, False]),
      testCase "test8c" $ assertEqual [] True (or' [False, True]),
      testCase "test8d" $ assertEqual [] False (or' [False, False]),
      testCase "test8e" $ assertEqual [] False (or' []),

      testCase "test9a" $ assertEqual [] [] (iSort' []),
      testCase "test9b" $ assertEqual []
              [(1.4, 1, "a"), (7.2, 2, "b"), (1.234, 3, "c"), (5.4, 4, "d")]
      (iSort' [(1.234, 3, "c"), (5.4, 4, "d"), (7.2, 2, "b"), (1.4, 1, "a")]),
      testCase "test9c" $ assertEqual []
              [(1.234, 2, "b"), (1.23, 3, "c"), (9.98, 3, "d")]
      (iSort' [(1.23, 3, "c"), (9.98, 3, "d"), (1.234, 2, "b")]),
      testCase "test9a" $ assertEqual []
              [(1.23, 3, "c"), (9.98, 3, "d"), (1.234, 3, "b")]
      (iSort' [(1.23, 3, "c"), (9.98, 3, "d"), (1.234, 3, "b")]),


      testCase "test10a" $ assertEqual [] [6,5,4,3,2,1] (merge [3,5,2] [4,1,6]),
      testCase "test10b" $ assertEqual [] [2,2,2,1,1,1] (merge [1,1,1] [2,2,2]),
      testCase "test10c" $ assertEqual [] [15,15,9,7,1] (merge [15,7] [9,15,1]),
      testCase "test10d" $ assertEqual [] [3,2,1] (merge [] [1,2,3]),
      testCase "test10e" $ assertEqual [] [3,2,1] (merge [2,1,3] []),
      testCase "test10f" $ assertEqual [] [] (merge [] [])

  ]
