{- ##################################
Vincent Do
Units Tests for Homework 5.

Usage: ghci Test5.hs; main
First time user type in the command below
      sudo cabal install tasty
      sudo cabal install tasty-hunit

   ################################## -}


import Prog5
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


unitTests = testGroup "Unit tests"
  [


      testCase "test1a" $ assertEqual [] "olleh" (reverse' "hello"),
      testCase "test1b" $ assertEqual [] "madam" (reverse' "madam"),
      testCase "test1c" $ assertEqual [] [4,3,2,1] (reverse' [1,2,3,4]),
      testCase "test1d" $ assertEqual [] "aaa" (reverse' "aaa"),

      testCase "test2a" $ assertEqual [] False (isPalindrome "mad"),
      testCase "test2b" $ assertEqual [] True (isPalindrome "madam"),
      testCase "test2c" $ assertEqual [] True (isPalindrome "aaa"),

      testCase "test3a" $ assertEqual [] Nothing (safeFindAfter "mad" []),
      testCase "test3b" $ assertEqual [] (Just []) (safeFindAfter "mad" ["Test", "Hello", "mad"]),
      testCase "test3b" $ assertEqual [] (Just ["Hello", "mad"]) (safeFindAfter "Test" ["Test", "Hello", "mad"]),


      testCase "test4a" $ assertEqual [] True (member 1 (Set [1,2,3])),
      testCase "test4b" $ assertEqual [] False (member 1 (Set []))


      -- testCase "test5a" $ assertEqual [] 3 (size [1,2,3]),
      --
      -- testCase "test6a" $ assertEqual [] [3,2,5] (add 2 [3,2,5]),
      --
      -- testCase "test7a" $ assertEqual [] [1,2,4] (safeRemoveMax [1,2,5,4]),
      --
      -- testCase "test8a" $ assertEqual [] True (equal [1,2,3] [1,2,3]),
      --
      -- testCase "test9a" $ assertEqual [] [1,2,3,4,5] (union [1,2,3] [3,4,5]),
      --
      -- testCase "test10a" $ assertEqual [] [3] (intersection [1,2,3] [3,4,5])

  ]
