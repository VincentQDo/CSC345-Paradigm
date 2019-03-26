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
      testCase "test1e" $ assertEqual [] [] (reverse' ""),

      testCase "test2a" $ assertEqual [] False (isPalindrome "mad"),
      testCase "test2b" $ assertEqual [] True (isPalindrome "madam"),
      testCase "test2c" $ assertEqual [] True (isPalindrome "aaa"),

      testCase "test3a" $ assertEqual [] Nothing (safeFindAfter "mad" []),
      testCase "test3b" $ assertEqual [] Nothing (safeFindAfter "Given" ["Test", "Hello", "mad"]),
      testCase "test3b" $ assertEqual [] (Just []) (safeFindAfter "mad" ["Test", "Hello", "mad"]),
      testCase "test3b" $ assertEqual [] (Just ["Hello", "mad"]) (safeFindAfter "Test" ["Test", "Hello", "mad"]),

      testCase "test4a" $ assertEqual [] True (member 1 (Set [1,2,3])),
      testCase "test4b" $ assertEqual [] False (member 1 (Set [])),

      testCase "test5a" $ assertEqual [] 3 (size (Set [1,2,3])),
      testCase "test5b" $ assertEqual [] 0 (size (Set [])),

      testCase "test6a" $ assertEqual [] (Set [3,2,5]) (add 2 (Set [3,2,5])),
      testCase "test6b" $ assertEqual [] (Set [3,2,5,1]) (add 1 (Set [3,2,5])),

      testCase "test7a" $ assertEqual [] (Just 5) (safeRemoveMax (Set [1,2,5,4])),
      testCase "test7b" $ assertEqual [] Nothing (safeRemoveMax (Set [])),

      testCase "test8a" $ assertEqual [] True (equal (Set [1,2,3]) (Set [1,2,3])),
      testCase "test8b" $ assertEqual [] False (equal (Set [1,2,3]) (Set [1,2,3,4])),
      testCase "test8c" $ assertEqual [] False (equal (Set [1,2,3]) (Set [1,2,5])),
      testCase "test8d" $ assertEqual [] True (equal (Set [1,2,3]) (Set [1,3,2])),


      testCase "test9a" $ assertEqual [] (Set [1,2,3,4,5,6,7]) (union (Set [1,2,3,4,5]) (Set [4,5,6,7])),
      testCase "test9b" $ assertEqual [] (Set [1,2,3,4,5,6,7]) (union (Set [1,2,3]) (Set [4,5,6,7])),
      testCase "test9c" $ assertEqual [] (Set [1,2,3]) (union (Set [1,2,3]) (Set [])),
      testCase "test9d" $ assertEqual [] (Set [1,2,3]) (union (Set []) (Set [1,2,3])),
      testCase "test9e" $ assertEqual [] EmptySet (union EmptySet EmptySet),
      testCase "test9f" $ assertEqual [] (Set [1,2,3]) (union EmptySet (Set [1,2,3])),
      testCase "test9g" $ assertEqual [] (Set [1,2,3]) (union (Set [1,2,3]) EmptySet),
      testCase "test9h" $ assertEqual [] EmptySet (union (Set []) (Set [])),

      testCase "test10a" $ assertEqual [] (Set [3]) (intersection (Set [1,2,3]) (Set [3,4,5])),
      testCase "test10b" $ assertEqual [] EmptySet (intersection (Set []) (Set [])),
      testCase "test10c" $ assertEqual [] EmptySet (intersection EmptySet EmptySet),
      testCase "test10d" $ assertEqual [] EmptySet (intersection EmptySet (Set [1,2,3])),
      testCase "test10h" $ assertEqual [] EmptySet (intersection (Set []) (Set [1,2,3])),
      testCase "test10e" $ assertEqual [] EmptySet (intersection (Set [1,2,3]) EmptySet),
      testCase "test10g" $ assertEqual [] EmptySet (intersection (Set [1,2,3]) (Set []))


  ]
