{- ##################################
   Richard Burns
   Units Tests for Homework 5.

   Usage: ghci Prog2Tests; main

   Dependencies: cabal install tasty
                 cabal install tasty-hunit
   ################################## -}


import Prog5
import Test.Tasty
import Test.Tasty.HUnit
import System.Environment

import Data.List hiding (union, intersection)

main = do
    setEnv "TASTY_TIMEOUT" "2s"
    defaultMain tests

{-
main = defaultMain tests
-}

tests :: TestTree
tests = testGroup "Tests" [unitTests]



animals = ["ape", "bear", "cat", "dog", "elephant", "fish"]

s1 = Set [1..5]
s2 = Set [1,3..9]
s3 = Set [2,4..10]
s4 = Set [1..10]
s5 = EmptySet

eqhelper :: Set -> Set -> Bool
eqhelper (Set a) (Set b) = (Data.List.sort a) == (Data.List.sort b)
eqhelper EmptySet EmptySet = True
eqhelper _ _ = False

len :: Set -> Int
len (Set a) = length a
len EmptySet = 0

elemhelp :: Int -> Set -> Bool
elemhelp x (Set a) = elem x a
elemhelp _ EmptySet = False

unitTests = testGroup "Unit tests"
  [
    -- reverse' [a] -> [a]
    testCase "test1a" $ assertEqual [] [5,4..1] (reverse' [1..5]),
    testCase "test1b" $ assertEqual [] [1,1,1] (reverse' [1,1,1]),
    testCase "test1c" $ assertEqual [] "apple" (reverse' "elppa"),
    testCase "test1d" $ assertEqual [] "a" (reverse' "a"),

    -- isPalindrome :: String -> Bool
    testCase "test2a" $ assertEqual [] False (isPalindrome "12345"),
    testCase "test2b" $ assertEqual [] True (isPalindrome "madam"),
    testCase "test2c" $ assertEqual [] False (isPalindrome "12322"),
    testCase "test2d" $ assertEqual [] True (isPalindrome "a"),
    testCase "test2e" $ assertEqual [] True (isPalindrome "aaa"),
    testCase "test2f" $ assertEqual [] True (isPalindrome "1221"),
    testCase "test2g" $ assertEqual [] False (isPalindrome "madamm"),

    -- safeFindAfter :: String -> [String] -> Maybe [String]
    testCase "test3a" $ assertEqual [] (Just (tail animals)) (safeFindAfter "ape" animals),
    testCase "test3b" $ assertEqual [] (Just ["fish"]) (safeFindAfter "elephant" animals),
    testCase "test3c" $ assertEqual [] (Just []) (safeFindAfter "fish" animals),
    testCase "test3d" $ assertEqual [] Nothing (safeFindAfter "bobcat" animals),
    testCase "test3e" $ assertEqual [] (Just ["elephant", "fish"]) (safeFindAfter "dog" animals),

    -- member :: Int -> Set -> Bool
    testCase "test4a" $ assertEqual [] False (member 0 s1),
    testCase "test4b" $ assertEqual [] True (member 1 s1),
    testCase "test4c" $ assertEqual [] True (member 5 s1),
    testCase "test4d" $ assertEqual [] False (member 5 s5),

    -- size :: Set -> Int
    testCase "test5a" $ assertEqual [] 5 (size s1),
    testCase "test5b" $ assertEqual [] 5 (size s2),
    testCase "test5c" $ assertEqual [] 5 (size s3),
    testCase "test5d" $ assertEqual [] 0 (size s5),

    -- add :: Int -> Set -> Set
    testCase "test6a" $ assertEqual [] 6 (len (add 6 s1)),
    testCase "test6b" $ assertEqual [] True (elemhelp 6 (add 6 s1)),
    testCase "test6d" $ assertEqual [] False (elemhelp 7 (add 6 s1)),
    testCase "test6e" $ assertEqual [] 1 (len (add 5 s5)),
    testCase "test6f" $ assertEqual [] True (elemhelp 6 (add 6 s5)),
    testCase "test6g" $ assertEqual [] False (elemhelp 7 (add 6 s5)),

    -- safeRemoveMax :: Set -> Maybe Int
    testCase "test7a" $ assertEqual [] (Just 5) (safeRemoveMax s1),
    testCase "test7b" $ assertEqual [] Nothing (safeRemoveMax s5),
    testCase "test7c" $ assertEqual [] (Just 8) (safeRemoveMax (Set [5,8,3])),
    testCase "test7d" $ assertEqual [] (Just 10) (safeRemoveMax (Set [10,6..2])),

    -- equal :: Set -> Set -> Bool
    testCase "test8a" $ assertEqual [] True (equal s1 s1),
    testCase "test8b" $ assertEqual [] True (equal EmptySet EmptySet),
    testCase "test8c" $ assertEqual [] False (equal s1 s5),
    testCase "test8d" $ assertEqual [] False (equal s1 s4),
    testCase "test8e" $ assertEqual [] True (equal s1 (Set [3,4,2,1,5])),
    testCase "test8f" $ assertEqual [] False (equal s1 (Set [3,4,2,1])),

    -- union :: Set -> Set -> Set
    testCase "test9a" $ assertEqual [] True (eqhelper s1 (union s1 s1)),
    testCase "test9b" $ assertEqual [] True (eqhelper s4 (union s2 s3)),
    testCase "test9c" $ assertEqual [] True (eqhelper (Set ([1..5]++[7,9])) (union s1 s2)),
    testCase "test9d" $ assertEqual [] True (eqhelper (Set ([1..5]++[7,9])) (union s2 s1)),
    testCase "test9e" $ assertEqual [] True (eqhelper s4 (union s4 s2)),

    -- intersection :: Set -> Set -> Set
    testCase "test10a" $ assertEqual [] True (eqhelper s1 (intersection s1 s1)),
    testCase "test10b" $ assertEqual [] True (eqhelper EmptySet (intersection s2 s3)),
    testCase "test10c" $ assertEqual [] True (eqhelper (Set [1,3,5]) (intersection s1 s2)),
    testCase "test10d" $ assertEqual [] True (eqhelper (Set [1,3,5]) (intersection s2 s1)),
    testCase "test10e" $ assertEqual [] True (eqhelper s2 (intersection s4 s2))
  ]
