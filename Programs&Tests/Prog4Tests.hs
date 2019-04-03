{- ##################################
   Richard Burns
   Units Tests for Homework 4.

   Usage: ghci Prog2Tests; main

   Dependencies: cabal install tasty
                 cabal install tasty-hunit
   ################################## -}


import Prog4
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

dates = [(3,21,1998),(7,12,2018),(6,28,2003),(11,11,2011),(11,9,2007),(2,12,2002)]

unitTests = testGroup "Unit tests"
  [
    -- older :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
    testCase "test1a" $ assertEqual [] (1,17,2018) (older (1,17,2018) (3,31,2018)),
    testCase "test1b" $ assertEqual [] (3,12,2018) (older (3,17,2018) (3,12,2018)),
    testCase "test1c" $ assertEqual [] (1,17,2008) (older (1,17,2018) (1,17,2008)),
    testCase "test1d" $ assertEqual [] (8,17,1999) (older (12,3,2005) (8,17,1999)),

    -- numInMonth :: Int -> [(Int, Int, Int)] -> Int
    testCase "test2a" $ assertEqual [] 1 (numInMonth 3 dates),
    testCase "test2b" $ assertEqual [] 2 (numInMonth 11 dates),
    testCase "test2c" $ assertEqual [] 0 (numInMonth 12 dates),

    -- datesInMonth :: Int -> [(Int, Int, Int)] -> [(Int,Int,Int)]
    testCase "test3a" $ assertEqual [] [(3,21,1998)] (datesInMonth 3 dates),
    testCase "test3b" $ assertEqual [] [(11,11,2011),(11,9,2007)] (datesInMonth 11 dates),
    testCase "test3c" $ assertEqual [] [] (datesInMonth 12 dates),

    -- date2Str :: (Int, Int, Int) -> String
    testCase "test4a" $ assertEqual [] "March 21, 1998" (date2Str (dates !! 0)),
    testCase "test4b" $ assertEqual [] "July 12, 2018" (date2Str (dates !! 1)),
    testCase "test4c" $ assertEqual [] "June 28, 2003" (date2Str (dates !! 2)),

    -- date2Str' :: (Int, Int, Int) -> String
    testCase "test5a" $ assertEqual [] "November 11, 2011" (date2Str' (dates !! 3)),
    testCase "test5b" $ assertEqual [] "November 9, 2007" (date2Str' (dates !! 4)),
    testCase "test5c" $ assertEqual [] "February 12, 2002" (date2Str' (dates !! 5)),

    -- monthLookup :: Int -> Int
    testCase "test6a" $ assertEqual [] 1 (monthLookup 15),
    testCase "test6a" $ assertEqual [] 1 (monthLookup 31),
    testCase "test6b" $ assertEqual [] 2 (monthLookup 32),
    testCase "test6c" $ assertEqual [] 5 (monthLookup 135),
    testCase "test6d" $ assertEqual [] 7 (monthLookup 195),

    -- monthRange :: Int -> Int -> [Int]
    testCase "test7a" $ assertEqual [] [1,2,3,4] (monthRange 23 101),
    testCase "test7b" $ assertEqual [] [1..7] (monthRange 23 195),
    testCase "test7c" $ assertEqual [] [6,7] (monthRange 165 195),
    testCase "test7d" $ assertEqual [] [7] (monthRange 194 195),

    -- validDate :: (Int, Int, Int) -> Bool
    testCase "test8a" $ assertEqual [] True (validDate (dates !! 0)),
    testCase "test8b" $ assertEqual [] True (validDate (dates !! 1)),
    testCase "test8c" $ assertEqual [] False (validDate (11,31,2018)),
    testCase "test8d" $ assertEqual [] False (validDate (1,32,2018)),

    -- validLeapDate :: (Int, Int, Int) -> Bool
    testCase "test9a" $ assertEqual [] True (validLeapDate (2,29,2016)),
    testCase "test9b" $ assertEqual [] True (validLeapDate (2,29,2008)),
    testCase "test9c" $ assertEqual [] False (validLeapDate (2,29,1900)),
    testCase "test9d" $ assertEqual [] False (validLeapDate (2,29,2001)),
    testCase "test9e" $ assertEqual [] True (validLeapDate (2,29,2000)),

    -- season :: (Int, Int, Int) -> String
    testCase "test10a" $ assertEqual [] "Spring" (season (dates !! 0)),
    testCase "test10b" $ assertEqual [] "Summer" (season (dates !! 1)),
    testCase "test10c" $ assertEqual [] "Fall" (season (dates !! 4)),
    testCase "test10d" $ assertEqual [] "Winter" (season (dates !! 5))
  ]
