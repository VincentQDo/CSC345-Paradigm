{- ##################################
Vincent Do
Units Tests for Homework 4.

Usage: ghci Test4; main
First time user type in the command below
      sudo cabal install tasty
      sudo cabal install tasty-hunit

   ################################## -}


import Prog4
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


      testCase "test1a" $ assertEqual [] (02,03,2009) (older (01,09,2010) (02,03,2009)),
      testCase "test1b" $ assertEqual [] (01,01,2009) (older (01,01,2009) (02,03,2009)),
      testCase "test1c" $ assertEqual [] (02,03,2009) (older (03,02,2009) (02,03,2009)),
      testCase "test1d" $ assertEqual [] (02,30,2009) (older (01,09,2010) (02,30,2009)),
      testCase "test1e" $ assertEqual [] (02,30,2009) (older (02,30,2009) (02,30,2009)),

      testCase "test2a" $ assertEqual [] 1 (numInMonth 1 [(01,01,2008),(02,03,2009),(12,01,2009)]),
      testCase "test2b" $ assertEqual [] 0 (numInMonth 1 []),
      testCase "test2a" $ assertEqual [] 1 (numInMonth 1 [(01,01,2008),(02,30,2009),(12,01,2009)]),


      testCase "test3a" $ assertEqual [] [(01,01,2008),(01,12,2009)] (datesInMonth 1 [(01,01,2008),(02,03,2009),(01,12,2009)]),
      testCase "test3b" $ assertEqual [] [] (datesInMonth 1 []),

      testCase "test4a" $ assertEqual [] "February 3, 2010" (date2Str (2,3,2010)),
      testCase "test4b" $ assertEqual [] "June 3, 2000" (date2Str (6,3,2000)),

      testCase "test5a" $ assertEqual [] "February 3, 2010" (date2Str' (2,3,2010)),
      testCase "test5b" $ assertEqual [] "June 3, 2000" (date2Str' (6,3,2000)),

      testCase "test6a" $ assertEqual [] 1 (monthLookup 31),
      testCase "test6b" $ assertEqual [] 12 (monthLookup 365),

      testCase "test7a" $ assertEqual [] [1,2] (monthRange 1 32),
      testCase "test7b" $ assertEqual [] [1,2,3,4,5,6,7,8,9,10,11,12] (monthRange 1 365),

      testCase "test8a" $ assertEqual [] True (validDate (3,12,2019)),
      testCase "test8b" $ assertEqual [] True (validDate (1,31,1996)),
      testCase "test8c" $ assertEqual [] False (validDate (11,31,1996)),

      testCase "test9a" $ assertEqual [] False (validLeapDate (2,29,1900)),
      testCase "test9b" $ assertEqual [] True (validLeapDate (2,29,1996)),
      testCase "test9c" $ assertEqual [] True (validLeapDate (2,29,2000)),

      testCase "test10a" $ assertEqual [] "Spring" (season (3,29,2019)),
      testCase "test10b" $ assertEqual [] "Summer" (season (6,21,2019)),
      testCase "test10c" $ assertEqual [] "Fall" (season (9,22,2019)),
      testCase "test10d" $ assertEqual [] "Winter" (season (12,21,2019)),
      testCase "test10e" $ assertEqual [] "Winter" (season (12,30,2019)),
      testCase "test10f" $ assertEqual [] "Spring" (season (5,21,2019)),
      testCase "test10g" $ assertEqual [] "Winter" (season (2,29,2020)),
      testCase "test10h" $ assertEqual [] "Not a valid date" (season (11,31,2019)),

      testCase "test4 fail" $ assertEqual [] [] (date2Str (0, 2, 2000)),
      testCase "test5 fail" $ assertEqual [] [] (date2Str' (0, 2, 2000)),
      testCase "test7 fail" $ assertEqual [] [] (monthRange (-1) 5),
      testCase "test6 fail" $ assertEqual [] 0 (monthLookup 0),
      testCase "test8 fail" $ assertEqual [] False (validDate (0,10,2000))

  ]
