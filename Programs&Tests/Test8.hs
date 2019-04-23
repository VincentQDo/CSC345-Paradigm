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
   
   
   unitTests = testGroup "Unit tests"
     [
   
         testCase "test1a" $ assertEqual [] [] (),

     ]
   