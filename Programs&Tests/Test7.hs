{- ##################################
Vincent Do
Units Tests for Homework 7.

Usage: ghci Test7; main
First time user type in the command below
      sudo cabal install tasty
      sudo cabal install tasty-hunit

   ################################## -}


   import Prog7
   import Test.Tasty
   import Test.Tasty.HUnit
   import System.Environment
   
   
   main = do
       setEnv "TASTY_TIMEOUT" "2s"
       defaultMain tests
   
   tests :: TestTree
   tests = testGroup "Tests" [unitTests]

   l0 = [2,2,2,2,2]
   l1 = [1,2,3,4,5,2]
   l2 = [1,2,3,4]
   l3 = [1]
   l4 :: [Int]
   l4 = []
   
   val1 = Val1 5
   val2 = Val1 7
   exp1 = Add1 val1 val2
   exp2 = Sub1 val2 val1
   exp3 = Sub1 val1 val2
   exp4 = Add1 val2 val1
   exp5 = Add1 exp1 exp3

   val21 = Val2 5
   val22 = Val2 10
   val23 = Val2 0
   expr21 = Add2 val21 val22
   expr22 = Sub2 val21 val22
   expr23 = Add2 expr21 expr22
   expr24 = Multi2 val21 val22
   expr25 = Div2 val21 val22
   expr26 = Div2 val21 val23
   expr27 = Multi2 expr25 expr26
   expr28 = Multi2 expr21 expr22
   expr29 = Multi2 expr23 expr24
   expr30 = Add2 expr29 expr28









   
   unitTests = testGroup "Unit tests"
     [
   
         testCase "test1a" $ assertEqual [] [] (unique l0),
         testCase "test1b" $ assertEqual [] [1,3,4,5] (unique l1),
         testCase "test1c" $ assertEqual [] [1,2,3,4] (unique l2),
         testCase "test1d" $ assertEqual [] [1] (unique l3),
         testCase "test1e" $ assertEqual [] [] (unique l4),

         testCase "test2a" $ assertEqual [] 5 (value1 val1),
         testCase "test2b" $ assertEqual [] 7 (value1 val2),
         testCase "test2c" $ assertEqual [] 12 (value1 exp1),
         testCase "test2d" $ assertEqual [] 2 (value1 exp2),
         testCase "test2e" $ assertEqual [] (-2) (value1 exp3),
         testCase "test2f" $ assertEqual [] 12 (value1 exp4),
         testCase "test2g" $ assertEqual [] 10 (value1 exp5),

         testCase "test3a" $ assertEqual [] (Just 5) (value2 val21),
         testCase "test3b" $ assertEqual [] (Just 10) (value2 val22),
         testCase "test3c" $ assertEqual [] (Just 15) (value2 expr21),
         testCase "test3d" $ assertEqual [] (Just (-5)) (value2 expr22),
         testCase "test3e" $ assertEqual [] (Just 10) (value2 expr23),
         testCase "test3f" $ assertEqual [] (Just 50) (value2 expr24),
         testCase "test3g" $ assertEqual [] (Just 0) (value2 expr25),
         testCase "test3h" $ assertEqual [] Nothing (value2 expr26),
         testCase "test3i" $ assertEqual [] Nothing (value2 expr27),
         testCase "test3j" $ assertEqual [] (Just (-75)) (value2 expr28),
         testCase "test3k" $ assertEqual [] (Just 500) (value2 expr29),
         testCase "test3l" $ assertEqual [] (Just 425) (value2 expr30)












     ]
   