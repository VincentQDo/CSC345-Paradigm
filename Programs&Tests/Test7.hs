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
   expr21 = Add2 val21 val22 --5+10
   expr22 = Sub2 val21 val22 --5-10
   expr23 = Add2 expr21 expr22 --(5+10) + (5-10)
   expr24 = Multi2 val21 val22 --5*10
   expr25 = Div2 val21 val22 --5/10
   expr26 = Div2 val21 val23 -- 10/0
   expr27 = Multi2 expr25 expr26 -- (5/10) * (10/0)
   expr28 = Multi2 expr21 expr22 -- (5+10) * (5-10)
   expr29 = Multi2 expr23 expr24 -- ((5+10) + (5-10)) * (5*10)
   expr30 = Add2 expr29 expr28 -- (((5+10) + (5-10)) * (5*10)) + ((5+10) * (5-10))


   leaf1 = Leaf 1
   leaf2 = Leaf 2
   leaf3 = Leaf 3
   leaf4 = Leaf 4
   testTree = Node subTree3 subTree6 --not balanced, 10 leaves, 3 left 7 right
   subTree1 = Node leaf1 leaf2 --balanced, 2 leaves
   subTree2 = Node leaf3 leaf4 --balanced, 2 leaves
   subTree3 = Node subTree1 leaf1 --balanced, 3 leaves, 2 left, 1 right
   subTree4 = Node subTree3 subTree2 --balanced, 5 leaves, 3 left, 2 right
   subTree5 = Node subTree1 subTree2 --balanced, 4 leaves, 2 on both sides
   subTree6 = Node subTree1 subTree4 --not balanced, 7 leaves, 2 left, 5 right
   
   value31 = Val3 5
   value32 = Val3 3
   boolExpr1 = BoolLit True
   boolExpr2 = BoolLit False
   boolExpr3 = EqualTo value31 value32 --False
   boolExpr4 = LessThan value31 value32 --False
   boolExpr5 = LessThan value32 value31 --True
   boolExpr6 = Or boolExpr1 boolExpr2 --True
   boolExpr7 = Or boolExpr1 boolExpr3 --True
   boolExpr8 = Or boolExpr3 boolExpr4 --False
   boolExpr9 = Or boolExpr6 boolExpr4 --True
   boolExpr10 = Or boolExpr8 boolExpr9 --True

   expr31 = Add3 value31 value32 --8
   expr32 = Sub3 value31 value32 --2
   expr33 = Multi3 value31 value32 --15
   expr34 = Div3 value31 value32 --1
   expr35 = If boolExpr1 value31 value32 --5
   expr36 = If boolExpr2 value31 value32 --3
   expr37 = If boolExpr3 value31 value32 --3
   expr38 = If boolExpr4 value31 value32 --3
   expr39 = If boolExpr5 value31 value32 --5
   expr310 = If boolExpr6 expr34 expr33 --1
   expr311 = If boolExpr7 expr33 expr310 --15
   expr312 = If boolExpr8 expr310 expr33 --15
   expr313 = If boolExpr9 expr311 expr31 --15
   expr314 = If boolExpr10 expr313 expr310 --15


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

         testCase "test4a" $ assertEqual [] (Just 5) (value2 val21),
         testCase "test4b" $ assertEqual [] (Just 10) (value2 val22),
         testCase "test4c" $ assertEqual [] (Just 15) (value2 expr21),
         testCase "test4d" $ assertEqual [] (Just (-5)) (value2 expr22),
         testCase "test4e" $ assertEqual [] (Just 10) (value2 expr23),
         testCase "test4f" $ assertEqual [] (Just 50) (value2 expr24),
         testCase "test4g" $ assertEqual [] (Just 0) (value2 expr25),
         testCase "test4h" $ assertEqual [] Nothing (value2 expr26),
         testCase "test4i" $ assertEqual [] Nothing (value2 expr27),
         testCase "test4j" $ assertEqual [] (Just (-75)) (value2 expr28),
         testCase "test4k" $ assertEqual [] (Just 500) (value2 expr29),
         testCase "test4l" $ assertEqual [] (Just 425) (value2 expr30),

         testCase "test5a" $ assertEqual [] "5" (show val21),
         testCase "test5b" $ assertEqual [] "10" (show val22),
         testCase "test5c" $ assertEqual [] "5+10" (show expr21),
         testCase "test5d" $ assertEqual [] "5-10" (show expr22),
         testCase "test5e" $ assertEqual [] "(5+10)+(5-10)" (show expr23),
         testCase "test5f" $ assertEqual [] "5*10" (show expr24),
         testCase "test5g" $ assertEqual [] "5/10" (show expr25),
         testCase "test5h" $ assertEqual [] "10/0" (show expr26),
         testCase "test5i" $ assertEqual [] "(5/10)*(10/0)" (show expr27),
         testCase "test5j" $ assertEqual [] "(5+10)*(5-10)" (show expr28),
         testCase "test5k" $ assertEqual [] "((5+10)+(5-10))*(5*10)" (show expr29),
         testCase "test5l" $ assertEqual [] "(((5+10)+(5-10))*(5*10))+((5+10)*(5-10))" (show expr30),

         testCase "test6a" $ assertEqual [] "eggyay" (piglatinize "egg"),
         testCase "test6b" $ assertEqual [] "eggray" (piglatinize "greg"),

         testCase "test7a" $ assertEqual [] True (balanced subTree1),
         testCase "test7b" $ assertEqual [] True (balanced subTree2),
         testCase "test7c" $ assertEqual [] True (balanced subTree3),
         testCase "test7d" $ assertEqual [] True (balanced subTree4),
         testCase "test7e" $ assertEqual [] True (balanced subTree5),
         testCase "test7f" $ assertEqual [] False (balanced subTree6),
         testCase "test7g" $ assertEqual [] False (balanced testTree),
         testCase "test7h" $ assertEqual [] True (balanced leaf1),

         testCase "test9a" $ assertEqual [] True (bEval boolExpr1),
         testCase "test9b" $ assertEqual [] False (bEval boolExpr2),
         testCase "test9c" $ assertEqual [] False (bEval boolExpr3),
         testCase "test9d" $ assertEqual [] False (bEval boolExpr4),
         testCase "test9e" $ assertEqual [] True (bEval boolExpr5),

         testCase "test10a" $ assertEqual [] (Just 8) (value3 expr31),
         testCase "test10b" $ assertEqual [] (Just 2) (value3 expr32),
         testCase "test10c" $ assertEqual [] (Just 15) (value3 expr33),
         testCase "test10d" $ assertEqual [] (Just 1) (value3 expr34),
         testCase "test10e" $ assertEqual [] (Just 5) (value3 expr35),
         testCase "test10f" $ assertEqual [] (Just 3) (value3 expr36),
         testCase "test10g" $ assertEqual [] (Just 3) (value3 expr37),
         testCase "test10h" $ assertEqual [] (Just 3) (value3 expr38),
         testCase "test10i" $ assertEqual [] (Just 5) (value3 expr39),
         testCase "test10j" $ assertEqual [] (Just 1) (value3 expr310),
         testCase "test10k" $ assertEqual [] (Just 15) (value3 expr311),
         testCase "test10l" $ assertEqual [] (Just 15) (value3 expr312),
         testCase "test10m" $ assertEqual [] (Just 15) (value3 expr313),
         testCase "test10n" $ assertEqual [] (Just 15) (value3 expr314)

     ]
   