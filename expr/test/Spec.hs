import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( (@?=), testCase, assertBool, assertFailure )
import Expr.Data
import Expr.Eval
import Expr.Simplify

import qualified Data.Map.Strict as M

testEval :: TestTree
testEval = 
    testGroup "Eval" [ testAdd, testSub, testMul, testDiv, testPow, testSqr, testSqrNeg, testExpNeg, testZeroDiv, testVar, testVarError ]
  where 
    testEvalNoVarListSuccess msg expr res = 
       testCase msg $ eval M.empty expr @?= Right res 
    testAdd = testGroup "Add" 
      [ testEvalNoVarListSuccess "1 + 2 == 3" (Add (Lit 1) (Lit 2)) 3
      , testEvalNoVarListSuccess "2 + 1 == 3" (Add (Lit 2) (Lit 1)) 3
      , testEvalNoVarListSuccess "0 + 2 == 2" (0 + 2) 2
      ]
    testSub = testGroup "Sub"
      [ testEvalNoVarListSuccess "1 + (2 - 1) == 2" (1 + Sub 2 1) 2
      , testEvalNoVarListSuccess "3 - 2 - 1 == 0" (Lit 3 - Lit 2 - Lit 1) 0
      , testEvalNoVarListSuccess "3 - (2 - 1) == 2" (Lit 3 - (2 - 1)) 2
      ]
    testMul = testGroup "Mul"
      [ testEvalNoVarListSuccess "(2 - 1) * (2 + 1) == 3" (Mul (2 - 1) (2 + 1)) 3
      , testEvalNoVarListSuccess "4 * 4 - 3 * 3 == 7" (Mul 4 4 - Mul 3 3) 7
      , testEvalNoVarListSuccess "3 * 5 == 15" (3 * 5) 15
      ]
    testDiv = testGroup "Div"
      [ testEvalNoVarListSuccess "15 / 3 == 5" (Div 15 3) 5
      , testEvalNoVarListSuccess "(7 + 3) / (5 - 3) == 5" (Div (7 + 3) (5 - 3)) 5
      ]
    testPow = testGroup "Pow" 
      [ testEvalNoVarListSuccess "2 ** 3 == 8" (Pow 2 3) 8
      , testEvalNoVarListSuccess "3 ** 2 == 9" (Pow 3 2) 9
      , testEvalNoVarListSuccess "2 ** (-1) == 0.5" (Pow 2 (-1)) 0.5
      ]
    testSqr = testGroup "Sqr"
      [ testEvalNoVarListSuccess "sqrt(81) == 9" (Sqr 81) 9
      , testEvalNoVarListSuccess "sqrt(0) == 0" (Sqr 0) 0
      ]

    testEvalFailsWith msg expr error = 
      testCase msg $ eval M.empty expr @?= Left error

    sqrNeg = Sqr (-1)
    sqrNegError = NegativeInSqrt M.empty (-1)
    testSqrNeg = testGroup "SqrNeg"
      [ testEvalFailsWith "sqr(-1) fails" sqrNeg sqrNegError
      , testEvalFailsWith "sqr(-1) + 1 fails" (1 + sqrNeg) sqrNegError
      , testEvalFailsWith "sqr(-1) * sqr(-1) fails" (sqrNeg * sqrNeg) sqrNegError
      ]
    
    expNeg = Pow (-2) (Lit 0.5)
    expNegError = NegativeInPower M.empty (-2)
    testExpNeg = testGroup "ExpNeg"
      [ testEvalFailsWith "(-2) ** 0.5 fails" expNeg expNegError
      , testEvalFailsWith "10 - (-2) ** 0.5 fails" (10 - expNeg) expNegError
      , testEvalFailsWith "0 ** 10 fails" (Pow 0 10) (NegativeInPower M.empty 0)
      ]
    
    zeroDiv = Div 10 0
    zeroDivError = ZeroInDivision M.empty (Lit 0)
    testZeroDiv = testGroup "ZeroDiv"
      [ testEvalFailsWith "10 / 0 fails" zeroDiv zeroDivError
      , testEvalFailsWith "sqrt(10 / 0)" (Sqr zeroDiv) zeroDivError
      ]

    varList = M.fromList [("one", 1), ("zero", 0), ("-one", -1), ("two", 2), ("ten", 10)]
    testEvalDefaultSuccess msg expr res =
      testCase msg $ eval varList expr @?= Right res
    
    testEvalFailsWithVar msg expr string = 
      testCase msg $ eval varList expr @?= Left (VariableNotFound string)

    testVar = testGroup "Var"
      [ testEvalDefaultSuccess "1 + 2 == 3 as vars" (Var "one" + Var "two") 3
      , testEvalDefaultSuccess "1 - 1 == 0 as vars" (Var "one" + Var "-one") 0
      , testEvalDefaultSuccess "10 / 2 == 5 as vars" (Div (Var "ten") (Var "two")) 5
      , testEvalDefaultSuccess "1 * (-1) == -1 as vars" (Mul (Var "one") (Var "-one")) (-1)
      , testEvalDefaultSuccess "10 ** (2 + 1) == 1000 as vars" (Pow (Var "ten") (Var "two" + Var "one")) 1000
      ]
    
    testVarError = testGroup "VarError"
      [ testEvalFailsWithVar "1 + 3 as vars fails" (Var "one" + Var "three") "three"
      , testEvalFailsWithVar "9 / 9 as vars fails" (Div (Var "nine") (Var "nine")) "nine"
      ]
    
    

main :: IO ()
main = 
  defaultMain $ testGroup "Expressions" [ testEval ]