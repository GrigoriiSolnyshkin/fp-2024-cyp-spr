import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( (@?=), testCase, assertBool, assertFailure )
import Expr.Data
import Expr.Eval
import Expr.Simplify
import Expr.PrefixNotationParser
import Data.Either (isLeft)

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
      [ testEvalFailsWith "sqrt(-1) fails" sqrNeg sqrNegError
      , testEvalFailsWith "sqrt(-1) + 1 fails" (1 + sqrNeg) sqrNegError
      , testEvalFailsWith "sqrt(-1) * sqrt(-1) fails" (sqrNeg * sqrNeg) sqrNegError
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

testSimplify :: TestTree
testSimplify =
    testGroup "Simplify" $ map (\(original, expected) -> testCase (show original ++ " simplifies to " ++ show expected) $ simplify original @?= expected) casesSimplify
  where
    casesSimplify :: [(Expr Double, Expr Double)]
    casesSimplify = [ (Var "x" * 0, 0)
                    , (Var "x" * 1, Var "x")
                    , (Var "x", Var "x")
                    , (5 * 0 + 4 * 1, 4)
                    , (Var "x" - Pow (Var "x") 1 + 10, 10)
                    , (Pow (Var "z") 0 - Pow 1 (Var "z"), 0)
                    , (Div (Var "x") (Div (Var "x") 1), 1)
                    , (0 + Var "x", Var "x")
                    , (Var "x" - 0, Var "x")
                    , (1 * (Var "x" - 4), Var "x" - 4)
                    , (1, 1)
                    , (0, 0)
                    , (Var "x" * Var "y", Var "x" * Var "y")
                    , (Div 1 $ Var "x", Div 1 $ Var "x")
                    , (Mul 2 (Var "x" + 1), Mul 2 (Var "x" + 1))
                    ]

testShow :: TestTree
testShow =
    testGroup "Show" $ map (\(expr, expected) -> testCase ("show " ++ expected) $ show expr @?= expected) casesShow
  where
    casesShow :: [(Expr Int, String)]
    casesShow = [ (Var "x", "$x"), (Lit 5, "5"), (5 + 5, "5 + 5"), (5 * 5, "5 * 5")
                , (Div 5 5, "5 / 5"), (5 - 5, "5 - 5"), (Pow 5 5, "5 ^ 5"), (Var "x" * 5, "$x * 5")
                , (Sqr 5, "sqrt(5)"), ((5 + 5) * (5 - 5), "(5 + 5) * (5 - 5)")
                , (Pow (5 + 5) (5 * 5), "(5 + 5) ^ (5 * 5)"), (5 * 5 + 5, "5 * 5 + 5")
                , (Pow 5 5 + 5 * 5, "5 ^ 5 + 5 * 5")
                , (Pow 5 5 * (5 + 5), "5 ^ 5 * (5 + 5)")
                , (Div (Pow (5 * 5 + 5) 5) 5, "(5 * 5 + 5) ^ 5 / 5")
                , (Pow (Sqr (Div 5 5)) (5 - 5), "sqrt(5 / 5) ^ (5 - 5)")
                ]

testParser :: TestTree
testParser =
    testGroup "Parser" [testParserOk, testParserNotOk]
  where
    testParserOk = testGroup "ParserOk" $ map (\(str, expected) -> testCase ("parse " ++ str) $ parseExpr str @?= Right expected) casesOk

    casesOk :: [(String, Expr Int)]
    casesOk = [ ("5", 5), ("a", Var "a"), ("a5", Var "a5"), ("a'5'5", Var "a'5'5"), ("049", 49), ("+ a b", Var "a" + Var "b"), ("+ 1 b", 1 + Var "b")
              , ("- 4 4", 4 - 4), ("* 4 5", 4 * 5), ("/ 22 c", Div 22 $ Var "c"), ("sqrt abcde", Sqr $ Var "abcde"), ("+ + 1 2 3", (1 + 2) + 3)
              , ("+ 1 + 2 3", 1 + (2 + 3)), ("+ sqrt 5 ^ 2 2", Sqr 5 + Pow 2 2), ("- - 4 sqrt 2 5", (4 - Sqr 2) - 5)
              ]
    
    testParserNotOk = testGroup "ParserNotOk" $ map (\str -> testCase ("not parse " ++ str) $ assertBool "" (isLeft $ parseExpr str)) casesNotOk

    casesNotOk :: [String]
    casesNotOk = ["5a", "5&", "+ 5", "'0", "sqrt", "-5", "+ 4 4 4", "6 6", " ", "", " 4", "3 ", "16 ^", "- 24", "+ + + + 4 4 4 4", "/ 4 4a", "/4 3", "% 5 2"]


main :: IO ()
main = 
  defaultMain $ testGroup "Expressions" [ testEval, testSimplify, testShow, testParser ]
