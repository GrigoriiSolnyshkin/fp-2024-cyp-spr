module Main where

import Text.Printf (printf)
import Control.Monad (unless)
import qualified Data.Map.Strict as M
import Expr.Data
import Expr.Eval
import Expr.Simplify


casesShow :: [(Expr Int, String)]
casesShow = [ (Var "x", "$x"), (Lit 5, "5"), (5 + 5, "5 + 5"), (5 * 5, "5 * 5")
            , (Div 5 5, "5 / 5"), (5 - 5, "5 - 5"), (Pow 5 5, "5 ^ 5"), (Var "x" * 5, "$x * 5")
            , (Sqr 5, "sqrt(5)"), ((5 + 5) * (5 - 5), "(5 + 5) * (5 - 5)")
            , (Pow (5 + 5) (5 * 5), "(5 + 5) ^ (5 * 5)"), (5 * 5 + 5, "5 * 5 + 5")
            , (Div (Pow (5 * 5 + 5) 5) 5, "(5 * 5 + 5) ^ 5 / 5")
            ]

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
                ]

casesDouble :: [((Expr Double, [(String, Double)]), EvalResult Double)]
casesDouble = [ ((four, []), Right 4.0), ((sum, []), Right 9.0), ((diff, []), Right (-1.0))
              , ((four * five, []), Right 20.0), ((Div four five, []), Right 0.8)
              , ((Pow four five, []), Right 1024.0), ((Sqr four, []), Right 2.0)
              , ((diff * diff, []), Right 1.0), ((Div sum diff, []), Right (-9.0))
              , ((Div diff zero, []), Left (ZeroInDivision M.empty zero))
              , ((Pow diff sum, []), Left (NegativeInPower M.empty diff))
              , ((Sqr diff, []), Left (NegativeInSqrt M.empty diff))
              , ((Pow zero sum, []), Left (NegativeInPower M.empty zero))
              , ((Var "x", xyz), Right 1.0)
              , ((Var "x" + Var "y", xyz), Right 3.0)
              , ((Var "x" * Var "a", xyz), Left $ VariableNotFound "a")
              ]
  where four = Lit 4
        five = Lit 5
        sum = four + five
        diff = four - five
        zero = four - four
        xyz = [("x", 1.0), ("y", 2.0), ("z", 3.0)]

testDouble :: (Expr Double, [(String, Double)]) -> EvalResult Double -> IO ()
testDouble (expr, varList) expected =
    let actual = eval (M.fromList varList) expr in
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual)

testSimplify :: Expr Double -> Expr Double -> IO()
testSimplify expr expected =
    let actual = simplify expr in
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "simplify (%s) should be %s but it was %s" (show expr) (show expected) (show actual)

testShow :: Expr Int -> String -> IO ()
testShow expr expected =
    let actual = show expr in
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure = printf "show (%s) should be %s bui it was %s" (show expr) expected

main :: IO ()
main = do
  mapM_ (uncurry testDouble) casesDouble
  mapM_ (uncurry testSimplify) casesSimplify
  mapM_ (uncurry testShow) casesShow
