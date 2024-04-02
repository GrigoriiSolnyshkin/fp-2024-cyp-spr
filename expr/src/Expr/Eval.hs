module Expr.Eval (eval) where

import Expr.Data
import qualified Data.Map.Strict as M

eval :: (Floating t, Ord t) => VarList t -> Expr t -> EvalResult t
eval varList expr = case expr of
  Lit x -> Right x
  Var s -> lookupVar s
  Add a b -> evalPureBinary (+) a b
  Sub a b -> evalPureBinary (-) a b
  Mul a b -> evalPureBinary (*) a b
  Div a b -> case (eval' a, eval' b) of
    (Right _, Right 0.0) -> Left (ZeroInDivision varList b)
    (Right evalA, Right evalB) -> Right (evalA / evalB)
    errorEvaluation -> extractError errorEvaluation
  Pow a b -> case (eval' a, eval' b) of
    (Right evalA, Right evalB) | evalA <= 0.0 -> Left (NegativeInPower varList a)
                               | otherwise -> Right (evalA ** evalB)
    errorEvaluation -> extractError errorEvaluation
  Sqr a -> case eval' a of
    Right evalA | evalA < 0.0 -> Left (NegativeInSqrt varList a)
                | otherwise -> Right (evalA ** 0.5)
    errorEvaluation -> errorEvaluation
  where
    eval' = eval varList
    extractError :: (EvalResult t, EvalResult t) -> EvalResult t
    extractError (Left err, _) = Left err
    extractError (_, Left err) = Left err
    evalPureBinary op a b = case (eval' a, eval' b) of
      (Right evalA, Right evalB) -> Right (op evalA evalB)
      errorEvaluation -> extractError errorEvaluation
    lookupVar s = case M.lookup s varList of
      Nothing -> Left (VariableNotFound s)
      Just x -> Right x