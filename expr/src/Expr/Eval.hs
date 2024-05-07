module Expr.Eval (eval, evalMonadic) where

import Control.Monad.State

import Expr.Data
import qualified Data.Map.Strict as M


evalMonadic :: (Floating t, Ord t) => Expr t -> State (VarList t) (EvalResult t)
evalMonadic expr = case expr of
  Lit x -> do return $ Right x
  Var s -> do
    state <- get
    return $ case M.lookup s state of
      Nothing -> Left (VariableNotFound s)
      Just value -> Right value
  Add a b -> evalPureBinary (+) a b
  Sub a b -> evalPureBinary (-) a b
  Mul a b -> evalPureBinary (*) a b
  Div a b -> do
    evalA <- evalMonadic a
    evalB <- evalMonadic b
    s <- get
    return $ case (evalA, evalB) of
      (Right _, Right 0.0) -> Left (ZeroInDivision s b)
      (Right resA, Right resB) -> Right $ resA / resB
      errorEvaluation -> extractError errorEvaluation
  Pow a b -> do
    evalA <- evalMonadic a
    evalB <- evalMonadic b
    s <- get
    return $ case (evalA, evalB) of
      (Right resA, Right _) | resA <= 0 -> Left (NegativeInPower s a)
      (Right resA, Right resB) -> Right $ resA ** resB
      errorEvaluation -> extractError errorEvaluation
  Sqr a -> do
    eval <- evalMonadic a
    s <- get
    return $ case eval of
      Right res | res < 0 -> Left (NegativeInSqrt s a)
      Right res -> Right $ res ** 0.5
      errorEvaluation -> errorEvaluation
  where
    evalPureBinary op a b = do
      evalA <- evalMonadic a
      evalB <- evalMonadic b
      return $ case (evalA, evalB) of
        (Right resA, Right resB) -> Right $ op resA resB
        errors -> extractError errors
    extractError (Left error, _) = Left error
    extractError (_, Left error) = Left error

eval :: (Floating t, Ord t) => VarList t -> Expr t -> EvalResult t
eval varList expr = evalState (evalMonadic expr) varList

