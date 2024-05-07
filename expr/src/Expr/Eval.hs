module Expr.Eval (eval, evalMonadic) where

import Control.Monad.Trans.State
import Control.Monad.Trans.Class

import Expr.Data
import qualified Data.Map.Strict as M


evalMonadic :: (Floating t, Ord t) => Expr t -> StateT (VarList t) (Either (Error t)) t
evalMonadic expr = case expr of
  Lit x -> do return x
  Var s -> do
    state <- get
    case M.lookup s state of
      Nothing -> lift $ Left (VariableNotFound s)
      Just value -> return value
  Add a b -> evalPureBinary (+) a b
  Sub a b -> evalPureBinary (-) a b
  Mul a b -> evalPureBinary (*) a b
  Div a b -> do
    evalA <- evalMonadic a
    evalB <- evalMonadic b
    s <- get
    if evalB == 0.0 then lift $ Left (ZeroInDivision s b) else return $ evalA / evalB
  Pow a b -> do
    evalA <- evalMonadic a
    evalB <- evalMonadic b
    s <- get
    if evalA <= 0 then lift $ Left (NegativeInPower s a) else return $ evalA ** evalB
  Sqr a -> do
    eval <- evalMonadic a
    s <- get
    if eval < 0 then lift $ Left (NegativeInSqrt s a) else return $ sqrt eval
  where
    evalPureBinary :: (Floating t, Ord t) => (t -> t -> t) -> Expr t -> Expr t -> StateT (VarList t) (Either (Error t)) t
    evalPureBinary op a b = do
      evalA <- evalMonadic a
      evalB <- evalMonadic b
      return $ op evalA evalB

eval :: (Floating t, Ord t) => VarList t -> Expr t -> EvalResult t
eval varList expr = evalStateT (evalMonadic expr) varList

