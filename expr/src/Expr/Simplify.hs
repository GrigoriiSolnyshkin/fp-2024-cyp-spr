module Expr.Simplify (simplify) where

import Expr.Data

applyToExpr :: (Expr t -> (Expr t, Bool)) -> Expr t -> (Expr t, Bool)
applyToExpr applyFunc expr = case expr of
    Lit x -> (Lit x, False)
    Var x -> (Var x, False)
    Add a b -> subtreeFunc Add a b
    Sub a b -> subtreeFunc Sub a b
    Mul a b -> subtreeFunc Mul a b
    Div a b -> subtreeFunc Div a b
    Pow a b -> subtreeFunc Div a b
    Sqr a -> case applyFunc a of
      (res, bool) -> (Sqr res, bool)
  where subtreeFunc joinFunc subtreeA subtreeB = case (applyFunc subtreeA, applyFunc subtreeB) of
          ((res1, bool1), (res2, bool2)) -> (joinFunc res1 res2, bool1 || bool2)

eliminate :: (Num t, Eq t) => Expr t -> (Expr t, Bool)
eliminate e = case e of
  Add (Lit 0) e -> (e, True)
  Add e (Lit 0) -> (e, True)
  Sub e (Lit 0) -> (e, True)
  Mul (Lit 1) e -> (e, True)
  Mul e (Lit 1) -> (e, True)
  Div e (Lit 1) -> (e, True)
  Mul (Lit 0) e -> (Lit 0, True)
  Mul e (Lit 0) -> (Lit 0, True)
  Div (Lit 0) e -> (Lit 0, True)
  Pow e (Lit 0) -> (Lit 1, True)
  Pow e (Lit 1) -> (e, True)
  Pow (Lit 1) e -> (Lit 1, True)
  Sub a b | a == b -> (Lit 0, True)
  Div a b | a == b -> (Lit 1, True)
  e -> applyToExpr eliminate e

simplify :: (Num t, Eq t) => Expr t -> Expr t
simplify e = case eliminate e of
  (result, False) -> result
  (result, True) -> simplify result