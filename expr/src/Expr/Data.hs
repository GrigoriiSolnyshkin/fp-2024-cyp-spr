module Expr.Data(Expr(..), VarList(..), Error(..), EvalResult(..)) where

import qualified Data.Map.Strict as M

addParentheses :: String -> String
addParentheses str = "(" ++ str ++ ")"

joinByOp :: Char -> String -> String -> String
joinByOp op str1 str2 = str1 ++ ' ':op:" " ++ str2

data Expr a = Sub (Expr a) (Expr a)
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            | Div (Expr a) (Expr a)
            | Pow (Expr a) (Expr a)
            | Lit a
            | Var String
            | Sqr (Expr a) deriving Eq

data Context = Pure
             | AddArg
             | SubLeftArg
             | SubRightArg
             | MulArg
             | DivLeftArg
             | DivRightArg
             | PowRightArg
             | PowLeftArg
             deriving (Eq, Ord)


showPure :: Show t => Expr t -> String
showPure (Add a b) = joinByOp '+' (showInCtx AddArg a) (showInCtx AddArg b)
showPure (Sub a b) = joinByOp '-' (showInCtx SubLeftArg a) (showInCtx SubRightArg b)
showPure (Mul a b) = joinByOp '*' (showInCtx MulArg a) (showInCtx MulArg b)
showPure (Div a b) = joinByOp '/' (showInCtx DivLeftArg a) (showInCtx DivRightArg b)
showPure (Pow a b) = joinByOp '^' (showInCtx PowLeftArg a) (showInCtx PowRightArg b)
showPure (Lit x) = show x
showPure (Var x) = '$':x
showPure (Sqr a) = "sqrt(" ++ showPure a ++ ")"

showInCtx :: Show t => Context -> Expr t -> String
showInCtx ctx expr = let
                       pure = showPure expr
                       inParentheses = addParentheses pure
                       delimitByCtx context = if ctx <= context then pure else inParentheses
                     in case expr of
  Lit _ -> pure
  Sqr _ -> pure
  Var _ -> pure
  Pow _ _ -> delimitByCtx PowRightArg
  Div _ _ -> delimitByCtx DivLeftArg
  Mul _ _ -> delimitByCtx DivLeftArg
  Sub _ _ -> delimitByCtx SubLeftArg
  Add _ _ -> delimitByCtx SubLeftArg


instance Show t => Show (Expr t) where
  show = showPure

instance Num t => Num (Expr t) where
  (+) = Add
  (-) = Sub
  (*) = Mul
  fromInteger i = Lit $ fromInteger i
  abs expr = expr
  signum expr = Lit 1

type VarList t = M.Map String t

showVarList :: Show t => VarList t -> String
showVarList mp = case M.assocs mp of
    [] -> ""
    x:xs -> "[" ++ foldl (\done entry-> done ++ ", " ++ showPair entry) (showPair x) xs ++ "]"
  where showPair (s, v) = s ++ "=" ++ show v

data Error t = NegativeInSqrt (VarList t) (Expr t)
             | NegativeInPower (VarList t) (Expr t)
             | ZeroInDivision (VarList t) (Expr t)
             | VariableNotFound String deriving Eq

instance Show t => Show (Error t) where
  show err = "Evaluation error! " ++ case err of
      NegativeInSqrt vl e -> "Argument of sqrt " ++ showFullExpr vl e ++ " evaluated to a value less than 0"
      NegativeInPower vl e -> "Base of exponentiation " ++ showFullExpr vl e ++ " evaluated to a value less than 0"
      ZeroInDivision vl e -> "Divisor " ++ showFullExpr vl e ++ " is evaluated to 0"
      VariableNotFound var -> "Variable '" ++ var ++ "' is not assigned a value!"
    where showFullExpr varList expr = "'" ++ showVarList varList ++ " " ++ show expr ++ "'"

type EvalResult t = Either (Error t) t
