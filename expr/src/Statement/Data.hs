
module Statement.Data where

import Expr.Data ( Expr )

data Statement a = Let String (Expr a) | Env | Eval (Expr a)

newtype StatementError = StatementError String

instance Show StatementError where
    show (StatementError str) = str

