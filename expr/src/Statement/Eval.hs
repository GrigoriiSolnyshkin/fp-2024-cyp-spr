module Statement.Eval where

import Control.Monad.Trans.State
import Control.Monad.Trans.Class

import Expr.Data
import Expr.Eval
import qualified Data.Map.Strict as M

evalLet :: (Floating t, Ord t) => String -> Expr t -> StateT (VarList t) (Either (Error t)) ()
evalLet s e = do
    res <- evalMonadic e
    modify $ M.insert s res

evalEval :: (Floating t, Ord t) => Expr t -> StateT (VarList t) (Either (Error t)) t
evalEval = evalMonadic

evalEnv :: (Floating t, Ord t) => StateT (VarList t) (Either (Error t)) [(String, t)]
evalEnv = M.toList <$> get

