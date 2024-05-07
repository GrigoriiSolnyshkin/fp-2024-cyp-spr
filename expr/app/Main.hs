
import Control.Monad.Trans.State

import Control.Monad.Trans.Class

import Expr.Data
import Expr.Eval
import qualified Data.Map.Strict as M

import Statement.Data
import Statement.Eval

import Statement.Parser
import Parser


printEnv :: (Show a, Num a) => [(String, a)] -> IO ()
printEnv [] = return ()
printEnv ((s, v):xs) = do
    putStrLn (s ++ " <- " ++ show v)
    printEnv xs

step :: StateT (VarList Double) IO ()
step = do
    line <- lift getLine
    let stmt = parseStatement line
    case stmt of
        Left error -> lift $ putStrLn error
        Right Env -> do
            env <- get
            lift $ printEnv $ M.toList env
        Right (Eval e) -> do
            env <- get
            case runStateT (evalEval e) env of
                Right (res, newEnv) -> do
                    lift $ print res
                    put newEnv
                Left error -> lift $ print error
        Right (Let x e) -> do
            env <- get
            case runStateT (evalLet x e) env of
                Right (res, newEnv) -> put newEnv
                Left error -> lift $ print error

steps :: StateT (VarList Double) IO ()
steps = do
    step
    steps

main :: IO ()
main = evalStateT steps M.empty