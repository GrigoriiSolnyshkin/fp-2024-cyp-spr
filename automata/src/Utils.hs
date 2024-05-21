module Utils(trim, makeApp, stateMsg) where

import qualified Data.Map.Strict as M
import Control.Monad.Trans.State
import Control.Monad.Trans

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import System.IO (hFlush, stdout)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

stateMsg :: String -> StateT a IO ()
stateMsg = lift . putStrLn

makeApp :: Eq b => (StateT a) IO c -> (a -> String) -> b -> M.Map (String, Int) ([String] -> (StateT a) IO b) -> (StateT a) IO b
makeApp welcome prefix contAction commands = do
  _ <- welcome
  go
  where
    go = do
      s <- get
      lift $ putStr $ "(" ++ prefix s ++ ") "
      lift $ hFlush stdout
      command <- lift getLine
      case words command of
        [] -> go
        arg:args -> case M.lookup (arg, length args) commands of
          Just f -> do
            result <- f args
            if result == contAction
            then go
            else return result
          _ -> do
            lift $ putStrLn "Unrecognized command and/or number of its arguments."
            go
