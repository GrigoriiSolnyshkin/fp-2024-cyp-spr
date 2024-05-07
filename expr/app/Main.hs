module Main where

import Lambda.Parser
import Parser

main :: IO ()
main = do
    putStrLn "Enter lambda terms one by one. They will be parsed and printed prettily."
    go
  where
    go = do
      str <- getLine
      case parseTerm str of
        Left error -> print error
        Right success -> print success
      go
