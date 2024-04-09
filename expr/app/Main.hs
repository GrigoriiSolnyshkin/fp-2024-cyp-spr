module Main where

import Expr.Data
import Expr.PrefixNotationParser
import Parser

casesNotOk :: [String]
casesNotOk = [ "5a"
             , "5&"
             , "+ 5"
             , "'0"
             , "sqrt"
             , "-5"
             , "+ 4 4 4"
             , "6 6"
             , " "
             , ""
             , " 4"
             , "3 "
             , "16 ^"
             , "- 24"
             , "+ + + + 4 4 4 4"
             , "/ 4 4a"
             , "/4 3"
             , "% 5 2"
             , "sqrt sqrt"
             ]



main :: IO ()
main = do
    go casesNotOk
  where
    go :: [String] -> IO ()
    go [] = do return ()
    go (x:xs) = do
      let Left parseError = parseExpr x
      putStrLn $ show x ++ "\t\t" ++ show parseError
      go xs
