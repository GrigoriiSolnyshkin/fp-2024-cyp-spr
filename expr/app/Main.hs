module Main where

import Expr.Data
import qualified Expr.PrefixNotationParser as PP
import qualified Expr.UberParser as UP
import Parser

casesForPP :: [String]
casesForPP = [ "5a"
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

casesForUP :: [String]
casesForUP = [ "5a", "5&", "5 +", "5 + +", "a++", "^8", "9%9", " 4 * (3 + 3", "4 * 3 + 3)", "7identifier", "sqrt 14", "sqrt(1 4)", "5 / +5"
             , "sqrt(sqrt)", " sqrt", "a a a", "", "   ", "2 ^ ()", "(((( ))))", "7 ^ 7 ^ 7 ^ ?", "\'", "\'a + \'b", "((((((11)))))", "-5 * -5", "+5 ^ +5"
             ]



main :: IO ()
main = do
    go PP.parseExpr casesForPP
    go UP.parseExpr casesForUP
  where
    go _ [] = do return ()
    go parseFunc (x:xs) = do
      let Left parseError = parseFunc x
      putStrLn $ show x ++ "\t\t" ++ show parseError
      go parseFunc xs
