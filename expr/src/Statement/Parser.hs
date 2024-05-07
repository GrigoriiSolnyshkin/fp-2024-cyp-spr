module Statement.Parser where

import Parser
import Expr.Data (Expr)
import Statement.Data (Statement(..))
import Expr.UberParser (parseExprMonadic)
import Control.Applicative


parseDirective :: String -> Parser ()
parseDirective s = parseWithWhitespaces $ do
    satisfies "Expected directive." (== ':')
    parseString s
    return ()

parseLet :: Num a => Parser (Statement a)
parseLet = do
    parseDirective "let"
    var <- parseWithWhitespaces $ parseIdent ["sqrt"]
    Let var <$> parseExprMonadic

parseEval :: Num a => Parser (Statement a)
parseEval = do
    parseDirective "eval"
    Eval <$> parseExprMonadic

parseEnv :: Parser (Statement a)
parseEnv = do
    parseDirective "env"
    parseEmptySuffix
    return Env

parseStatement :: Num a => Parser (Statement a)
parseStatement = parseLet <|> parseEval <|> parseEnv
