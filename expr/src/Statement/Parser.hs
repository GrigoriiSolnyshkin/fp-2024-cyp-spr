module Statement.Parser(parseStatement, parseStatementMonadic) where

import Parser
import Expr.Data (Expr)
import Statement.Data (Statement(..))
import Expr.UberParser (parseExprMonadic)
import Control.Applicative


parseDirective :: String -> Parser ()
parseDirective s = parseWithWhitespaces $ do
    satisfies "Expected directive." (== ':')
    parseWithErrorReplacement "Uknown directive." $ parseString s
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

parseStatementMonadic :: Num a => Parser (Statement a)
parseStatementMonadic = parseLet <|> parseEval <|> parseEnv

parseStatement :: Num a => String -> Either String (Statement a)
parseStatement s = case tryParse parseStatementMonadic s of
    Right (_, _, result) -> Right result
    Left left -> Left $ show left
