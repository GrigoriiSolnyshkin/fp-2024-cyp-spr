module Expr.PrefixNotationParser(parseExpr) where

import Parser

import Expr.Data(Expr(..))
import Control.Applicative(many, (<|>))
import Control.Monad(unless)

keywords :: [String]
keywords = ["sqrt"]

parseBinop :: Num a => Parser (Expr a)
parseBinop = do
    c <- satisfies "Expected binary operator (\'+\', \'-\', \'*\', \'/\' or \'^\')." (`elem` "+-*/^")
    parseWhitespace
    e1 <- parseSubexpr
    parseWhitespace
    e2 <- parseSubexpr
    return $ case c of
        '+' -> Add e1 e2
        '-' -> Sub e1 e2
        '*' -> Mul e1 e2
        '/' -> Div e1 e2
        '^' -> Pow e1 e2


parseSqrt :: Parser ()
parseSqrt = parseWithErrorReplacement "Expected \'sqrt\'" $ do
    keyword <- parseWord
    unless (keyword == "sqrt") parseWithEmptyError

parseSubexpr :: Num a => Parser (Expr a)
parseSubexpr = parseWithErrorReplacementIfNothingParsed "Expected expression." $ (do Lit <$> parseNum) <|> (do Var <$> parseIdent keywords) <|> (do parseBinop) <|> (do
    ident <- parseSqrt
    parseWhitespace
    Sqr <$> parseSubexpr
    )

parseExprMonadic :: Num a => Parser (Expr a)
parseExprMonadic = do
    e <- parseSubexpr
    parseEmptySuffix
    return e

parseExpr :: Num a => String -> Either ParseError (Expr a)
parseExpr s = case tryParse parseExprMonadic s of
    Right (_, _, result) -> Right result
    Left left -> Left left
