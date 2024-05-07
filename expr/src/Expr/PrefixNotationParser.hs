module Expr.PrefixNotationParser(parseExpr) where

import Parser

import Expr.Data(Expr(..))
import Control.Applicative(many, (<|>))
import Control.Monad(unless)

keywords :: [String]
keywords = ["sqrt"]

parseBinopChar :: Char -> (Expr a -> Expr a -> Expr a) -> Parser (Expr a -> Expr a -> Expr a)
parseBinopChar expectedChar f = do
    c <- satisfies "Expected binary operator (\'+\', \'-\', \'*\', \'/\' or \'^\')." (== expectedChar)
    return f

parseOneOfBinopChars :: Parser (Expr a -> Expr a -> Expr a)
parseOneOfBinopChars = parseBinopChar '+' Add <|> parseBinopChar '-' Sub <|> parseBinopChar '*' Mul <|> parseBinopChar '/' Div <|> parseBinopChar '^' Pow

parseBinop :: Num a => Parser (Expr a)
parseBinop = do
    binop <- parseOneOfBinopChars
    parseWhitespace
    e <- parseSubexpr
    binop e <$> (parseWhitespace >> parseSubexpr)


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
