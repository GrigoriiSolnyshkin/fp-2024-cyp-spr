{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use void" #-}
module Expr.Parser(parseExpr) where

import Parser(Parser(..), ParseError(..))

import Expr.Data(Expr(..))
import Data.Char(isAlpha, isAlphaNum, isDigit)
import Control.Applicative(many, (<|>))

keywords = ["sqrt"]

isAlphaNum' :: Char -> Bool
isAlphaNum' c = isAlphaNum c || c == '\''

satisfies :: (Char -> Bool) -> Parser Char
satisfies p = Parser $ \s -> case s of
    [] -> Left $ ParseError "Empty string does not satisfy predicate for any char."
    h:t -> if p h then Right (t, h) else Left $ ParseError ("Character '" ++ h : "' does not satisfy a predicate.")

parsingError :: String -> Parser a
parsingError msg = Parser $ \s -> Left $ ParseError msg

parseWord :: Parser String
parseWord = do
    c <- satisfies isAlpha
    r <- many (satisfies isAlphaNum')
    return (c:r)

parseIdent :: Parser String
parseIdent = do
    ident <- parseWord
    if ident `elem` keywords then parsingError $ ident ++ " cannot be used as identifier as it is a keyword." else return ident

parseNum :: Num a => Parser a
parseNum = do
    c <- satisfies isDigit
    r <- many (satisfies isDigit)
    return $ fromInteger (read $ c:r)

parseKeyword :: Parser String
parseKeyword = do
    keyword <- parseWord
    if keyword `elem` keywords then return keyword else parsingError $ keyword ++ " is not a keyword."

parseWhitespace :: Parser ()
parseWhitespace = () <$ satisfies (== ' ')

parseEmptyString :: Parser ()
parseEmptyString = Parser $ \s -> case s of
    "" -> Right (s, ())
    _ -> Left $ ParseError "Parsed string is not empty."

parseBinopChar :: Char -> (Expr a -> Expr a -> Expr a) -> Parser (Expr a -> Expr a -> Expr a)
parseBinopChar expectedChar f = do
    c <- satisfies (== expectedChar)
    return f

parseOneOfBinopChars :: Parser (Expr a -> Expr a -> Expr a)
parseOneOfBinopChars = parseBinopChar '+' Add <|> parseBinopChar '-' Sub <|> parseBinopChar '*' Mul <|> parseBinopChar '/' Div <|> parseBinopChar '^' Pow

parseBinop :: Num a => Parser (Expr a)
parseBinop = do
    binop <- parseOneOfBinopChars
    parseWhitespace
    e <- parseSubexpr
    binop e <$> (parseWhitespace >> parseSubexpr)


parseSubexpr :: Num a => Parser (Expr a)
parseSubexpr = (Lit <$> parseNum) <|> (Var <$> parseIdent) <|> parseBinop <|> (do
    ident <- parseKeyword
    parseWhitespace
    Sqr <$> parseSubexpr
    )

parseExprMonadic :: Num a => Parser (Expr a)
parseExprMonadic = do
    e <- parseSubexpr
    parseEmptyString
    return e

parseExpr :: Num a => String -> Either ParseError (Expr a)
parseExpr s = case tryParse parseExprMonadic s of
    Right (_, result) -> Right result
    Left left -> Left left
