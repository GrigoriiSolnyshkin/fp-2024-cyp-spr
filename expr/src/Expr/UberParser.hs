module Expr.UberParser(parseExpr) where

import Expr.Data
import Parser
import Control.Applicative(many, (<|>))

parsePrimitive :: Num a => Parser (Expr a)
parsePrimitive = do
    many parseWhitespace
    res <- parseInBrackets "(" ")" parseSubexpr <|> (Lit <$> parseNum) <|> (Var <$> parseIdent ["sqrt"]) <|> (do
        parseWithErrorReplacement "Expected sqrt." $ parseString "sqrt"
        many parseWhitespace
        Sqr <$> parseInBrackets "(" ")" parseSubexpr
        )
    many parseWhitespace
    return res

binops :: [(Associativity, [(String, Expr a -> Expr a -> Expr a)])]
binops = [ (LeftAssoc, [("+", Add), ("-", Sub)])
         , (LeftAssoc, [("*", Mul), ("/", Div)])
         , (RightAssoc, [("^", Pow)])
         ]

parseSubexpr :: Num a => Parser (Expr a)
parseSubexpr = parseBinops parsePrimitive binops

parseExprMonadic :: Num a => Parser (Expr a)
parseExprMonadic = do
    many parseWhitespace
    res <- parseSubexpr
    many parseWhitespace
    parseEmptySuffix
    return res

parseExpr :: Num a => String -> Either ParseError (Expr a)
parseExpr s = case tryParse parseExprMonadic s of
    Right (_, _, result) -> Right result
    Left left -> Left left

