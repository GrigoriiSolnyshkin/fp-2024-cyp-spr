module Expr.UberParser(parseExpr) where

import Expr.Data
import Parser
import Control.Applicative(many, (<|>))

parsePrimitive :: Num a => Parser (Expr a)
parsePrimitive = parseWithErrorReplacementIfNothingParsed "Expected expression." $ do
    res <- parseWithWhitespaces ( parseInBrackets "(" ")" parseSubexpr <|> (Lit <$> parseNum) <|> (Var <$> parseIdent ["sqrt"]) <|> (do
        parseWithErrorReplacement "Expected sqrt." $ parseString "sqrt"
        Sqr <$> parseWithWhitespaces (parseInBrackets "(" ")" parseSubexpr)
        ))
    parseWhitespaces
    return res

binops :: Num a => [PriorityLevel (Expr a)]
binops = [ Binary LeftAssoc [("+", Add), ("-", Sub)]
         , Unary [("+", id), ("-", (0 -))]
         , Binary LeftAssoc [("*", Mul), ("/", Div)]
         , Binary RightAssoc [("^", Pow)]
         ]

parseSubexpr :: Num a => Parser (Expr a)
parseSubexpr = parseBinops parsePrimitive binops

parseExprMonadic :: Num a => Parser (Expr a)
parseExprMonadic = do
    res <- parseWithWhitespaces parseSubexpr
    parseEmptySuffix
    return res

parseExpr :: Num a => String -> Either ParseError (Expr a)
parseExpr s = case tryParse parseExprMonadic s of
    Right (_, _, result) -> Right result
    Left left -> Left left

