module Lambda.Parser(parseTerm) where

import Lambda.Data
import Parser
import Control.Applicative (many, (<|>))

parseSubterm :: Parser LambdaTerm
parseSubterm = do
    arg0 <- parseNonApplication
    applicationRow <- many parseNonApplication
    return $ foldl Application arg0 applicationRow

parseNonApplication :: Parser LambdaTerm
parseNonApplication = parseWithErrorReplacementIfNothingParsed "Expected term." $ parseWithWhitespaces $ Primitive <$> parseWord <|> parseInBrackets "(" ")" parseSubterm <|> (do
    satisfies "Expected abstraction." (== '\\')
    abstractions <- many (parseWithWhitespaces parseWord)
    case abstractions of
        [] -> parseWithError "Expected abstraction variable."
        _ -> do
          satisfies "Expected '.'." (== '.')
          term <- parseSubterm
          return $ foldr Abstraction term abstractions
  )

parseTermMonadic :: Parser LambdaTerm
parseTermMonadic = do
    result <- parseSubterm
    parseEmptySuffix
    return result

parseTerm :: String -> Either ParseError LambdaTerm
parseTerm s = case tryParse parseTermMonadic s of
    Right (_, _, result) -> Right result
    Left left -> Left left
