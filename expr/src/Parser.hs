{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use void" #-}
module Parser where

import Control.Applicative(Alternative, empty, (<|>), many)
import Data.Char(isAlpha, isAlphaNum, isDigit)

newtype Loc = Loc { getLoc :: Int } deriving (Show, Eq, Ord)

instance Num Loc where
  (+) (Loc a) (Loc b) = Loc (a + b)
  (*) (Loc a) (Loc b) = Loc (a * b)
  abs (Loc a) = Loc (abs a)
  signum (Loc a) = Loc (signum a)
  fromInteger = Loc . fromInteger
  negate (Loc a) = Loc (negate a)

data ParseError = ParseError String Loc deriving (Eq)

instance Show ParseError where
  show (ParseError msg (Loc loc)) = "Parser error at position " ++ show loc ++ ": " ++ msg

newtype Parser a = Parser { tryParse :: String -> Either ParseError (String, Loc, a) }

instance Functor Parser where
    fmap f parser = Parser $ \s ->
        case tryParse parser s of
            Right (suff, loc, result) -> Right (suff, loc, f result)
            Left error -> Left error

instance Applicative Parser where
  pure res = Parser $ \s -> Right (s, Loc 0, res)
  (<*>) funcParser parser = Parser $ \s -> case tryParse funcParser s of
    Left err -> Left err
    Right (suff, loc, funcResult) -> case tryParse parser suff of
        Left (ParseError msg loc') -> Left (ParseError msg $ loc + loc')
        Right (suff', loc', result) -> Right (suff', loc + loc', funcResult result)


instance Monad Parser where 
  (>>=) parser nextParser = Parser $ \s -> case tryParse parser s of
    Left err -> Left err
    Right (suff, loc, result) -> case tryParse (nextParser result) suff of
      Left (ParseError msg loc') -> Left (ParseError msg $ loc + loc')
      Right (suff', loc', result) -> Right (suff', loc + loc', result)


instance Alternative Parser where
  empty = Parser $ \s -> Left $ ParseError "Unknown error." 0
  (<|>) firstOption secondOption = Parser $ \s -> case tryParse firstOption s of
    res@(Right (suff, loc, result)) -> res
    err@(Left (ParseError msg loc)) -> case tryParse secondOption s of
      res@(Right (suff, loc, result)) -> res
      err'@(Left (ParseError msg' loc')) -> if loc > loc' then err else err'

satisfies :: String -> (Char -> Bool) -> Parser Char
satisfies description p = Parser $ \s -> case s of
    [] -> parsingError description
    h:t -> if p h then Right (t, 1, h) else parsingError description

parsingError :: String -> Either ParseError b
parsingError msg = Left $ ParseError msg 0

parseWithError :: String -> Parser a
parseWithError msg = Parser $ \s -> parsingError msg

parseWithEmptyError :: Parser a
parseWithEmptyError = parseWithError ""

parseWithErrorReplacement :: String -> Parser a -> Parser a
parseWithErrorReplacement msg = parseWithErrorReplacementIf msg (const True)

parseWithErrorReplacementIf :: String -> (ParseError -> Bool) -> Parser a -> Parser a
parseWithErrorReplacementIf msg predicate parser = Parser $ \s -> case tryParse parser s of
    res@(Right _) -> res
    err@(Left parseError) -> if predicate parseError then parsingError msg else err

parseWithErrorReplacementIfNothingParsed :: String -> Parser a -> Parser a
parseWithErrorReplacementIfNothingParsed msg = parseWithErrorReplacementIf msg (\(ParseError _ loc) -> loc == 0)


parseWord :: Parser String
parseWord = do
    c <- satisfies "Expected Latin letter." isAlpha
    r <- many (satisfies "Expected Latin letter, digit or \''\'." isAlphaNum')
    return (c:r)
  where
    isAlphaNum' c = isAlphaNum c || c == '\''

parseIdent :: [String] -> Parser String
parseIdent reservedWords = parseWithErrorReplacementIfNothingParsed "Expected identifier." $ do
    ident <- parseWord
    if ident `elem` reservedWords then parseWithError $ ident ++ " cannot be used as identifier as it is a keyword." else return ident
    

parseNum :: Num a => Parser a
parseNum = parseWithErrorReplacement "Expected number." $ do
    c <- satisfies "Expected digit." isDigit
    r <- many (satisfies "Expected digit." isDigit)
    return $ fromInteger (read $ c:r)

parseWhitespace :: Parser ()
parseWhitespace = () <$ satisfies "Expected whitespace." (== ' ')

parseEmptyString :: Parser ()
parseEmptyString = Parser $ \s -> case s of
    "" -> Right (s, 0, ())
    _ -> parsingError "Suffix is not empty."
