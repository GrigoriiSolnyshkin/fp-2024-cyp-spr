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

parseString :: String -> Parser String
parseString str = do
    go str
  where
    go :: String -> Parser String
    go [] = return str
    go (x:xs) = do
      satisfies ("Expected \'" ++ x:"\'.") (== x)
      go xs

parseOneOfStrings :: [String] -> Parser String
parseOneOfStrings strs = parseWithErrorReplacement ("Expected one of following tokens: " ++ listShow strs ++ ".") $ foldr (\str parser -> parser <|> parseString str) empty strs
  where
    listShow [x] = show x
    listShow (x:xs) = "\'" ++ show x ++ "\' " ++ listShow xs

parseInBrackets :: String -> String -> Parser a -> Parser a
parseInBrackets openingBracket closingBracket parser = do
  parseString openingBracket
  result <- parser
  parseString closingBracket
  return result

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

parseWhitespaces :: Parser ()
parseWhitespaces = do
  many parseWhitespace
  return ()

parseWithWhitespacesPrefix :: Parser a -> Parser a
parseWithWhitespacesPrefix parser = Parser $ \s -> case tryParse parseWhitespaces s of
    Right (suff, loc, _) -> case tryParse parser suff of
      Right (suff', loc', result) -> Right (suff', loc + loc', result)
      Left (ParseError msg loc') -> if loc' == 0 then Left $ ParseError msg 0 else Left $ ParseError msg (loc + loc')

parseWithWhitespaces :: Parser a -> Parser a
parseWithWhitespaces parser = do
  value <- parseWithWhitespacesPrefix parser
  parseWhitespaces
  return value

parseEmptySuffix :: Parser ()
parseEmptySuffix = Parser $ \s -> case s of
    "" -> Right (s, 0, ())
    _ -> parsingError "Suffix is not empty."

data Associativity = LeftAssoc | RightAssoc | NonAssoc deriving (Show, Eq)

data PriorityLevel a = Unary [(String, a -> a)] | Binary Associativity [(String, a -> a -> a)]

parseBinops :: Parser a -> [PriorityLevel a] -> Parser a
parseBinops = foldr applyLevel
  where
    parseOperatorAndArgument :: Parser a -> [String] -> Parser (String, a)
    parseOperatorAndArgument parser operators = do
      opString <- parseWithWhitespaces $ parseOneOfStrings operators
      argument <- parseWithWhitespaces parser
      return (opString, argument)

    lookupJust :: String -> [(String, a)] -> a
    lookupJust s xs = case lookup s xs of
      Just res -> res

    applyLevel :: PriorityLevel a -> Parser a -> Parser a
    applyLevel (Binary assoc ops) prevParser = do
      arg0 <- parseWithWhitespaces prevParser
      opsWithArgs <- parseWithWhitespaces $ many (parseOperatorAndArgument prevParser (map fst ops))
      case assoc of
        LeftAssoc -> return $ foldl (\acc (opString, arg) -> lookupJust opString ops acc arg) arg0 opsWithArgs
        RightAssoc -> return $ if null opsWithArgs then arg0 else let
            args = arg0 : map snd opsWithArgs
            opsWithArgs' = zip (map fst opsWithArgs) args
          in foldr (\(opString, arg) acc -> lookupJust opString ops arg acc) (last args) opsWithArgs'
        NonAssoc -> case opsWithArgs of
          [] -> return arg0
          [(opString, arg1)] -> return $ lookupJust opString ops arg0 arg1
          _ -> parseWithError "Two or more non-associative binary operators in a row."
    applyLevel (Unary ops) prevParser = do
      opsList <- many $ parseWithWhitespaces $ parseOneOfStrings $ map fst ops
      argument <- parseWithWhitespaces prevParser
      return $ foldr (`lookupJust` ops) argument opsList
