module Parser where

import Control.Applicative(Alternative, empty, (<|>))

newtype ParseError = ParseError String deriving (Show, Eq)

newtype Parser a = Parser { tryParse :: String -> Either ParseError (String, a) }

instance Functor Parser where
    fmap f parser = Parser $ \s ->
        case tryParse parser s of
            Right (suff, result) -> Right (suff, f result)
            Left err -> Left err

instance Applicative Parser where
  pure res = Parser $ \s -> Right (s, res)
  (<*>) funcParser parser = Parser $ \s -> case tryParse funcParser s of
    Left err -> Left err
    Right (suff, funcResult) -> case tryParse parser suff of
        Left err -> Left err
        Right (suff', result) -> Right (suff', funcResult result)


instance Monad Parser where 
  (>>=) parser nextParser = Parser $ \s -> case tryParse parser s of
    Left err -> Left err
    Right (suff, result) -> tryParse (nextParser result) suff


instance Alternative Parser where
  empty = Parser $ \s -> Left (ParseError $ "Parser error on string " ++ s)
  (<|>) firstOption secondOption = Parser $ \s -> case tryParse firstOption s of
    Right (suff, result) -> Right (suff, result)
    Left err -> tryParse secondOption s 


