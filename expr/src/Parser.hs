module Parser where

import Control.Applicative(Alternative, empty, (<|>))

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


