module Description.Parser(parseDescription) where

import Description.Data

import Text.Parsec
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Char as Chr
import qualified Data.Set as S

import Data.Functor.Identity(Identity(..))

import Control.Monad (when, unless)

data Modifier = Final | Initial deriving (Eq, Show)


descLanguage :: Tok.GenLanguageDef String AutomataDesc Identity
descLanguage = Tok.LanguageDef  { Tok.commentStart    = ""
                                , Tok.commentEnd      = ""
                                , Tok.commentLine     = "//"
                                , Tok.nestedComments  = False
                                , Tok.identStart      = Chr.alphaNum
                                , Tok.identLetter     = Chr.alphaNum
                                , Tok.reservedNames   = []
                                , Tok.reservedOpNames = []
                                , Tok.caseSensitive   = True
                                }

lexer :: Tok.GenTokenParser String AutomataDesc Identity
lexer = Tok.makeTokenParser descLanguage

reserved :: String -> ParsecT String AutomataDesc Identity ()
reserved s = try $ do
    ident <- identifier
    if ident == s
    then return ()
    else fail $ s ++ " expected."

identifier :: ParsecT String AutomataDesc Identity String
identifier = Tok.identifier lexer

parseModifier :: ParsecT String AutomataDesc Identity Modifier
parseModifier = try $ (do
    reserved "final"
    return Final) <|> (do
    reserved "initial"
    return Initial)

parseModifiers :: ParsecT String AutomataDesc Identity [Modifier]
parseModifiers = many parseModifier

parseState :: ParsecT String AutomataDesc Identity ()
parseState = do
    mods <- parseModifiers
    reserved "state"
    ident <- identifier
    let astate = AutomataState ident
    st <- getState
    if containsAState astate st
        then fail "State redeclaration."
        else do
            modifyState (graceAddAState astate)
            when (Final `elem` mods) $ modifyState (forceMarkFinal astate)
            when (Initial `elem` mods) $ modifyState (forceSetInitial astate)

parseName :: ParsecT String AutomataDesc Identity ()
parseName = do
    reserved "name"
    ident <- identifier
    st <- getState
    if getAName st == ""
    then modifyState (\d -> d {getAName = ident})
    else fail "Name redeclaration."

parseTransition :: ParsecT String AutomataDesc Identity ()
parseTransition = do
    reserved "transition"
    isIdent <- identifier
    cIdent <- identifier
    fsIdent <- identifier
    st <- getState
    let is = AutomataState isIdent
    let fs = AutomataState fsIdent

    c <- case cIdent of
        [c] -> return c
        _ -> fail "Transition must be made via single character."

    unless (S.member c $ alphabet st) $ fail "Character is not present in the alphabet."
    unless (containsAState is st) $ fail "Initial state is not defined."
    unless (containsAState fs st) $ fail "Final state is not defined."
    when (containsARule is c st) $ fail "Rule with this initial state and this character is already defined."

    modifyState (forceAddARule $ AutomataRule is c fs)

parseAlphabet :: ParsecT String AutomataDesc Identity ()
parseAlphabet = do
    reserved "alphabet"
    characters <- identifier
    st <- getState
    if not $ null $ alphabet st
    then fail "Alphabet redeclaration."
    else modifyState (\d -> d {alphabet = S.fromList characters})

parseSingle :: ParsecT String AutomataDesc Identity ()
parseSingle = parseState <|> parseAlphabet <|> parseTransition <|> parseName

finalCheck :: ParsecT s AutomataDesc Identity ()
finalCheck = do
    st <- getState

    case initial st of
        Nothing -> fail "Initial state is not defined."
        _ -> return ()

    case getAName st of
        "" -> fail "Name is not defined."
        _ -> return ()

    when (null $ alphabet st) $ fail "Alphabet was not declared."

parseWhole :: ParsecT String AutomataDesc Identity AutomataDesc
parseWhole = do
    Tok.whiteSpace lexer
    go
    getState
  where
    go = (do {parseSingle; _ <- Tok.semi lexer; go}) <|> (do {eof; finalCheck})

parseDescription :: String -> Either String AutomataDesc
parseDescription s = case runParser parseWhole newADesc "" s of
    Left err -> Left $ show err
    Right desc -> Right desc
