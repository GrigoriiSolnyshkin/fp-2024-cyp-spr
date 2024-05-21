module Description.Parser where

import Description.Data

import Text.Parsec
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Char as Chr
import qualified Data.Set as S

import qualified Control.Monad.Trans.State as St
import qualified Control.Monad.Trans as T
import Data.Functor.Identity(Identity(..))

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

lexer = Tok.makeTokenParser descLanguage

reserved s = try $ do
    ident <- identifier 
    if ident == s
    then return ()
    else fail $ s ++ " expected."

identifier = Tok.identifier lexer

parseModifier = try $ (do
    reserved "final"
    return Final) <|> (do
    reserved "initial"
    return Initial)

parseModifiers = many parseModifier

parseState = do
    mods <- parseModifiers
    reserved "state"
    ident <- identifier
    let astate = AutomataState ident
    st <- getState
    case containsAState astate st of
        False -> do
            modifyState (graceAddAState astate)

            if elem Final mods
                then modifyState (forceMarkFinal astate)
                else return ()

            if elem Initial mods
                then modifyState (forceSetInitial astate)
                else return ()
        True ->
            fail "State redeclaration."

parseName = do
    reserved "name"
    ident <- identifier
    st <- getState
    if getAName st == ""
    then modifyState (\d -> d {getAName = ident})
    else fail "Name redeclaration."

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
    
    if not $ S.member c $ alphabet st
        then fail "Character is not present in the alphabet."
        else return ()
    
    if not $ containsAState is st
        then fail "Initial state is not defined."
        else return ()

    if not $ containsAState fs st
        then fail "Final state is not defined."
        else return ()

    if containsARule is c st
        then fail "Rule with this initial state and this character is already defined."
        else return ()

    modifyState (forceAddARule $ AutomataRule is c fs)

parseAlphabet = do
    reserved "alphabet"
    characters <- identifier
    st <- getState
    if not $ null $ alphabet st
    then fail "Alphabet redeclaration."
    else modifyState (\d -> d {alphabet = S.fromList characters})

parseSingle = parseState <|> parseAlphabet <|> parseTransition <|> parseName

finalCheck = do
    st <- getState

    case initial st of
        Nothing -> fail "Initial state is not defined."
        _ -> return ()

    case getAName st of
        "" -> fail "Name is not defined."
        _ -> return ()

    if null $ alphabet st
        then fail "Alphabet was not declared."
        else return ()

parseWhole = do
    Tok.whiteSpace lexer
    go
    st <- getState
    return st
  where
    go = (do {parseSingle; Tok.semi lexer; go}) <|> (do {eof; finalCheck})
    

-- testing = runParser parseWhole newADesc "" "name new; alphabet 05; initial final state init; state;"