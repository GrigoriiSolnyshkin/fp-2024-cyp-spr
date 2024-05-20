module Description.FromIO (editDescription, constructDescription, Action(..)) where

import Description.Data

import Control.Monad.Trans.State
import Control.Monad.Trans

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Data.Char(isSpace, isAlphaNum)

import Data.List ( dropWhileEnd )
import qualified Data.Maybe as MB


trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

getName :: IO String
getName = do
    putStrLn "Enter name for your automaton."
    go
  where
    go = do
        name <- getLine
        case trim name of
          "" -> do
            putStrLn "Name must not be empty."
            go
          s -> return s

getAlphabet :: IO String
getAlphabet = do
    putStrLn "Enter characters belonging to your alphabet. An alphabet is allowed to contain alphanumerical characters only. All other characters will be ignored."
    go
  where
    go = do
        abc <- getLine
        case filter isAlphaNum abc of
            "" -> do
              putStrLn "Alphabet must not be empty."
              go
            s -> return s

data Action = Abort | LoadToMemory | LoadToFile | Continue

helpString :: String
helpString = "Usage: \n\
             \ help: this reminder\n\
             \ as n: adds state with name nm to the automaton if it has not been added yet\n\
             \ rs n: removes state with name n from the automaton\n\
             \ rsf n: removes state with name n from the automaton ignoring warnings\n\
             \ ar is c fs: adds transition from the state is to the state fs by the character c to the automaton\n\
             \ arf is c fs: adds transition ignoring warnings \n\
             \ rr is c: removes transition from the state is by the character c\n\
             \ list: lists all states and transitions to the output \n\
             \ list s: lists all transitions from the state s\n\
             \ abort: exits editing mode with all changes lost\n\
             \ load: exits editing mode with the automaton loaded to the memory\n\
             \ fload: exits editing mode with the automaton loaded to the file"

help :: [String] -> StateT AutomataDesc IO Action
help args = do
  case args of 
    [] -> lift $ putStrLn helpString
    _ -> lift $ putStrLn "'help' requires no arguments."
  return Continue

as :: [String] -> StateT AutomataDesc IO Action
as args = do
  case args of
    [name] -> modify (graceAddAState $ AutomataState name)
    _ -> lift $ putStrLn "'as' requires one argument."
  return Continue

rs :: [String] -> StateT AutomataDesc IO Action
rs args = do
  case args of
    [name] -> do
      s <- get
      case graceRemoveAState (AutomataState name) s of
        (StateNotEmpty, _) -> lift $ putStrLn "There are transitions associated with this state. Consider using 'rsf' if you want to remove them."
        (_, newS) -> put newS
    _ -> lift $ putStrLn "'rs' requires one argument."
  return Continue

rsf :: [String] -> StateT AutomataDesc IO Action
rsf args = do
  case args of
    [name] -> modify (forceRemoveAState $ AutomataState name)
    _ -> lift $ putStrLn "'rsf' requires one argument."
  return Continue

arf :: [String] -> StateT AutomataDesc IO Action
arf args = do
  case args of
    [is, [c], fs] -> do
      s <- get
      if not $ S.member c (alphabet s)
      then lift $ putStrLn ("'" ++ c:"' is not in the alphabet.")
      else modify (forceAddARule $ fromStringsARule is c fs)
    _ -> lift $ putStrLn "'arf' requires three arguments second of them being a single character."
  return Continue

ar :: [String] -> StateT AutomataDesc IO Action
ar args = do
  case args of
    [is, [c], fs] -> do
      s <- get
      if not $ S.member c (alphabet s)
      then lift $ putStrLn ("'" ++ c:"' is not in the alphabet.")
      else case graceAddARule (fromStringsARule is c fs) s of
        (Successful, newS) -> put newS
        (RuleExists, _) -> lift $ putStrLn "Rule already exists. To rewrite it consider using 'arf'."
        (NoSuchState, _) -> lift $ putStrLn "One of the states does not exist. To create a state automatically consider using 'arf'."
        _ -> undefined
    _ -> lift $ putStrLn "'ar' requires three arguments second of them being a single character."
  return Continue


listAction :: [String] -> StateT AutomataDesc IO Action
listAction args = do
    s <- get
    case args of 
      [] -> do
        lift $ putStrLn "States:"
        lift $ goStates $ getAStates s
        lift $ putStrLn "Transitions:"
        lift $ goRules $ getAllARules s
      [st] -> do
        lift $ putStrLn "Transitions:"
        lift $ goRules $ MB.fromMaybe [] $ getARules (AutomataState st) s

      _ -> lift $ putStrLn "'list' requires one or no arguments."
    return Continue
  where
    goRules [] = return ()
    goRules (r:rs) = do
      putStrLn $ "\t" ++ show r
      goRules rs

    goStates [] = return ()
    goStates (s:ss) = do
      putStrLn $ "\t" ++ show s
      goStates ss

abort :: [String] -> StateT AutomataDesc IO Action
abort args =
  case args of 
    [] -> return Abort
    _ -> do
      lift $ putStrLn "'abort' requires no arguments."
      return Continue

load :: [String] -> StateT AutomataDesc IO Action
load args =
  case args of 
    [] -> return LoadToMemory
    _ -> do
      lift $ putStrLn "'load' requires no arguments."
      return Continue

fload :: [String] -> StateT AutomataDesc IO Action
fload args =
  case args of
    [] -> return LoadToMemory
    _ -> do
      lift $ putStrLn "'fload' requires no arguments."
      return Continue 

rr :: [String] -> StateT AutomataDesc IO Action
rr args = do
  case args of
    [is, [c]] -> modify (removeARule (AutomataState is) c)
    _ -> lift $ putStrLn "'rr' requires two arguments second of them being a single character."
  return Continue

allCommands :: M.Map String ([String] -> StateT AutomataDesc IO Action)
allCommands = M.fromList [ ("load", load) 
                         , ("fload", fload)
                         , ("abort", abort)
                         , ("help", help)
                         , ("list", listAction)
                         , ("ar", ar)
                         , ("rr", rr)
                         , ("arf", arf)
                         , ("as", as)
                         , ("rs", rs)
                         , ("rsf", rsf)
                         ]


editDescription :: StateT AutomataDesc IO Action
editDescription = do
  command <- lift getLine
  case words command of
    [] -> editDescription
    arg:args -> case M.lookup arg allCommands of
      Just f -> do
        result <- f args
        case result of
          Continue -> editDescription
          _ -> return result
      _ -> do
        lift $ putStrLn "Unrecognized command."
        editDescription



constructDescription :: StateT AutomataDesc IO Action
constructDescription = do
    name <- lift getName
    modify (\desc -> desc {getAName = name})
    abc <- lift getAlphabet
    modify (\desc -> desc {alphabet = S.fromList abc})
    editDescription
    