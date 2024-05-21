module Description.Editor (editDescription, constructDescription, Action(..)) where

import Description.Data

import Control.Monad.Trans.State
import Control.Monad.Trans

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Data.Char(isAlphaNum)
import qualified Data.Maybe as MB
import Utils(trim, makeApp)

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

data Action = Abort | StoreToMemory | StoreToFile | Continue deriving Eq

helpString :: String
helpString = "Usage: \n\
             \ help: this reminder\n\
             \ as n: adds state with name nm to the automaton if it has not been added yet\n\
             \ rs n: removes state with name n from the automaton\n\
             \ rsf n: removes state with name n from the automaton ignoring warnings\n\
             \ ar is c fs: adds transition from the state is to the state fs by the character c to the automaton\n\
             \ arf is c fs: adds transition ignoring warnings \n\
             \ rr is c: removes transition from the state is by the character c\n\
             \ mf s: marks state s as final\n\
             \ mff s: marks state s as final ignoring warnings\n\
             \ uf s: marks state s as non-final\n\
             \ si s: (re)sets initial state to s\n\
             \ sif s: (re)sets initial state to s ignoring warninngs\n\
             \ list: lists all states and transitions to the output \n\
             \ list s: lists all transitions from the state s\n\
             \ abort: exits editing mode with all changes lost\n\
             \ store: exits editing mode with the automaton stored in the memory\n\
             \ fstore: exits editing mode with the automaton stored in the file"

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
        (IsInitial, _) -> lift $ putStrLn "The state is initial. Consider using 'rsf' if you want to discard initial state."
        (_, newS) -> put newS
    _ -> lift $ putStrLn "'rs' requires one argument."
  return Continue

mf :: [String] -> StateT AutomataDesc IO Action
mf args = do
  case args of
    [name] -> do
      s <- get
      case graceMarkFinal (AutomataState name) s of
        (NoSuchState, _) -> lift $ putStrLn "There is no such state. Consider using 'mff' if you want to add state as well."
        (_, newS) -> put newS
    _ -> lift $ putStrLn "'mf' requires one argument."
  return Continue

mff :: [String] -> StateT AutomataDesc IO Action
mff args = do
  case args of
    [name] -> modify (forceMarkFinal $ AutomataState name)
    _ -> lift $ putStrLn "'mff' requires one argument."
  return Continue

si :: [String] -> StateT AutomataDesc IO Action
si args = do
  case args of
    [name] -> do
      s <- get
      case graceSetInitial (AutomataState name) s of
        (NoSuchState, _) -> lift $ putStrLn "There is no such state. Consider using 'sif' if you want to add state as well."
        (_, newS) -> put newS
    _ -> lift $ putStrLn "'si' requires one argument."
  return Continue

sif :: [String] -> StateT AutomataDesc IO Action
sif args = do
  case args of
    [name] -> modify (forceSetInitial $ AutomataState name)
    _ -> lift $ putStrLn "'sif' requires one argument."
  return Continue

uf :: [String] -> StateT AutomataDesc IO Action
uf args = do
  case args of
    [name] -> modify (graceUnmarkFinal $ AutomataState name)
    _ -> lift $ putStrLn "'uf' requires one argument."
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
        lift $ goStates (initial s) (finals s) (getAStates s)
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

    goStates _ _ [] = return ()
    goStates is fs (s:ss) = do
      let initialPref = if Just s == is then "initial " else ""
      let finalPref = if S.member s fs then "final " else ""
      putStrLn $ "\t" ++ initialPref ++ finalPref ++ show s
      goStates is fs ss

abort :: [String] -> StateT AutomataDesc IO Action
abort args =
  case args of 
    [] -> return Abort
    _ -> do
      lift $ putStrLn "'abort' requires no arguments."
      return Continue

storeAction :: [String] -> StateT AutomataDesc IO Action
storeAction args =
  case args of 
    [] -> return StoreToMemory
    _ -> do
      lift $ putStrLn "'store' requires no arguments."
      return Continue

fstore :: [String] -> StateT AutomataDesc IO Action
fstore args =
  case args of
    [] -> return StoreToMemory
    _ -> do
      lift $ putStrLn "'fstore' requires no arguments."
      return Continue 

rr :: [String] -> StateT AutomataDesc IO Action
rr args = do
  case args of
    [is, [c]] -> modify (removeARule (AutomataState is) c)
    _ -> lift $ putStrLn "'rr' requires two arguments second of them being a single character."
  return Continue

allCommands :: M.Map String ([String] -> StateT AutomataDesc IO Action)
allCommands = M.fromList [ ("store", storeAction) 
                         , ("fstore", fstore)
                         , ("abort", abort)
                         , ("help", help)
                         , ("list", listAction)
                         , ("ar", ar)
                         , ("rr", rr)
                         , ("arf", arf)
                         , ("as", as)
                         , ("rs", rs)
                         , ("rsf", rsf)
                         , ("mf", mf)
                         , ("mff", mff)
                         , ("uf", uf)
                         , ("si", si)
                         , ("sif", sif)
                         ]


editDescription :: StateT AutomataDesc IO Action
editDescription = makeApp (help []) (("editing " ++) . getAName) Continue allCommands


constructDescription :: StateT AutomataDesc IO Action
constructDescription = do
    name <- lift getName
    modify (\desc -> desc {getAName = name})
    abc <- lift getAlphabet
    modify (\desc -> desc {alphabet = S.fromList abc})
    editDescription
    