{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Description.Editor (editDescription, constructDescription, Action(..)) where

import Description.Data

import Control.Monad.Trans.State
import Control.Monad.Trans

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Data.Char(isAlphaNum)
import qualified Data.Maybe as MB
import Utils(trim, makeApp, stateMsg)

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
help [] = do
  stateMsg helpString
  return Continue

as :: [String] -> StateT AutomataDesc IO Action
as [name] = do
  modify (graceAddAState $ AutomataState name)
  return Continue

rs :: [String] -> StateT AutomataDesc IO Action
rs [name] = do
  s <- get
  case graceRemoveAState (AutomataState name) s of
    (StateNotEmpty, _) -> stateMsg "There are transitions associated with this state. Consider using 'rsf' if you want to remove them."
    (IsInitial, _) -> stateMsg "The state is initial. Consider using 'rsf' if you want to discard initial state."
    (_, newS) -> put newS
  return Continue

mf :: [String] -> StateT AutomataDesc IO Action
mf [name] = do
  s <- get
  case graceMarkFinal (AutomataState name) s of
    (NoSuchState, _) -> stateMsg "There is no such state. Consider using 'mff' if you want to add state as well."
    (_, newS) -> put newS
  return Continue

mff :: [String] -> StateT AutomataDesc IO Action
mff [name] = do
  modify (forceMarkFinal $ AutomataState name)
  return Continue

si :: [String] -> StateT AutomataDesc IO Action
si [name] = do
  s <- get
  case graceSetInitial (AutomataState name) s of
    (NoSuchState, _) -> stateMsg "There is no such state. Consider using 'sif' if you want to add state as well."
    (_, newS) -> put newS
  return Continue

sif :: [String] -> StateT AutomataDesc IO Action
sif [name] = do
  modify (forceSetInitial $ AutomataState name)
  return Continue

uf :: [String] -> StateT AutomataDesc IO Action
uf [name] = do
  modify (graceUnmarkFinal $ AutomataState name)
  return Continue

rsf :: [String] -> StateT AutomataDesc IO Action
rsf [name] = do
  modify (forceRemoveAState $ AutomataState name)
  return Continue

arf :: [String] -> StateT AutomataDesc IO Action
arf [is, cs, fs] = do
  case cs of
    [c] -> do
      s <- get
      if not $ S.member c (alphabet s)
      then lift $ putStrLn ("'" ++ c:"' is not in the alphabet.")
      else modify (forceAddARule $ fromStringsARule is c fs)
    _ -> lift $ putStrLn "'arf' requires second argument to be a single character."
  return Continue

ar :: [String] -> StateT AutomataDesc IO Action
ar [is, cs, fs] = do
  case cs of
    [c] -> do
      s <- get
      if not $ S.member c (alphabet s)
      then lift $ putStrLn ("'" ++ c:"' is not in the alphabet.")
      else case graceAddARule (fromStringsARule is c fs) s of
        (Successful, newS) -> put newS
        (RuleExists, _) -> lift $ putStrLn "Rule already exists. To rewrite it consider using 'arf'."
        (NoSuchState, _) -> lift $ putStrLn "One of the states does not exist. To create a state automatically consider using 'arf'."
        _ -> undefined
    _ -> lift $ putStrLn "'ar' requires second argument to be a single character."
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
abort [] = return Abort

storeAction :: [String] -> StateT AutomataDesc IO Action
storeAction [] = return StoreToMemory

fstore :: [String] -> StateT AutomataDesc IO Action
fstore [] = return StoreToFile

rr :: [String] -> StateT AutomataDesc IO Action
rr args = do
  case args of
    [is, [c]] -> modify (removeARule (AutomataState is) c)
    _ -> lift $ putStrLn "'rr' requires two arguments second of them being a single character."
  return Continue

allCommands :: M.Map (String, Int) ([String] -> StateT AutomataDesc IO Action)
allCommands = M.fromList [ (("store", 0), storeAction) 
                         , (("fstore", 0), fstore)
                         , (("abort", 0), abort)
                         , (("help", 0), help)
                         , (("list", 0), listAction)
                         , (("list", 1), listAction)
                         , (("ar", 3), ar)
                         , (("rr", 2), rr)
                         , (("arf", 3), arf)
                         , (("as", 1), as)
                         , (("rs", 1), rs)
                         , (("rsf", 1), rsf)
                         , (("mf", 1), mf)
                         , (("mff", 1), mff)
                         , (("uf", 1), uf)
                         , (("si", 1), si)
                         , (("sif", 1), sif)
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
    