module Application(runApplication) where

import Description.Data
import qualified Description.Editor as E

import qualified Data.Map.Strict as M


import Control.Monad.Trans.State
import Control.Monad.Trans

import Control.Exception

import Description.Parser

import Utils(makeApp, stateMsg)
import Runner.Runner (runnerApp)

import System.IO (readFile')

type Storage = M.Map String AutomataDesc

helpString :: String
helpString = "Usage: \n\
             \ help: this reminder \n\
             \ edit nm: edit automaton loaded into memory named nm \n\
             \ store nm: store automaton loaded into memory named nm in the file \n\
             \ load nm: load automaton named nm from the file \n\
             \ new: create new automaton \n\
             \ exit: exit application\n\
             \ run nm: enters run environment for automaton named nm\n\
             \ list: lists all loaded automata"

data Action = Continue | Exit deriving Eq

helpAction :: [String] -> StateT Storage IO Action
helpAction args = do
    case args of
        [] -> lift $ putStrLn helpString
        _ -> lift $ putStrLn "'help' requires no  arguments."
    return Continue

storeAction :: [String] -> StateT Storage IO Action
storeAction args = do
    case args of
        [name] -> do
            storage <- get
            case M.lookup name storage of
                Nothing -> lift $ putStrLn "No automaton loaded with such name."
                Just automaton -> lift $ storeAutomaton automaton
        _ -> lift $ putStrLn "'store' requires one argument."
    return Continue

loadAction :: [String] -> StateT Storage IO Action
loadAction args = do
    case args of
        [name] -> do
            loaded <- lift $ loadAutomaton name
            case loaded of
                Left msg -> lift $ putStrLn msg
                Right automaton -> modify (M.insert name automaton)
        _ -> lift $ putStrLn "'load' requires one argument."
    return Continue

exitAction :: [String] -> StateT Storage IO Action
exitAction args =
    case args of
        [] -> return Exit
        _ -> do
            lift $ putStrLn "'exit' requires no  arguments."
            return Continue

editAction :: [String] -> StateT Storage IO Action
editAction args = do
    case args of
        [name] -> do
            storage <- get
            case M.lookup name storage of
                Nothing -> lift $ putStrLn "No automaton loaded with such name."
                Just automaton -> do
                    (action, automaton) <- lift $ runStateT E.editDescription automaton
                    case action of
                        E.Abort -> return ()
                        E.StoreToMemory -> modify (M.insert name automaton)
                        E.StoreToFile -> lift $ storeAutomaton automaton
                        _ -> undefined
        _ -> lift $ putStrLn "'edit' requires one argument."
    return Continue

newAction :: [String] -> StateT Storage IO Action
newAction args = do
    case args of
        [] -> do
            (action, automaton) <- lift $ runStateT E.constructDescription newADesc
            case action of
                E.Abort -> return ()
                E.StoreToMemory -> modify (M.insert (getAName automaton) automaton)
                E.StoreToFile -> lift $ storeAutomaton automaton
                _ -> undefined
        _ -> lift $ putStrLn "'new' requires no arguments."
    return Continue

safeWriteFile :: String -> String -> IO ()
safeWriteFile path content = writeFile (path ++ ".automaton.txt") content `catch` handleExists
  where
    handleExists :: IOException -> IO ()
    handleExists _ = putStrLn "Unable to access the file with write access."

storeAutomaton :: AutomataDesc -> IO ()
storeAutomaton d = safeWriteFile (getAName d) $ show d

safeReadFile :: String -> IO (Maybe String)
safeReadFile path = fmap Just (readFile' path) `catch` handleExists
  where
    handleExists :: IOException -> IO (Maybe String)
    handleExists _ = return Nothing

loadAutomaton :: String -> IO (Either String AutomataDesc)
loadAutomaton name = do
    s <- safeReadFile $ name ++ ".automaton.txt"
    case s of
        Nothing -> return $ Left "No file with description detected."
        Just desc -> return $ parseDescription desc

listAction :: [String] -> StateT Storage IO Action
listAction args = do
    case args of
        [] -> do
            storage <- get
            let keys = M.keys storage 
            go keys
        _ -> stateMsg "'list' requires no arguments."
    return Continue
  where
    go [] = return ()
    go (name:ss) = do
        stateMsg name
        go ss

runAction :: [String] -> StateT Storage IO Action
runAction args = do
    case args of
        [name] -> do
            storage <- get
            case M.lookup name storage of
                Nothing -> stateMsg "No automaton loaded with such name."
                Just automaton -> do
                    (_, _) <- lift $ runStateT runnerApp (initial automaton, automaton)
                    return ()
        _ -> stateMsg "'run' requires one argument."
    return Continue

allActions :: M.Map (String, Int) ([String] -> StateT Storage IO Action)
allActions = M.fromList [ (("load", 1), loadAction)
                        , (("store", 1), storeAction)
                        , (("help", 0), helpAction)
                        , (("new", 0), newAction)
                        , (("edit", 1), editAction)
                        , (("exit", 0), exitAction)
                        , (("list", 0), listAction)
                        , (("run", 1), runAction)
                        ]

application :: StateT Storage IO Action
application = makeApp (helpAction []) (const "app") Continue allActions

runApplication :: IO ()
runApplication = do
    _ <- runStateT application M.empty
    return ()
