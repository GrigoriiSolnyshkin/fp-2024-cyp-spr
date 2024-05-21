module Runner.Runner where

import Description.Data

import Control.Monad.Trans.State
import Control.Monad.Trans

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Utils (stateMsg, makeApp)
import Control.Monad (when)

type RunnerEnv = (Maybe AutomataState, AutomataDesc)


helpString :: String
helpString = "Usage: \n\
            \ help: this reminder\n\
            \ abort: exits to main menu\n\
            \ set s: sets current state to s\n\
            \ reset: sets current state to the initial\n\
            \ fullpath s: consumes string s and shows step by step log of the automaton\n\
            \ result s: consumes string s and shows final result (if it is accepted by the automaton)"

data Action = Continue | Abort deriving (Eq)

putAState :: AutomataState -> StateT RunnerEnv IO ()
putAState new = do
    (_, adesc) <- get
    put (Just new, adesc)

putMaybeAState :: Maybe AutomataState -> StateT RunnerEnv IO ()
putMaybeAState new = do
    (_, adesc) <- get
    put (new, adesc)

discardAState :: Monad m => StateT RunnerEnv m ()
discardAState = do
    (_, adesc) <- get
    put (Nothing, adesc)

getAState :: Monad m => StateT RunnerEnv m (Maybe AutomataState)
getAState = do
    (astate, _) <- get
    return astate

getADesc :: Monad m => StateT RunnerEnv m AutomataDesc
getADesc = do
    (_, adesc) <- get
    return adesc


helpAction :: [String] -> StateT RunnerEnv IO Action
helpAction args = do
    stateMsg $ case args of
        [] -> helpString
        _ ->  "'help' requires no arguments."
    return Continue

abortAction :: [String] -> StateT RunnerEnv IO Action
abortAction args =
    case args of
        [] -> return Abort
        _ ->  do
            stateMsg "'abort' requires no arguments."
            return Continue

setAction :: [String] -> StateT RunnerEnv IO Action
setAction args = do
    case args of
        [name] -> do
            let astate = AutomataState name
            desc <- getADesc
            if containsAState astate desc
                then putAState astate
                else stateMsg "State is not present in the description."
        _ -> stateMsg "'set' requires one argument."
    return Continue

resetAction :: [String] -> StateT RunnerEnv IO Action
resetAction args = do
    case args of
        [] -> do
            desc <- getADesc
            putMaybeAState $ initial desc
        _ -> stateMsg "'reset' requires no positional arguments."
    return Continue

makeStep :: Bool -> Char -> StateT RunnerEnv IO Bool
makeStep toLog newC = do
    maybeAState <- getAState
    desc <- getADesc
    case maybeAState of
        Just astate -> case getARule astate newC desc of
            Nothing -> do
                stateMsg $ "No rule for transition from " ++ show astate ++ " by character" ++ newC:". Computation failed."
                putMaybeAState Nothing
                return False
            Just rule -> do
                putAState $ getFinal rule
                when toLog $ stateMsg $ "Successful transition using rule " ++ show rule ++ "."
                return True
        Nothing -> do
            stateMsg "Please set the state before running automaton."
            return False


fullpathAction :: [String] -> StateT RunnerEnv IO Action
fullpathAction args = do
    case args of
        [s] -> go s
        _ -> stateMsg "'fullpath' requires one argument."
    return Continue
  where
    go [] = return ()
    go (c:cs) = do
        success <- makeStep True c
        if not success then return () else go cs

resultAction :: [String] -> StateT RunnerEnv IO Action
resultAction args = do
    case args of
        [s] -> go s
        _ -> stateMsg "'result' requires one argument."
    return Continue
  where
    go [] = do
        Just astate <- getAState
        desc <- getADesc
        stateMsg $ if S.member astate $ finals desc then "Automaton accepted the string." else "Automaton did not accept the string."
        return ()
    go (c:cs) = do
        success <- makeStep True c
        if not success then return () else go cs

allActions :: M.Map String ([String] -> StateT RunnerEnv IO Action)
allActions = M.fromList [ ("fullpath", fullpathAction)
                        , ("result", resultAction)
                        , ("set", setAction)
                        , ("reset", resetAction)
                        , ("abort", abortAction)
                        , ("help", helpAction)
                        ]
                        
runnerApp :: StateT RunnerEnv IO ()
runnerApp = do 
    _ <- makeApp (helpAction []) (\(astate, adesc) -> "running " ++ getAName adesc ++ maybe "" show astate) Continue allActions
    return ()
