{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Runner.Runner(runnerApp) where

import Description.Data

import Control.Monad.Trans.State

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
            \ fullpath s: consumes string s and shows step by step log of the automaton (you can use \"\" for empty string)\n\
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
helpAction [] = do
    stateMsg helpString
    return Continue

abortAction :: [String] -> StateT RunnerEnv IO Action
abortAction [] = return Abort

setAction :: [String] -> StateT RunnerEnv IO Action
setAction [name] = do
    let astate = AutomataState name
    desc <- getADesc
    if containsAState astate desc
        then putAState astate
        else stateMsg "State is not present in the description."
    return Continue

resetAction :: [String] -> StateT RunnerEnv IO Action
resetAction [] = do
    desc <- getADesc
    putMaybeAState $ initial desc
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
fullpathAction [s] = do
    desc <- getADesc
    case checkString desc s of
        Nothing -> stateMsg "String contains characters not belonging to the alphabet."
        Just s -> go s
    return Continue
  where
    go [] = return ()
    go (c:cs) = do
        success <- makeStep True c
        if not success then return () else go cs

checkString :: AutomataDesc -> String -> Maybe String
checkString _ "\"\"" = Just ""
checkString desc s = if any (\c -> not $ S.member c $ alphabet desc) s then Nothing else Just s

resultAction :: [String] -> StateT RunnerEnv IO Action
resultAction [s] = do
    desc <- getADesc
    case checkString desc s of
        Nothing -> stateMsg "String contains characters not belonging to the alphabet."
        Just s -> go s
    return Continue
  where
    go [] = do
        Just astate <- getAState
        desc <- getADesc
        stateMsg $ if S.member astate $ finals desc then "Automaton accepted the string." else "Automaton did not accept the string."
        return ()
    go (c:cs) = do
        success <- makeStep False c
        if not success then return () else go cs

allActions :: M.Map (String, Int) ([String] -> StateT RunnerEnv IO Action)
allActions = M.fromList [ (("fullpath", 1), fullpathAction)
                        , (("result", 1), resultAction)
                        , (("set", 1), setAction)
                        , (("reset", 0), resetAction)
                        , (("abort", 0), abortAction)
                        , (("help", 0), helpAction)
                        ]

runnerApp :: StateT RunnerEnv IO ()
runnerApp = do 
    _ <- makeApp (helpAction []) (\(astate, adesc) -> "running " ++ getAName adesc ++ maybe "" show astate) Continue allActions
    return ()
