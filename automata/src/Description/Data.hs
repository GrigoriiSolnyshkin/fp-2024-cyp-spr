module Description.Data(
    AutomataState(..),
    AutomataRule(..),
    AutomataDesc(..),
    DescriptionError(..),
    newADesc,
    containsAState,
    containsARule,
    getAStates,
    getARules,
    getBackARules,
    getAllARules,
    getARule,
    graceAddAState,
    graceRemoveAState,
    removeARule,
    graceAddARule,
    forceAddARule,
    forceRemoveAState,
    fromStringsARule
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Maybe as MB

newtype AutomataState = AutomataState { getStateName :: String } deriving (Eq, Ord)

instance Show AutomataState where
    show s = "[" ++ getStateName s ++ "]"

data AutomataRule = AutomataRule { getInitial :: AutomataState, underChar :: Char, getFinal :: AutomataState} deriving (Eq, Ord)

fromStringsARule :: String -> Char -> String -> AutomataRule
fromStringsARule is c fs = AutomataRule (AutomataState is) c (AutomataState fs)

instance Show AutomataRule where
    show r = show (getInitial r) ++ ", '" ++ underChar r:"' -> " ++ show (getFinal r)

data AutomataDesc = AutomataDesc {
    rules :: M.Map AutomataState (M.Map Char AutomataState),
    backRules :: M.Map AutomataState (S.Set AutomataRule),
    alphabet :: S.Set Char,
    getAName :: String
  }

instance Show AutomataDesc where
    show d = foldl (\current rule -> current ++ "\n" ++ show rule) (getAName d) (getAllARules d) 

data DescriptionError = Successful | RuleExists | StateNotEmpty | NoSuchState | NoSuchRule | NoSuchChar deriving (Eq)

newADesc :: String -> AutomataDesc
newADesc = AutomataDesc M.empty M.empty S.empty

containsAState :: AutomataState -> AutomataDesc -> Bool
containsAState s d = M.member s $ rules d

containsARule :: AutomataState -> Char -> AutomataDesc -> Bool
containsARule s c d = M.member c $ MB.fromMaybe M.empty (M.lookup s $ rules d)

getAStates :: AutomataDesc -> [AutomataState]
getAStates = M.keys . rules

getARules :: AutomataState -> AutomataDesc -> Maybe [AutomataRule]
getARules s d = do
    mapView <- M.lookup s $ rules d
    return $ map (uncurry (AutomataRule s)) $ M.toList mapView

getBackARules :: AutomataState -> AutomataDesc -> Maybe [AutomataRule]
getBackARules s d = do
    mapView <- M.lookup s $ backRules d
    return $ S.toList mapView

getAllARules :: AutomataDesc -> [AutomataRule]
getAllARules d = concatMap (S.toList . snd) $ M.toList $ backRules d

getARule :: AutomataState -> Char -> AutomataDesc -> Maybe AutomataRule
getARule s c d = do
    mapView <- M.lookup s $ rules d
    final <- M.lookup c mapView
    return $ AutomataRule s c final

graceAddAState :: AutomataState -> AutomataDesc -> AutomataDesc
graceAddAState s d = AutomataDesc {
    rules = M.insertWith (\_ y -> y) s M.empty $ rules d,
    backRules = M.insertWith (\_ y -> y) s S.empty $ backRules d,
    alphabet = alphabet d,
    getAName = getAName d
} 

innerRemoveAState :: AutomataState -> AutomataDesc -> AutomataDesc
innerRemoveAState s d = AutomataDesc {
    rules = M.delete s $ rules d,
    backRules = M.delete s $ backRules d,
    alphabet = alphabet d,
    getAName = getAName d
}


graceRemoveAState :: AutomataState -> AutomataDesc -> (DescriptionError, AutomataDesc)
graceRemoveAState s d = case M.lookup s $ rules d of
    Nothing -> (NoSuchState, d)
    Just someRules -> case M.lookup s $ backRules d of
        Just someBackRules -> if null someRules && null someBackRules then (Successful, innerRemoveAState s d) else (StateNotEmpty, d)
        _ -> undefined

innerRemoveARule :: AutomataRule -> AutomataDesc -> AutomataDesc
innerRemoveARule rule d = AutomataDesc {
    rules = updateRules rule $ rules d, 
    backRules = updateBackRules rule $ backRules d, 
    getAName = getAName d,
    alphabet = alphabet d
} where
    updateBackRules r = M.update (Just . S.delete r) (getFinal r)
    updateRules r = M.update (Just . M.delete (underChar r)) (getInitial r)


removeARule :: AutomataState -> Char -> AutomataDesc -> AutomataDesc
removeARule s c d = case getARule s c d of
    Nothing -> d
    Just r -> innerRemoveARule r d

innerAddARule :: AutomataRule -> AutomataDesc -> AutomataDesc
innerAddARule rule d = AutomataDesc {
    rules = updateRules rule $ rules d, 
    backRules = updateBackRules rule $ backRules d, 
    alphabet = alphabet d,
    getAName = getAName d
} where
    updateBackRules r = M.update (Just . S.insert r) (getFinal r)
    updateRules r = M.update (Just . M.insert (underChar r) (getFinal r)) (getInitial r)

graceAddARule :: AutomataRule -> AutomataDesc -> (DescriptionError, AutomataDesc)
graceAddARule r d
  | not (containsAState (getInitial r) d) || not (containsAState (getFinal r) d) = (NoSuchState, d)
  | containsARule (getInitial r) (underChar r) d = (RuleExists, d)
  | otherwise = (Successful, innerAddARule r d)

forceAddARule :: AutomataRule -> AutomataDesc -> AutomataDesc
forceAddARule r = innerAddARule r . removeARule (getInitial r) (underChar r) . graceAddAState (getInitial r) . graceAddAState (getFinal r)

forceRemoveAState :: AutomataState -> AutomataDesc -> AutomataDesc
forceRemoveAState s d = innerRemoveAState s $ case (getARules s d, getBackARules s d) of
    (Just rs, Just brs) -> foldr innerRemoveARule (foldr innerRemoveARule d rs) brs
    _ -> d
