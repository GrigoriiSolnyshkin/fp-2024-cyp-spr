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
    fromStringsARule,
    forceMarkFinal,
    graceMarkFinal,
    graceUnmarkFinal,
    forceSetInitial,
    graceSetInitial
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
    initial :: Maybe AutomataState,
    finals :: S.Set AutomataState,
    alphabet :: S.Set Char,
    getAName :: String
  } deriving Eq

instance Show AutomataDesc where
    show d = "name " ++ getAName d ++ ";\n" ++
             "alphabet " ++ S.toList (alphabet d) ++ ";\n" ++
             foldl (\current s -> current ++ showState s) "" (getAStates d) ++
             foldl (\current r -> current ++ showRule r) "" (getAllARules d)

      where
        showState s = (if S.member s $ finals d then "final " else "") ++ (if Just s == initial d then "initial " else "") ++ "state " ++ (getStateName s) ++ ";\n"
        showRule (AutomataRule is c fs) = "transition " ++ getStateName is ++ " " ++ c:" " ++ getStateName fs ++ ";\n"

data DescriptionError = Successful | RuleExists | StateNotEmpty | NoSuchState | NoSuchRule | IsInitial deriving (Eq)

newADesc :: AutomataDesc
newADesc = AutomataDesc M.empty M.empty Nothing S.empty S.empty ""

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
graceAddAState s d = d {
    rules = M.insertWith (\_ y -> y) s M.empty $ rules d,
    backRules = M.insertWith (\_ y -> y) s S.empty $ backRules d
}

innerRemoveAState :: AutomataState -> AutomataDesc -> AutomataDesc
innerRemoveAState s d = d {
    rules = M.delete s $ rules d,
    backRules = M.delete s $ backRules d,
    initial = if initial d == Just s then Nothing else initial d,
    finals = S.delete s $ finals d
}


graceRemoveAState :: AutomataState -> AutomataDesc -> (DescriptionError, AutomataDesc)
graceRemoveAState s d = case M.lookup s $ rules d of
    Nothing -> (NoSuchState, d)
    Just someRules -> case M.lookup s $ backRules d of
        Just someBackRules | not $ null someRules && null someBackRules -> (StateNotEmpty, d)
        Just _ | initial d == Just s -> (IsInitial, d)
        _ -> (Successful, innerRemoveAState s d)

innerRemoveARule :: AutomataRule -> AutomataDesc -> AutomataDesc
innerRemoveARule rule d = d {
    rules = updateRules rule $ rules d,
    backRules = updateBackRules rule $ backRules d
} where
    updateBackRules r = M.update (Just . S.delete r) (getFinal r)
    updateRules r = M.update (Just . M.delete (underChar r)) (getInitial r)


removeARule :: AutomataState -> Char -> AutomataDesc -> AutomataDesc
removeARule s c d = case getARule s c d of
    Nothing -> d
    Just r -> innerRemoveARule r d

innerAddARule :: AutomataRule -> AutomataDesc -> AutomataDesc
innerAddARule rule d = d {
    rules = updateRules rule $ rules d,
    backRules = updateBackRules rule $ backRules d
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

innerMarkFinal :: AutomataState -> AutomataDesc -> AutomataDesc
innerMarkFinal s d = d {
    finals = S.insert s $ finals d
}

innerSetInitial :: AutomataState -> AutomataDesc -> AutomataDesc
innerSetInitial s d = d {
    initial = Just s
}

forceSetInitial :: AutomataState -> AutomataDesc -> AutomataDesc
forceSetInitial s = innerSetInitial s . graceAddAState s

graceSetInitial :: AutomataState -> AutomataDesc -> (DescriptionError, AutomataDesc)
graceSetInitial s d = if containsAState s d
                      then (Successful, innerSetInitial s d)
                      else (NoSuchState, d)

forceMarkFinal :: AutomataState -> AutomataDesc -> AutomataDesc
forceMarkFinal s = innerMarkFinal s . graceAddAState s

graceMarkFinal :: AutomataState -> AutomataDesc -> (DescriptionError, AutomataDesc)
graceMarkFinal s d = if containsAState s d
                     then (Successful, innerMarkFinal s d)
                     else (NoSuchState, d)

graceUnmarkFinal :: AutomataState -> AutomataDesc -> AutomataDesc
graceUnmarkFinal s d = d {
    finals = S.delete s $ finals d
}