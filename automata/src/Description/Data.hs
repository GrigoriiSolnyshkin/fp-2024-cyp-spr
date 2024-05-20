module Description.Data(AutomataState(..), AutomataRule(..)) where

import qualified Data.Map.Strict as M

newtype AutomataState = AutomataState { getStateName :: String } deriving (Eq)



instance Show AutomataState where
    show s = "[" ++ getStateName s ++ "]"

data AutomataRule = AutomataRule { getInitial :: AutomataState, underChar :: Char, getFinal :: AutomataState } deriving (Eq)


instance Show AutomataRule where
    show r = show (getInitial r) ++ ", '" ++ underChar r:"' -> " ++ show (getFinal r)


