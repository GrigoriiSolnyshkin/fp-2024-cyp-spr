{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module State(State(..), execState, get, put, modify) where

newtype State s a = State { runState :: s -> (s, a)}

instance Functor (State s) where
  fmap f state = State $ \t -> case runState state t of
    (newState, res) -> (newState, f res)

instance Applicative (State s) where
  pure r = State $ \t -> (t, r)
  (<*>) funcState state = State $ \t -> case runState funcState t of
    (newT, funcRes) -> case runState state newT of
      (newT', res) -> (newT', funcRes res)

instance Monad (State s) where
  (>>=) state nextState = State $ \t -> case runState state t of
    (newT, res) -> runState (nextState res) newT

execState :: State s a -> s -> a
execState state s = snd $ runState state s

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put t = State $ const (t, ())

modify :: (s -> s) -> State s ()
modify f = do
    t <- get
    put $ f t
    return ()


