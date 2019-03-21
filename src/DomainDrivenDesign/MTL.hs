{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DomainDrivenDesign.MTL 
    ( Restoring
    , EventSourced
    , rebuildState
    , rebuildAggregate
    , AggregateMonad(..)
    
    , runCounterStack
    ) where

import Control.Monad.State (MonadState, get, modify, put, State, runState)
import Control.Monad.Except (MonadError, throwError, runExceptT)

import Control.Monad.Trans.Except (ExceptT)

newtype Restoring s = Restoring { unRestoring :: s } 
    deriving Functor

class EventSourced st ev | st -> ev where
    initState :: st
    apply :: ev -> Restoring st -> Restoring st

rebuildState :: (EventSourced st ev, Foldable f) => f ev -> st
rebuildState = applyEvents initState

rebuildAggregate 
    :: (AggregateMonad st ev err m, Foldable f) 
    => f ev
    -> m ()
rebuildAggregate ev = put . Versioned version [] . rebuildState $ ev
  where
    version = length ev
        

applyEvents :: (EventSourced st ev, Foldable f) => st -> f ev -> st
applyEvents startState = unRestoring . foldr apply (Restoring startState)

data Versioned st ev = Versioned 
    { version :: Int -- Nat ?
    , pendingEvents :: [ev]
    , aggState :: st
    }

class (EventSourced st ev, MonadState (Versioned st ev) m, MonadError err m)
    => AggregateMonad st ev err m
    | st -> ev
    , st -> err
    where
    raiseEvent :: ev -> m ()
    raiseEvent = raiseEvent'
    getAggregate :: m st
    getAggregate = getAggregate'
    raiseError :: err -> m ()
    raiseError = throwError

pushEvent :: EventSourced st ev => ev -> Versioned st ev -> Versioned st ev
pushEvent ev Versioned{..} = Versioned (version + 1) (ev:pendingEvents) (applyEvents aggState [ev])

getAggregate' :: (EventSourced st ev, MonadState (Versioned st ev) m) => m st 
getAggregate' = fmap aggState get

-- Not cool
--raiseError' :: (EventSourced st ev, MonadError err m) => err -> m ()
--raiseError' = throwError

raiseEvent' :: (EventSourced st ev, MonadState (Versioned st ev) m) => ev -> m ()
raiseEvent' = modify . pushEvent 

instance EventSourced Int Int where
    initState = 0
    apply x = fmap (+x)

newtype CounterStack a = CounterStack { unMonad :: ExceptT Int (State (Versioned Int Int)) a }
    deriving (Functor, Applicative, Monad, MonadError Int, MonadState (Versioned Int Int))

instance AggregateMonad Int Int Int CounterStack

runCounterStack :: Int -> CounterStack a -> Either Int Int
runCounterStack start agg = fmap (const st) $ e
  where
    startVersion = Versioned 0 [] start
    (e, (Versioned _ _ st)) = (runState . runExceptT . unMonad) agg startVersion
