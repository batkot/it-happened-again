{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DomainDrivenDesign.MTL 
    ( rebuildAggregate
    , AggregateMonad(..)
    , AggregateActionT
    
    , runAggregate
    ) where

import Control.Arrow ((&&&))

import Control.Monad.State (MonadState, get, modify, put, runStateT)
import Control.Monad.Except (MonadError, throwError, runExceptT)

import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Except (ExceptT)

import DomainDrivenDesign.EventSourcing 

rebuildAggregate 
    :: (AggregateMonad st ev err m, Foldable f) 
    => f ev
    -> m ()
rebuildAggregate ev = put $ Versioned version [] aggState
  where
    (version, aggState) = length &&& rebuildState $ ev

pushEvent :: EventSourced st ev => ev -> Versioned st ev -> Versioned st ev
pushEvent ev Versioned{..} = Versioned (version + 1) (ev:pendingEvents) (apply ev aggState)

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

getAggregate' :: (EventSourced st ev, MonadState (Versioned st ev) m) => m st 
getAggregate' = fmap aggState get

raiseEvent' :: (EventSourced st ev, MonadState (Versioned st ev) m) => ev -> m ()
raiseEvent' = modify . pushEvent 

newtype AggregateActionT st ev err m a = AggregateActionT { runAggregateT :: ExceptT err (StateT (Versioned st ev) m) a }
    deriving (Functor, Applicative, Monad, MonadError err, MonadState (Versioned st ev))

runAggregate :: (Monad m, EventSourced st ev) => st -> AggregateActionT st ev err m a -> m (Either err st)
runAggregate startState agg = do
    (e, (Versioned _ _ st)) <- (runStateT . runExceptT . runAggregateT) agg startVersion
    return $ fmap (const st) $ e
  where
    startVersion = Versioned 0 [] startState
