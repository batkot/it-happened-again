{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DomainDrivenDesign.MTL 
    ( AggregateActionT
    
    , runAggregateActionT
    , makeEventProcessor
    ) where

import Control.Arrow ((&&&))

import Control.Monad.State (MonadState, gets, modify, runStateT)
import Control.Monad.Except (MonadError, throwError, runExceptT)

import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Except (ExceptT)

import DomainDrivenDesign.EventSourcing 
import DomainDrivenDesign.Tagless

data Versioned st ev = Versioned 
    { version :: Int -- Nat ?
    , pendingEvents :: [ev]
    , aggState :: st
    }

pushEvent :: EventSourced st ev => ev -> Versioned st ev -> Versioned st ev
pushEvent ev Versioned{..} = Versioned (version + 1) (ev:pendingEvents) (apply ev aggState)

newtype AggregateActionT st ev err m a = AggregateActionT { runAggregateT :: ExceptT err (StateT (Versioned st ev) m) a } 
    deriving (Functor, Applicative, Monad, MonadError err, MonadState (Versioned st ev))

instance (Aggregate st ev err, Monad m) => MonadAggregateAction st ev err (AggregateActionT st ev err m)
  where
    raiseEvent = modify . pushEvent
    raiseError = throwError
    getAggregate = gets aggState

runAggregateActionT
    :: (Monad m, EventSourced st ev, Foldable f) 
    => AggregateActionT st ev err m a 
    -> f ev
    -> m (Either err [ev])
runAggregateActionT agg events = do
    (e, Versioned _ ev _) <- (runStateT . runExceptT . runAggregateT) agg startVersion
    return $ ev <$ e
  where
    (version, recoveredState) = length &&& rebuildState $ events
    startVersion = Versioned version [] recoveredState

makeEventProcessor 
    :: (EventSourced st ev, Monad m)
    => AggregateActionT st ev err m a 
    -> EventProcessor ev m
makeEventProcessor agg = fmap (either (const []) id) . runAggregateActionT agg 
