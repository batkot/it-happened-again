{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module DomainDrivenDesign.MTL 
    ( Restoring
    , EventSourced
    , rebuildState
    
    , raiseError
    , raiseEvent
    , getAggregate
    , runAggregate
    ) where

import Control.Monad.State (MonadState, get, modify)
import Control.Monad.Except (MonadError, throwError, catchError)

newtype Restoring s = Restoring { unRestoring :: s } 
    deriving Functor

class EventSourced st ev | st -> ev where
    initState :: st
    apply :: ev -> Restoring st -> Restoring st

rebuildState :: (EventSourced st ev, Foldable f) => f ev -> st
rebuildState = applyEvents initState

applyEvents :: (EventSourced st ev, Foldable f) => st -> f ev -> st
applyEvents startState = unRestoring . foldr apply (Restoring startState)

data Versioned st ev = Versioned 
    { version :: Int -- Nat ?
    , pendingEvents :: [ev]
    , aggState :: st
    }

pushEvent :: EventSourced st ev => ev -> Versioned st ev -> Versioned st ev
pushEvent ev Versioned{..} = Versioned (version + 1) (ev:pendingEvents) (applyEvents aggState [ev])

getAggregate :: (EventSourced st ev, MonadState (Versioned st ev) m) => m st 
getAggregate = fmap aggState get

-- Not cool
raiseError :: (EventSourced st ev, MonadError err m) => err -> m ()
raiseError = throwError

raiseEvent :: (EventSourced st ev, MonadState (Versioned st ev) m) => ev -> m ()
raiseEvent = modify . pushEvent 

runAggregate :: (EventSourced st ev, MonadState (Versioned st ev) m, MonadError err m) 
             => m a
             -> m (Either err st)
runAggregate agg = 
    catchError result $ \e -> return (Left e)
  where
    result = fmap Right $ agg >>= const getAggregate 
