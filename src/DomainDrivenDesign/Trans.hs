{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module DomainDrivenDesign.Trans
    ( EventSourced
    , AggregateActionMonad(..)

    , rebuildState
    , getAggregate
    , raiseError
    , raiseEvent
    , runAggregate
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Trans.State (State, get, modify, runState)

import DomainDrivenDesign.EventSourcing

data Versioned st ev = Versioned 
    { version :: Int -- Nat ?
    , pendingEvents :: [ev]
    , aggState :: st
    }

pushEvent :: EventSourced st ev => ev -> Versioned st ev -> Versioned st ev
pushEvent ev Versioned{..} = Versioned (version + 1) (ev:pendingEvents) (apply ev aggState)

type AggregateState st ev = State (Versioned st ev)
newtype AggregateActionMonad st ev err a = AggregateActionMonad { unMonad :: ExceptT err (AggregateState st ev) a }
    deriving (Functor, Applicative, Monad)

getAggregate :: EventSourced st ev => AggregateActionMonad st ev err st
getAggregate = AggregateActionMonad $ fmap aggState $ lift get

raiseError :: EventSourced st ev => err -> AggregateActionMonad st ev err ()
raiseError = AggregateActionMonad . throwE

raiseEvent :: EventSourced st ev => ev -> AggregateActionMonad st ev err ()
raiseEvent ev = AggregateActionMonad $ lift $ modify $ pushEvent ev 

runAggregate :: (EventSourced st ev) => st -> AggregateActionMonad st ev err a -> Either err st
runAggregate startState agg = fmap (const st) $ e
  where
    startVersion = Versioned 0 [] startState
    (e, (Versioned _ _ st)) = (runState . runExceptT . unMonad) agg startVersion
