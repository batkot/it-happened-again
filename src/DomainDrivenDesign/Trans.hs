{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module DomainDrivenDesign.Trans
    ( EventSourced
    , Restoring
    , AggregateActionMonad(..)

    , rebuildState
    , getAggregate
    , raiseError
    , pushEvent
    , runAggregate
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Trans.State (State, get, modify, runState)

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

pushEvent' :: EventSourced st ev => ev -> Versioned st ev -> Versioned st ev
pushEvent' ev Versioned{..} = Versioned (version + 1) (ev:pendingEvents) (applyEvents aggState [ev])

-- Naive hardwired way of doing stuff
-- Hell lot of lifting
type AggregateState st ev = State (Versioned st ev)
newtype AggregateActionMonad st ev err a = AggregateActionMonad { unMonad :: ExceptT err (AggregateState st ev) a }
    deriving (Functor, Applicative, Monad)

getAggregate :: EventSourced st ev => AggregateActionMonad st ev err st
getAggregate = AggregateActionMonad $ fmap aggState $ lift get

raiseError :: EventSourced st ev => err -> AggregateActionMonad st ev err ()
raiseError = AggregateActionMonad . throwE

pushEvent :: EventSourced st ev => ev -> AggregateActionMonad st ev err ()
pushEvent ev = AggregateActionMonad $ lift $ modify $ pushEvent' ev 

runAggregate :: (EventSourced st ev) => st -> AggregateActionMonad st ev err a -> Either err st
runAggregate startState agg = fmap (const st) $ e
  where
    startVersion = Versioned 0 [] startState
    (e, (Versioned _ _ st)) = (runState . runExceptT . unMonad) agg startVersion

instance EventSourced Integer Integer where
    initState = 0
    apply e = fmap (+e)
