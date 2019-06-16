{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module DomainDrivenDesign.Tagless 
    ( Aggregate
    , MonadAggregateAction(..)
    , AggregateActionT(..)
    , runAggregateActionT
    )
    where

import DomainDrivenDesign.EventSourcing

import Data.Foldable (toList)

import Control.Monad.State (modify, gets, MonadState)

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.Trans.Except (ExceptT, runExceptT)

-- | Marker Class representing aggregate to enforce explicit Aggregate marking
class EventSourced st ev => Aggregate st ev err | st -> ev, st -> err

class (Aggregate st ev err, Monad m) => MonadAggregateAction st ev err m
    | m -> st 
  where
    raiseEvent :: ev -> m ()
    raiseError :: err -> m ()
    getAggregate :: m st

newtype AggregateActionT st ev err m a = AggregateActionT { runAggregateT :: ExceptT err (StateT [ev] m) a } 
    deriving (Functor, Applicative, Monad, MonadState [ev], MonadError err)

instance (Aggregate st ev err, Monad m) => MonadAggregateAction st ev err (AggregateActionT st ev err m)
  where
    raiseEvent = modify . (:) 
    raiseError = throwError
    getAggregate = gets rebuildState

runAggregateActionT
    :: (Monad m, Foldable f, EventSourced st ev) 
    => AggregateActionT st ev err m a
    -> f ev
    -> m (Either err [ev])
runAggregateActionT agg events = do
    (e, x) <- (runStateT . runExceptT . runAggregateT) agg $ toList events
    return $ x <$ e
