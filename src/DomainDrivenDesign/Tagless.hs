module DomainDrivenDesign.Tagless 
    ( Aggregate
    , MonadAggregateAction(..)
    )
    where

import DomainDrivenDesign.EventSourcing

-- | Marker Class representing aggregate to enforce explicit Aggregate marking
class EventSourced st ev => Aggregate st ev err | st -> ev, st -> err

-- | Monad representing actions in context of a given aggregate
class (Aggregate st ev err, Monad m) => MonadAggregateAction st ev err m
    | m -> st 
  where
    -- | Raises new aggregate event 
    raiseEvent :: ev -> m ()
    -- | Raises aggregate error. 
    --  Raised error should stop computations
    raiseError :: err -> m ()
    -- | Gets current aggregate state
    getAggregate :: m st
