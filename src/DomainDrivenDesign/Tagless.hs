module DomainDrivenDesign.Tagless 
    ( Aggregate
    , MonadAggregateAction(..)
    )
    where

import DomainDrivenDesign.EventSourcing

-- | Marker Class representing aggregate to enforce explicit Aggregate marking
class EventSourced st ev => Aggregate st ev err | st -> ev, st -> err

class (Aggregate st ev err, Monad m) => MonadAggregateAction st ev err m
    | m -> st 
  where
    raiseEvent :: ev -> m ()
    raiseError :: err -> m ()
    getAggregate :: m st
