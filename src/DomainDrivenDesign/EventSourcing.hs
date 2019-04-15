module DomainDrivenDesign.EventSourcing 
    ( EventSourced(..)
    , EventStream
    , rebuildState

    , runEventProcessor
    , EventProcessor
    , Retrieve
    , Store
    ) where

-- | Class representing projection @st@ fueled by @ev@ events 
class EventSourced st ev | st -> ev where
    -- | Initial state of projection
    initState :: st
    -- | Projection function
    apply :: ev -> st -> st

rebuildState :: (EventSourced st ev, Foldable f) => f ev -> st
rebuildState = foldr apply initState

type EventStream a = [a]

type EventProcessor ev m = EventStream ev -> m (EventStream ev)
type Retrieve id a m = id -> m a
type Store id a m = id -> a -> m ()

runEventProcessor 
    :: Monad m 
    => Retrieve Int (EventStream ev) m 
    -> Store Int (EventStream ev) m 
    -> Int 
    -> EventProcessor ev m
    -> m ()
runEventProcessor fetchStream appendToStream streamId action = 
    fetchStream streamId >>= action >>= appendToStream streamId
