module DomainDrivenDesign.EventSourcing 
    ( EventSourced(..)
    , rebuildState
    ) where

class EventSourced st ev | st -> ev where
    initState :: st
    apply :: ev -> st -> st

rebuildState :: (EventSourced st ev, Foldable f) => f ev -> st
rebuildState = foldr apply initState
