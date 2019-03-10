module DomainDrivenDesign
    ( EventSourced(..)
    , rebuildAggregate
    ) where

class EventSourced st cmd ev err
    | st -> cmd
    , st -> ev 
    , st -> err
  where
    initState :: st
    execute :: st -> cmd -> Either err [ev]
    apply :: ev -> st -> st

rebuildAggregate :: (EventSourced s c e er, Foldable f) => f e -> s
rebuildAggregate = foldr apply initState
