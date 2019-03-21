module DomainDrivenDesign.Silly
    ( EventSourced(..)
    , Restoring
    , rebuildAggregate
    , executeWithState
    , singleEvent
    , failure
    ) where

newtype Restoring s = Restoring { getState :: s } 
    deriving (Functor)

class EventSourced st cmd ev err
    | st -> cmd
    , st -> ev 
    , st -> err
  where
    initState :: st
    execute :: st -> cmd -> Either err [ev]
    apply :: ev -> Restoring st -> Restoring st

rebuildAggregate :: (EventSourced s c e er, Foldable f) => f e -> s
rebuildAggregate = applyEvents initState

executeWithState :: (EventSourced st cmd ev err) => st -> cmd -> Either err ([ev], st)
executeWithState st = fmap (\ev -> (ev, applyEvents st ev)) . execute st 

applyEvents :: (EventSourced s c e er, Foldable f) => s -> f e -> s
applyEvents startState = getState . foldr apply (Restoring startState)

singleEvent :: a -> Either b [a]
singleEvent = pure . pure

failure :: err -> Either err [b]
failure = Left
