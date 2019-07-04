module Test.DomainDrivenDesign.MTL 
    ( TestAggregate 
    , given
    , when
    , expect
    ) where

import Control.Monad.Identity (Identity, runIdentity)

import DomainDrivenDesign.EventSourcing
import DomainDrivenDesign.MTL

type TestAggregate st ev err = AggregateActionT st ev err Identity 

type Act st ev err a = TestAggregate st ev err a -> Either err [ev]

given :: (EventSourced st ev, Foldable f) => f ev -> Act st ev err a
given events = runIdentity . flip runAggregateActionT events

when :: (EventSourced st ev, Eq ev, Eq err) => Act st ev err a -> TestAggregate st ev err a -> Either err [ev]
when giv act = giv act 

expect :: Eq a => a -> a -> Bool
expect = (==)
