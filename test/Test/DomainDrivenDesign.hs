module Test.DomainDrivenDesign
    ( given
    , expectSingleEvent
    , expectFailure
    , aggregateStateTests
    ) where

import DomainDrivenDesign

import Test.Tasty (testGroup, TestTree)

expectFailure :: (EventSourced st cmd ev err, Eq ev, Eq err)
              => st
              -> cmd
              -> err
              -> Bool
expectFailure st cmd err = execute st cmd == failure err

expectSingleEvent :: (EventSourced st cmd ev err, Eq ev, Eq err)
            => st
            -> cmd
            -> ev
            -> Bool
expectSingleEvent st cmd ev = execute st cmd == singleEvent ev

given :: (EventSourced st cmd ev err) => [ev] -> st
given = rebuildAggregate

aggregateStateTests
    :: ( EventSourced st cmd ev err , Eq ev , Eq err)
    => String 
    -> [ev]
    -> [st -> TestTree]
    -> TestTree
aggregateStateTests desc events tests = 
    testGroup desc $ fmap (\f -> f aggregateState)  tests
  where
    aggregateState = given events

